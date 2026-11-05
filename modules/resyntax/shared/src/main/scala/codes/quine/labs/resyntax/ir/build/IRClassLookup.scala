package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.charset.CaseMapping
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.CompatibilityPropertyTable
import codes.quine.labs.resyntax.charset.PropertyTable
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.DotNetCharClass
import codes.quine.labs.resyntax.ir.build.IRPropertyLookup.ascii
import codes.quine.labs.resyntax.ir.build.IRPropertyLookup.binary
import codes.quine.labs.resyntax.ir.build.IRPropertyLookup.gc
import codes.quine.labs.resyntax.ir.build.IRPropertyLookup.of

/** IRClassLookup is a dialect's table that `IRCharResolver` looks up by the name of an escape class (e.g. `\w`) or a
  * POSIX class (e.g. `[:alpha:]`), giving its code points.
  *
  * The sets are of code points; the caller restricts them to the unit mode.
  */
private[build] sealed abstract class IRClassLookup:

  import IRClassLookup.*

  /** Returns the characters of `\d` under the flags. */
  def digit(flags: IRCharFlagSet): CharSet

  /** Returns the characters of `\s` under the flags. */
  def space(flags: IRCharFlagSet): CharSet

  /** Returns the characters of `\w` under the flags. */
  def word(flags: IRCharFlagSet): CharSet

  /** Returns the word characters of `\b` and `\B` under the flags. */
  def boundaryWord(flags: IRCharFlagSet): CharSet = word(flags)

  /** Returns the characters of `\h` as a horizontal space (Java, PCRE and Perl). */
  def horizontal: CharSet = pcreHorizontal

  /** Returns the characters of `\v` as a vertical space (Java, PCRE and Perl). */
  def vertical: CharSet = Vertical

  /** Returns the characters of `\h` as a hexadecimal digit (Ruby). */
  def hexDigit: CharSet = ascii('0' -> '9', 'A' -> 'F', 'a' -> 'f')

  /** Returns the characters of `[[:name:]]` under the flags, or `None` for a name that the dialect does not know. */
  // The parsers of the dialects without POSIX classes reject them.
  // $COVERAGE-OFF$
  def posix(name: String, flags: IRCharFlagSet): Option[CharSet] =
    throw new IllegalArgumentException(s"No POSIX class in $this")
  // $COVERAGE-ON$

object IRClassLookup:

  /** Returns the table of the kind in the unit mode, where `perlUnicodeRules` makes Perl's `/d` use Unicode rules. */
  private[build] def from(kind: IRClassLookupKind, mode: UnitMode, perlUnicodeRules: Boolean): IRClassLookup =
    kind match
      case IRClassLookupKind.JavaScript => JavaScript(mode)
      case IRClassLookupKind.Java       => Java
      case IRClassLookupKind.DotNet     => DotNet
      case IRClassLookupKind.PCRE       => PCRE
      case IRClassLookupKind.Perl       => Perl(native = !perlUnicodeRules)
      case IRClassLookupKind.Python     => Python(mode)
      case IRClassLookupKind.Ruby       => Ruby(mode)

  private enum Rule:

    case ASCII, Unicode, Native

  private def pick(rule: Rule, ascii: => CharSet, unicode: => CharSet): CharSet = rule match
    case Rule.ASCII   => ascii
    case Rule.Unicode => unicode
    case Rule.Native  => unicode.diff(Latin1)

  /** JavaScript is the table of JavaScript, whose classes are of ASCII. */
  private[build] final case class JavaScript(mode: UnitMode) extends IRClassLookup:

    def digit(flags: IRCharFlagSet): CharSet = ASCIIDigit

    def space(flags: IRCharFlagSet): CharSet = javaScriptSpace

    def word(flags: IRCharFlagSet): CharSet =
      // With `u` or `v` and `i`, ECMAScript's WordCharacters includes the characters folded to ASCII ones.
      if mode.isCodePoint && flags.ignoreCase then ASCIIWord.union(CaseMapping.SimpleFold.preimage(ASCIIWord))
      else ASCIIWord

  /** Java is the table of Java, whose classes are of Unicode with `U`. */
  private[build] case object Java extends IRClassLookup:

    private def ruleOf(flags: IRCharFlagSet): Rule = if flags.unicodeClass then Rule.Unicode else Rule.ASCII

    def digit(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIIDigit, gc("Nd"))

    def space(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIISpace, CompatibilityPropertyTable.Space)

    def word(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIIWord, CompatibilityPropertyTable.Word)

  /** DotNet is the table of .NET, whose classes are of Unicode. */
  private[build] case object DotNet extends IRClassLookup:

    def digit(flags: IRCharFlagSet): CharSet = gc("Nd")

    def space(flags: IRCharFlagSet): CharSet = gc("Z").union(of("\f\n\r\t\u000b\u0085"))

    def word(flags: IRCharFlagSet): CharSet = DotNetCharClass.Word

    override def boundaryWord(flags: IRCharFlagSet): CharSet = DotNetCharClass.BoundaryWord

  /** PCRE is the table of PCRE, whose classes are of Unicode in UCP mode, except the ones that `a` restricts. */
  private[build] case object PCRE extends IRClassLookup:

    private def ruleOf(flags: IRCharFlagSet, asciiOf: IRCharset.ASCII => Boolean): Rule =
      val restricted = flags.charset match
        case a: IRCharset.ASCII => asciiOf(a)
        case _                  => false
      if flags.unicodeClass && !restricted then Rule.Unicode else Rule.ASCII

    def digit(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags, _.digit), ASCIIDigit, gc("Nd"))

    def space(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags, _.space), ASCIISpace, pcreHorizontal.union(Vertical))

    def word(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags, _.word), ASCIIWord, pcreWord)

    override def posix(name: String, flags: IRCharFlagSet): Option[CharSet] =
      val rule = ruleOf(flags, a => if name == "digit" || name == "xdigit" then a.posixDigit else a.posix)
      ASCII_POSIX
        .get(name)
        .map: a =>
          val set = pick(rule, a, pcrePOSIX(name).getOrElse(a))
          // PCRE matches both cases with `[:upper:]` and `[:lower:]` under case-insensitivity in ASCII (not in UCP mode).
          if flags.ignoreCase && (name == "upper" || name == "lower") && rule == Rule.ASCII then
            set.union(ASCII_POSIX(if name == "upper" then "lower" else "upper"))
          else set

    private def pcrePOSIX(name: String): Option[CharSet] =
      // pcre2pattern: "[:graph:] matches all characters with the L, M, N, P, S, or Cf properties, except for U+061C, U+180E and U+2066 - U+2069".
      val graph = gc("L", "M", "N", "P", "S", "Cf").diff(of("\u061C\u180E\u2066\u2067\u2068\u2069"))
      name match
        case "alnum" => Some(gc("L", "N"))
        case "alpha" => Some(gc("L"))
        case "blank" => Some(pcreHorizontal)
        case "cntrl" => Some(gc("Cc"))
        case "digit" => Some(gc("Nd"))
        case "graph" => Some(graph)
        case "lower" => Some(gc("Ll"))
        // PCRE2's `[:print:]` keeps U+180E, which `[:graph:]` excludes.
        case "print"  => Some(graph.union(gc("Zs")).union(of("\u180e")).diff(gc("Cc")))
        case "punct"  => Some(gc("P").union(gc("S").intersect(CharSet.range(0, 0x7f))))
        case "space"  => Some(pcreHorizontal.union(Vertical))
        case "upper"  => Some(gc("Lu"))
        case "word"   => Some(pcreWord)
        case "xdigit" => Some(binary("Hex_Digit"))
        case _        => None

  /** Perl is the table of Perl, whose classes are of Unicode or of the rules of the character set.
    *
    * `native` is Perl's `/d` rules without a reason to use Unicode rules, where `U+0080..U+00FF` follow ASCII rules.
    */
  private[build] final case class Perl(native: Boolean) extends IRClassLookup:

    // Perl reads the bytes of a byte string as Latin-1 characters.
    private def ruleOf(flags: IRCharFlagSet): Rule = flags.charset match
      case _: IRCharset.ASCII => Rule.ASCII
      case IRCharset.Unicode  => Rule.Unicode
      // `/l` takes the C locale, which has no non-ASCII character in a class, even in a UTF-8 pattern.
      case IRCharset.Locale => Rule.Native
      case _                => if native then Rule.Native else Rule.Unicode

    def digit(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIIDigit, gc("Nd"))

    def space(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIISpace, binary("White_Space"))

    def word(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIIWord, CompatibilityPropertyTable.Word)

    override def horizontal: CharSet = pcreHorizontal.diff(of("\u180E"))

    override def posix(name: String, flags: IRCharFlagSet): Option[CharSet] =
      val asciiSet = ASCII_POSIX.get(name)
      // Perl matches the cased characters with `[:upper:]` and `[:lower:]` under case-insensitivity.
      if flags.ignoreCase && (name == "upper" || name == "lower") then
        Some(pick(ruleOf(flags), ascii('A' -> 'Z', 'a' -> 'z'), binary("Cased")))
      else if name == "ascii" then asciiSet
      else
        asciiSet.map: a =>
          val PropertyResult.Found(set, _) = IRPropertyLookup.perl("XPosix" + name, None): @unchecked
          pick(ruleOf(flags), a, set.chars)

  /** Python is the table of Python, whose classes are of Unicode in a `str` pattern without `a`. */
  private[build] final case class Python(mode: UnitMode) extends IRClassLookup:

    private def ruleOf(flags: IRCharFlagSet): Rule = flags.charset match
      case _ if mode == UnitMode.Byte => Rule.ASCII
      case _: IRCharset.ASCII         => Rule.ASCII
      case _                          => Rule.Unicode

    def digit(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIIDigit, gc("Nd"))

    def space(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIISpace, pythonSpace)

    def word(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIIWord, pythonWord)

  /** Ruby is the table of Ruby, whose escape classes are of Unicode with `u`, and POSIX classes without `a`. */
  private[build] final case class Ruby(mode: UnitMode) extends IRClassLookup:

    private def ruleOf(flags: IRCharFlagSet): Rule = flags.charset match
      case _ if mode == UnitMode.Byte => Rule.ASCII
      case IRCharset.Unicode          => Rule.Unicode
      case _                          => Rule.ASCII

    def digit(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIIDigit, gc("Nd"))

    def space(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIISpace, binary("White_Space"))

    def word(flags: IRCharFlagSet): CharSet = pick(ruleOf(flags), ASCIIWord, rubyWord)

    // Onigmo's `(?d)` uses Unicode for `\b` and `\B`, unlike `\w`.
    override def boundaryWord(flags: IRCharFlagSet): CharSet =
      if mode.isCodePoint && (flags.charset == IRCharset.Default || flags.charset == IRCharset.Locale) then rubyWord
      else word(flags)

    override def posix(name: String, flags: IRCharFlagSet): Option[CharSet] =
      val asciiSet = ASCII_POSIX.get(name)
      val rule =
        if mode == UnitMode.Byte then Rule.ASCII
        else
          flags.charset match
            case _: IRCharset.ASCII => Rule.ASCII
            case _                  => Rule.Unicode
      IRPropertyLookup.rubyPOSIX(name).map(u => pick(rule, asciiSet.get, u))

  /** The marks of Java's `\b` without `U`, where a non-spacing mark is a word character after a letter or a digit. */
  lazy val javaWordMarks: IRWordMarkSet =
    // `Pattern.hasBaseCharacter` steps back by UTF-16 code units, so the characters before a mark are of the BMP.
    val bmp = CharSet.range(0, 0xffff)
    IRWordMarkSet(gc("Mn"), gc("Mn").intersect(bmp), gc("L", "Nd").intersect(bmp))

  val Latin1: CharSet = CharSet.range(0x80, 0xff)

  val ASCIIDigit: CharSet = ascii('0' -> '9')

  val ASCIIWord: CharSet = ascii('0' -> '9', 'A' -> 'Z', '_' -> '_', 'a' -> 'z')

  val ASCIISpace: CharSet = of(" \t\n\u000b\f\r")

  val Vertical: CharSet = CharSet.range(0x0a, 0x0d).union(of("\u0085\u2028\u2029"))

  /** pcreHorizontal is the characters of `\h` in Java and PCRE. */
  lazy val pcreHorizontal: CharSet =
    of("\t \u00A0\u1680\u180E\u202F\u205F\u3000").union(CharSet.range(0x2000, 0x200a))

  /** javaScriptSpace is the characters of JavaScript's `\s`, which are ECMAScript's WhiteSpace and LineTerminator. */
  lazy val javaScriptSpace: CharSet = gc("Zs").union(of("\t\n\u000b\f\r\u2028\u2029\uFEFF"))

  /** pythonSpace is the characters of Python's `\s`, which are `str.isspace`: `Zs`, or the bidirectional class `WS`,
    * `B` or `S`.
    */
  lazy val pythonSpace: CharSet =
    val bc = PropertyTable.BidiClass
    gc("Zs").union(CharSet.union(Seq("WS", "B", "S").map(bc.value(_).get.set)))

  /** pythonWord is the characters of Python's `\w`, which are `str.isalnum` (a letter or a character with a numeric
    * type) or `_`.
    */
  lazy val pythonWord: CharSet =
    val numeric = Seq("Decimal", "Digit", "Numeric").map(PropertyTable.NumericType.value(_).get.set)
    gc("L").union(CharSet.union(numeric)).union(of("_"))

  /** pcreWord is the characters of PCRE2's `\w` in UCP mode (with `Mn` and `Pc` since 10.43). */
  lazy val pcreWord: CharSet = gc("L", "N", "Mn", "Pc").union(of("_"))

  /** rubyWord is the characters of Onigmo's `\w` in Unicode: `[[:word:]]` and the `No` characters of Latin-1 (its
    * Latin-1 ctype table marks them).
    */
  lazy val rubyWord: CharSet = IRPropertyLookup.rubyPOSIX("word").get.union(of("\u00b2\u00b3\u00b9\u00bc\u00bd\u00be"))

  /** ASCII_POSIX is the characters of each POSIX class in ASCII, by its name (e.g. `alpha`). */
  val ASCII_POSIX: Map[String, CharSet] = Map(
    "alnum" -> ascii('0' -> '9', 'A' -> 'Z', 'a' -> 'z'),
    "alpha" -> ascii('A' -> 'Z', 'a' -> 'z'),
    "ascii" -> CharSet.range(0, 0x7f),
    "blank" -> of(" \t"),
    "cntrl" -> CharSet.range(0, 0x1f).union(CharSet.single(0x7f)),
    "digit" -> ASCIIDigit,
    "graph" -> CharSet.range(0x21, 0x7e),
    "lower" -> ascii('a' -> 'z'),
    "print" -> CharSet.range(0x20, 0x7e),
    "punct" -> ascii('!' -> '/', ':' -> '@', '[' -> '`', '{' -> '~'),
    "space" -> ASCIISpace,
    "upper" -> ascii('A' -> 'Z'),
    "word" -> ASCIIWord,
    "xdigit" -> ascii('0' -> '9', 'A' -> 'F', 'a' -> 'f')
  )
