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

  /** Returns the marks that `\b` and `\B` take as word characters after a base under the flags, if any. */
  def wordMarks(flags: IRCharFlagSet): Option[IRWordMarkSet] = None

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

  /** Returns the characters of `\h` as a blank (python-regex) under the flags. */
  // The parsers of the other dialects read `\h` otherwise.
  // $COVERAGE-OFF$
  def blank(flags: IRCharFlagSet): CharSet =
    throw new IllegalArgumentException(s"No blank class in $this")
  // $COVERAGE-ON$

object IRClassLookup:

  /** Returns the table of the kind in the unit mode, where `perlUnicodeRules` makes Perl's `/d` use Unicode rules. */
  private[build] def from(kind: IRClassLookupKind, mode: UnitMode, perlUnicodeRules: Boolean): IRClassLookup =
    kind match
      case IRClassLookupKind.JavaScript  => JavaScript(mode)
      case IRClassLookupKind.Java        => Java
      case IRClassLookupKind.DotNet      => DotNet
      case IRClassLookupKind.PCRE        => PCRE
      case IRClassLookupKind.Perl        => Perl(native = !perlUnicodeRules)
      case IRClassLookupKind.Python      => Python(mode)
      case IRClassLookupKind.PythonRegex => PythonRegex(mode)
      case IRClassLookupKind.Ruby        => Ruby(mode)
      case IRClassLookupKind.ICU         => ICU
      case IRClassLookupKind.BoostRegex  => BoostRegex(mode)

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

    // Java's `\b` with `U` has the non-spacing marks in its word set already.
    override def wordMarks(flags: IRCharFlagSet): Option[IRWordMarkSet] =
      Option.when(!flags.unicodeClass)(javaWordMarks)

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

  /** PythonRegex is the table of python-regex, whose classes are its properties (e.g. `\w` is `\p{Word}`) under the
    * encoding of the flags.
    */
  private[build] final case class PythonRegex(mode: UnitMode) extends IRClassLookup:

    private def property(name: String, flags: IRCharFlagSet): CharSet =
      val PropertyResult.Found(set, _) = IRPropertyLookup.pythonRegex(None, name, false, flags, mode): @unchecked
      set.chars

    def digit(flags: IRCharFlagSet): CharSet = property("Digit", flags)

    def space(flags: IRCharFlagSet): CharSet = property("Space", flags)

    def word(flags: IRCharFlagSet): CharSet = property("Word", flags)

    override def blank(flags: IRCharFlagSet): CharSet = property("Blank", flags)

    // The parser keeps the value of `[[:name=value:]]` after `=`.
    override def posix(name: String, flags: IRCharFlagSet): Option[CharSet] =
      val result = name.indexOf('=') match
        case -1 => IRPropertyLookup.pythonRegex(None, name, true, flags, mode)
        case i  => IRPropertyLookup.pythonRegex(Some(name.take(i)), name.drop(i + 1), true, flags, mode)
      result match
        case PropertyResult.Found(set, _) => Some(set.chars)
        case _                            => None

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

  /** ICU is the table of ICU, whose classes are of Unicode.
    *
    * Its `\b` takes a character of `Grapheme_Extend` or `Cf` as the word character before it, if any, as
    * `isWordBoundary` of `rematch.cpp` skips them.
    */
  private[build] case object ICU extends IRClassLookup:

    def digit(flags: IRCharFlagSet): CharSet = gc("Nd")

    def space(flags: IRCharFlagSet): CharSet = CompatibilityPropertyTable.Space

    def word(flags: IRCharFlagSet): CharSet = CompatibilityPropertyTable.Word

    override def boundaryWord(flags: IRCharFlagSet): CharSet = CompatibilityPropertyTable.Word.diff(icuWordExtenders)

    override def wordMarks(flags: IRCharFlagSet): Option[IRWordMarkSet] = Some(icuWordMarks)

    override def horizontal: CharSet = gc("Zs").union(of("\t"))

  /** BoostRegex is the table of Boost.Regex, whose escape classes are its class names (e.g. `\w` is `[[:w:]]`). */
  private[build] final case class BoostRegex(mode: UnitMode) extends IRClassLookup:

    private def lookup(name: String, ignoreCase: Boolean): CharSet =
      val PropertyResult.Found(set, _) = IRPropertyLookup.boostRegex(name, ignoreCase, mode): @unchecked
      set.chars

    def digit(flags: IRCharFlagSet): CharSet = lookup("d", flags.ignoreCase)

    def space(flags: IRCharFlagSet): CharSet = lookup("s", flags.ignoreCase)

    def word(flags: IRCharFlagSet): CharSet = lookup("w", flags.ignoreCase)

    override def horizontal: CharSet = lookup("h", ignoreCase = false)

    override def vertical: CharSet = lookup("v", ignoreCase = false)

  /** The combining characters of Boost.Regex's `\X` in `u32regex`, which are the fixed table of
    * `is_combining_implementation` of `regex_traits_defaults.hpp`.
    */
  lazy val boostRegexCombining: CharSet =
    val ranges = Seq(
      0x0300 -> 0x0361,
      0x0483 -> 0x0486,
      0x0903 -> 0x0903,
      0x093e -> 0x0940,
      0x0949 -> 0x094c,
      0x0982 -> 0x0983,
      0x09be -> 0x09c0,
      0x09c7 -> 0x09cc,
      0x09d7 -> 0x09d7,
      0x0a3e -> 0x0a40,
      0x0a83 -> 0x0a83,
      0x0abe -> 0x0ac0,
      0x0ac9 -> 0x0acc,
      0x0b02 -> 0x0b03,
      0x0b3e -> 0x0b3e,
      0x0b40 -> 0x0b40,
      0x0b47 -> 0x0b4c,
      0x0b57 -> 0x0b57,
      0x0b83 -> 0x0b83,
      0x0bbe -> 0x0bbf,
      0x0bc1 -> 0x0bcc,
      0x0bd7 -> 0x0bd7,
      0x0c01 -> 0x0c03,
      0x0c41 -> 0x0c44,
      0x0c82 -> 0x0c83,
      0x0cbe -> 0x0cbe,
      0x0cc0 -> 0x0cc4,
      0x0cc7 -> 0x0ccb,
      0x0cd5 -> 0x0cd6,
      0x0d02 -> 0x0d03,
      0x0d3e -> 0x0d40,
      0x0d46 -> 0x0d4c,
      0x0d57 -> 0x0d57,
      0x0f7f -> 0x0f7f,
      0x20d0 -> 0x20e1,
      0x3099 -> 0x309a,
      0xfe20 -> 0xfe23
    )
    CharSet.union(ranges.map((start, end) => CharSet.range(start, end)))

  /** The characters that ICU's `\b` skips: `Grapheme_Extend` and `Cf`. */
  private lazy val icuWordExtenders: CharSet = binary("Grapheme_Extend").union(gc("Cf"))

  /** The marks of ICU's `\b`, where a character that it skips is a word character after a word character. */
  lazy val icuWordMarks: IRWordMarkSet =
    IRWordMarkSet(icuWordExtenders, icuWordExtenders, CompatibilityPropertyTable.Word.diff(icuWordExtenders))

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
