package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.CaseMapping
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.Properties
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.build.IRProperties.ascii
import codes.quine.labs.resyntax.ir.build.IRProperties.binary
import codes.quine.labs.resyntax.ir.build.IRProperties.gc
import codes.quine.labs.resyntax.ir.build.IRProperties.of

/** IRCharClasses gives the code points of escape classes (e.g. `\w`) and POSIX classes in a dialect.
  *
  * The sets are of code points; the caller restricts them to the unit mode. `perlNative` is Perl's `/d` rules without a
  * reason to use Unicode rules, where `U+0080..U+00FF` follow ASCII rules.
  */
private[build] final class IRCharClasses(dialect: Dialect, mode: UnitMode, perlNative: Boolean):
  import IRCharClasses.*

  private def ruleOf(flags: IRCharFlags, asciiOf: IRCharset.ASCII => Boolean): Rule =
    val unicode = mode != UnitMode.Byte || flags.unicodeClass
    dialect match
      case Dialect.JavaScript => Rule.ASCII
      case Dialect.Java       => if flags.unicodeClass then Rule.Unicode else Rule.ASCII
      case Dialect.DotNet     => Rule.Unicode
      case _: Dialect.Python  =>
        flags.charset match
          case _ if mode == UnitMode.Byte => Rule.ASCII
          case _: IRCharset.ASCII         => Rule.ASCII
          case _                          => Rule.Unicode
      case _: Dialect.Ruby =>
        flags.charset match
          case _ if mode == UnitMode.Byte => Rule.ASCII
          case IRCharset.Unicode          => Rule.Unicode
          case _                          => Rule.ASCII
      // Perl reads the bytes of a byte string as Latin-1 characters.
      case _: Dialect.Perl =>
        flags.charset match
          case _: IRCharset.ASCII => Rule.ASCII
          case IRCharset.Unicode  => Rule.Unicode
          // `/l` takes the C locale, which has no non-ASCII character in a class, even in a UTF-8 pattern.
          case IRCharset.Locale => Rule.Native
          case _                => if perlNative then Rule.Native else Rule.Unicode
      case Dialect.PCRE =>
        val restricted = flags.charset match
          case a: IRCharset.ASCII => asciiOf(a)
          case _                  => false
        if flags.unicodeClass && unicode && !restricted then Rule.Unicode else Rule.ASCII

  private def pick(rule: Rule, ascii: => CharSet, unicode: => CharSet): CharSet = rule match
    case Rule.ASCII   => ascii
    case Rule.Unicode => unicode
    case Rule.Native  => unicode.diff(Latin1)

  /** `\d`. */
  def digit(flags: IRCharFlags): CharSet =
    pick(ruleOf(flags, _.digit), ASCIIDigit, if dialect.isJavaScript then ASCIIDigit else gc("Nd"))

  /** `\s`. */
  def space(flags: IRCharFlags): CharSet = dialect match
    case Dialect.JavaScript => javaScriptSpace
    case Dialect.DotNet     => gc("Z").union(of("\f\n\r\t\u000b\u0085"))
    case _                  =>
      val unicode = dialect match
        case Dialect.Java      => gc("Z").union(CharSet.range(0x09, 0x0d)).union(CharSet.single(0x85))
        case _: Dialect.Python => pythonSpace
        case Dialect.PCRE      => pcreHorizontal.union(Vertical)
        case _                 => binary("White_Space")
      pick(ruleOf(flags, _.space), ASCIISpace, unicode)

  /** `\w`. */
  def word(flags: IRCharFlags): CharSet = dialect match
    case Dialect.JavaScript =>
      // With `u` or `v` and `i`, ECMAScript's WordCharacters includes the characters folded to ASCII ones.
      if mode == UnitMode.CodePoint && flags.ignoreCase then ASCIIWord.union(CaseMapping.SimpleFold.preimage(ASCIIWord))
      else ASCIIWord
    case Dialect.DotNet => dotNetWord
    case _              =>
      val unicode = dialect match
        case Dialect.Java      => IRProperties.javaWord
        case _: Dialect.Python => pythonWord
        case Dialect.PCRE      => pcreWord
        case _: Dialect.Ruby   => rubyWord
        case _                 => perlWord
      pick(ruleOf(flags, _.word), ASCIIWord, unicode)

  /** The word characters of `\b` and `\B`. */
  def boundaryWord(flags: IRCharFlags): CharSet = dialect match
    // .NET's `RegexCharClass.IsBoundaryWordChar` adds ZWNJ and ZWJ.
    case Dialect.DotNet => dotNetWord.union(of("‌‍"))
    // Onigmo's `(?d)` uses Unicode for `\b` and `\B`, unlike `\w`.
    case _: Dialect.Ruby
        if mode == UnitMode.CodePoint && (flags.charset == IRCharset.Default || flags.charset == IRCharset.Locale) =>
      rubyWord
    case _ => word(flags)

  /** The marks of Java's `\b` without `U`, where a non-spacing mark is a word character after a letter or a digit.
    *
    * `Pattern.hasBaseCharacter` steps back by UTF-16 code units, so the characters before a mark are of the BMP.
    */
  def javaWordMarks: IRWordMarks =
    val bmp = CharSet.range(0, 0xffff)
    IRWordMarks(gc("Mn"), gc("Mn").intersect(bmp), gc("L", "Nd").intersect(bmp))

  /** Perl's `[[:upper:]]` and `[[:lower:]]` under case-insensitivity: the cased characters. */
  def perlCased(flags: IRCharFlags): CharSet =
    pick(ruleOf(flags, _.posix), ascii('A' -> 'Z', 'a' -> 'z'), binary("Cased"))

  /** `\h` as a horizontal space (Java, PCRE and Perl). */
  def horizontal: CharSet = dialect match
    case _: Dialect.Perl => pcreHorizontal.diff(of("᠎"))
    case _               => pcreHorizontal

  /** `\v` as a vertical space (Java, PCRE and Perl). */
  def vertical: CharSet = Vertical

  /** `\h` as a hexadecimal digit (Ruby). */
  def hexDigit: CharSet = ascii('0' -> '9', 'A' -> 'F', 'a' -> 'f')

  /** `[[:name:]]`, or `None` for a name that the dialect does not know. */
  def posix(name: String, flags: IRCharFlags): Option[CharSet] =
    val asciiSet = ASCIIPOSIX.get(name)
    dialect match
      case _: Dialect.Ruby =>
        val rule =
          if mode == UnitMode.Byte then Rule.ASCII
          else
            flags.charset match
              case _: IRCharset.ASCII => Rule.ASCII
              case _                  => Rule.Unicode
        IRProperties.rubyPOSIX(name).map(u => pick(rule, asciiSet.get, u))
      case _: Dialect.Perl if name == "ascii" => asciiSet
      case _: Dialect.Perl                    =>
        IRProperties.perl("XPosix" + name, None) match
          case PropertyResult.Found(set, _) => asciiSet.map(a => pick(ruleOf(flags, _.posix), a, set.chars))
          case _                            => None
      case Dialect.PCRE =>
        val rule = ruleOf(flags, a => if name == "digit" || name == "xdigit" then a.posixDigit else a.posix)
        asciiSet.map(a => pick(rule, a, pcrePOSIX(name).getOrElse(a)))
      case _ => asciiSet

  private def pcrePOSIX(name: String): Option[CharSet] =
    // pcre2pattern: "[:graph:] matches all characters with the L, M, N, P, S, or Cf properties, except for U+061C,
    // U+180E and U+2066 - U+2069".
    val graph = gc("L", "M", "N", "P", "S", "Cf").diff(of("؜᠎⁦⁧⁨⁩"))
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

object IRCharClasses:

  private enum Rule:
    case ASCII, Unicode, Native

  val Latin1: CharSet = CharSet.range(0x80, 0xff)

  val ASCIIDigit: CharSet = ascii('0' -> '9')

  val ASCIIWord: CharSet = ascii('0' -> '9', 'A' -> 'Z', '_' -> '_', 'a' -> 'z')

  val ASCIISpace: CharSet = of(" \t\n\u000b\f\r")

  val Vertical: CharSet = CharSet.range(0x0a, 0x0d).union(of("\u0085  "))

  /** `\h` of Java and PCRE. */
  lazy val pcreHorizontal: CharSet =
    of("\t   ᠎  　").union(CharSet.range(0x2000, 0x200a))

  /** ECMAScript's WhiteSpace and LineTerminator. */
  lazy val javaScriptSpace: CharSet = gc("Zs").union(of("\t\n\u000b\f\r  ﻿"))

  /** Python's `str.isspace`: `Zs`, or the bidirectional class `WS`, `B` or `S`. */
  lazy val pythonSpace: CharSet =
    val bc = Properties.BidiClass
    gc("Zs").union(CharSet.union(Seq("WS", "B", "S").map(bc.value(_).get.set)))

  /** Python's `str.isalnum` or `_`, approximated by letters and numbers. */
  lazy val pythonWord: CharSet = gc("L", "N").union(of("_"))

  /** PCRE2's `\w` in UCP mode (with `Mn` and `Pc` since 10.43). */
  lazy val pcreWord: CharSet = gc("L", "N", "Mn", "Pc").union(of("_"))

  /** Onigmo's `\w` in Unicode: `[[:word:]]` and the `No` characters of Latin-1 (its Latin-1 ctype table marks them). */
  lazy val rubyWord: CharSet = IRProperties.rubyPOSIX("word").get.union(of("\u00b2\u00b3\u00b9\u00bc\u00bd\u00be"))

  /** Perl's `\p{Word}`. */
  lazy val perlWord: CharSet = binary("Alphabetic").union(gc("M", "Nd", "Pc")).union(binary("Join_Control"))

  /** .NET's `\w`: `[\p{L}\p{Mn}\p{Nd}\p{Pc}]`. */
  lazy val dotNetWord: CharSet = gc("L", "Mn", "Nd", "Pc")

  /** The POSIX classes in ASCII. */
  val ASCIIPOSIX: Map[String, CharSet] = Map(
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
