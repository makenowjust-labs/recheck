package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.util.NumberFormat
import codes.quine.labs.recheck.util.RepeatUtil
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.recheck.util.UStringUtil.*
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.Properties
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** AttackPattern is an attack pattern string to ReDoS.
  *
  * It is a string with repetition structure and the number of repeating. It can consider as the general representation
  * of both [[automaton.Witness]] and [[fuzz.FString]].
  *
  *   - `pump` is a sequence of pumps. The 1st element of the item is a prefix string, the 2nd element is pump string,
  *     and the 3rd element is a bias of the number of repeating.
  *   - `suffix` is a suffix string.
  *   - `n` is the base number of repeating. The total repeating number is a sum of bias and base.
  *   - `style` is the notation of `toString`, which is the host language of the pattern's dialect.
  *
  * For example, when the pattern is `AttackPattern(Seq(w, u, n), (v, s, m), t, l)`, it means `w u^(n+l) v s^(m+l) t`.
  *
  * The strings are UTF-16 code units, or bytes if the pattern matches bytes.
  */
final case class AttackPattern(
    pumps: Seq[(UString, UString, Int)],
    suffix: UString,
    n: Int,
    style: AttackPattern.Style = AttackPattern.JavaScript
):

  /** Returns a size number of sum of fixed parts. */
  def fixedSize: Int = pumps.map(_._1.length).sum + suffix.length

  /** Returns a size number of sum of repeat parts. */
  def repeatSize: Int = pumps.map(_._2.length).sum

  /** Adjusts repetition count in this string to the complexity. */
  def adjust(complexity: AttackComplexity.Vulnerable, limit: Int, maxSize: Int): AttackPattern =
    val n = complexity match
      case AttackComplexity.Polynomial(degree, _) =>
        RepeatUtil.polynomial(degree, limit, fixedSize, repeatSize, maxSize)
      case AttackComplexity.Exponential(_) =>
        RepeatUtil.exponential(limit, fixedSize, repeatSize, maxSize)
    copy(n = n)

  /** Returns the `UString` represented by this. */
  def asUString: UString =
    val str = Array.newBuilder[Int]

    for (s, t, m) <- pumps do
      str.addAll(s.iterator)
      for _ <- 1 to (n + m) do str.addAll(t.iterator)
    str.addAll(suffix.iterator)

    UString.from(str.result())

  /** Returns the string representation of this in the style. */
  def toString(style: AttackPattern.Style): String =
    val parts = Seq.newBuilder[String]

    for (s, t, m) <- pumps do
      if s.nonEmpty then parts.addOne(style.literal(s))
      parts.addOne(style.repeat(t, math.max(0, n + m)))
    if suffix.nonEmpty then parts.addOne(style.literal(suffix))

    val result = parts.result()
    if result.isEmpty then style.literal(UString.empty) else result.mkString(style.join)

  override def toString: String = toString(style)

object AttackPattern:

  /** Style is a notation of [[AttackPattern]] and similar classes.
    *
    * Except [[Superscript]], it writes an expression of the language that evaluates to the string. The strings are
    * UTF-16 code units, or bytes in a style for bytes.
    */
  sealed abstract class Style extends Product with Serializable:

    def literal(s: UString): String

    /** Returns an expression repeating the string `n` times. */
    def repeat(s: UString, n: Int): String

    /** Returns a string concatenation operator. */
    def join: String

  object Style:

    /** Returns the style of the host language of the dialect for the unit mode of a pattern. */
    def of(dialect: Dialect, mode: UnitMode): Style =
      val bytes = mode == UnitMode.Byte
      dialect match
        case Dialect.JavaScript => JavaScript
        case _: Dialect.Python  => Python(bytes)
        case _: Dialect.Ruby    => Ruby(bytes)
        case Dialect.Java       => Java
        case Dialect.DotNet     => CSharp
        case _: Dialect.Perl    => Perl
        case Dialect.PCRE       => PHP(bytes)

  /** JavaScript is a JavaScript style specifier, e.g. `'a'.repeat(31) + '\x00'`. */
  case object JavaScript extends Style:
    def literal(s: UString): String = UStringUtil.show(s)
    def repeat(s: UString, n: Int): String = s"${literal(s)}.repeat(${n})"
    def join: String = " + "

  /** Superscript is a superscript style specifier for logs, e.g. `'a'³¹ '\x00'`. */
  case object Superscript extends Style:
    def literal(s: UString): String = UStringUtil.show(s)
    def repeat(s: UString, n: Int): String = s"${literal(s)}${NumberFormat.superscript(n)}"
    def join: String = " "

  /** Python is a Python style specifier, e.g. `'a' * 31 + '\x00'`, or `b'a' * 31 + b'\x00'` for bytes. */
  final case class Python(bytes: Boolean = false) extends Style:
    def literal(s: UString): String = s.chars(true).map(char).mkString(if bytes then "b'" else "'", "", "'")
    def repeat(s: UString, n: Int): String = s"${literal(s)} * $n"
    def join: String = " + "

    private def char(c: Int): String = c match
      case '\\' | '\''               => s"\\${c.toChar}"
      case Named(e)                  => e
      case c if isAsciiPrintable(c)  => c.toChar.toString
      case c if !bytes && isShown(c) => codePoint(c)
      case c if c <= 0xff            => f"\\x$c%02x"
      case c if c <= 0xffff          => f"\\u$c%04x"
      case c                         => f"\\U$c%08x"

  /** Ruby is a Ruby style specifier, e.g. `"a" * 31 + "\x00"`, or `"a".b * 31 + "\x00".b` for bytes. */
  final case class Ruby(bytes: Boolean = false) extends Style:
    def literal(s: UString): String =
      val cs = s.chars(true).toIndexedSeq
      cs.indices
        .map: i =>
          // `#` starts an interpolation before these characters.
          if cs(i) == '#' && i + 1 < cs.size && "{$@".indexOf(cs(i + 1)) >= 0 then "\\#" else char(cs(i))
        .mkString("\"", "", if bytes then "\".b" else "\"")
    def repeat(s: UString, n: Int): String = s"${literal(s)} * $n"
    def join: String = " + "

    private def char(c: Int): String = c match
      case '\\' | '"'               => s"\\${c.toChar}"
      case Named(e)                 => e
      case c if isAsciiPrintable(c) => c.toChar.toString
      case c if bytes || c < 0x80   => f"\\x$c%02X"
      case c if isSurrogate(c)      => UString(c).encode(UnitMode.Byte).map(b => f"\\x$b%02X").mkString
      case c if isShown(c)          => codePoint(c)
      case c if c <= 0xffff         => f"\\u$c%04X"
      case c                        => f"\\u{$c%X}"

  /** Java is a Java style specifier, e.g. `"a".repeat(31) + "\u0000"`. */
  case object Java extends Style:
    def literal(s: UString): String = s.chars(true).map(char).mkString("\"", "", "\"")
    def repeat(s: UString, n: Int): String = s"${literal(s)}.repeat($n)"
    def join: String = " + "

    // Java translates `\uXXXX` before reading a literal, so a line terminator, `"` and `\` need the other escapes.
    private def char(c: Int): String = c match
      case '\\' | '"'               => s"\\${c.toChar}"
      case Named(e)                 => e
      case c if isAsciiPrintable(c) => c.toChar.toString
      case c if isShown(c)          => codePoint(c)
      case c                        => utf16(c)

  /** CSharp is a C# style specifier for .NET, e.g. `string.Concat(Enumerable.Repeat("a", 31)) + "\u0000"`.
    *
    * `Enumerable.Repeat` needs `System.Linq`.
    */
  case object CSharp extends Style:
    def literal(s: UString): String = s.chars(true).map(char).mkString("\"", "", "\"")
    def repeat(s: UString, n: Int): String = s"string.Concat(Enumerable.Repeat(${literal(s)}, $n))"
    def join: String = " + "

    private def char(c: Int): String = c match
      case '\\'                     => "\\\\"
      case '"'                      => "\\\""
      case Named(e)                 => e
      case c if isAsciiPrintable(c) => c.toChar.toString
      case c if isShown(c)          => codePoint(c)
      case c                        => utf16(c)

  /** Perl is a Perl style specifier, e.g. `"a" x 31 . "\x00"`.
    *
    * Its literals are ASCII, so that they do not depend on `use utf8`.
    */
  case object Perl extends Style:
    def literal(s: UString): String = s.chars(true).map(char).mkString("\"", "", "\"")
    def repeat(s: UString, n: Int): String = s"${literal(s)} x $n"
    def join: String = " . "

    private def char(c: Int): String = c match
      case '\\' | '"' | '$' | '@'   => s"\\${c.toChar}"
      case Named(e)                 => e
      case c if isAsciiPrintable(c) => c.toChar.toString
      case c if c <= 0xff           => f"\\x$c%02X"
      case c                        => f"\\x{$c%X}"

  /** PHP is a PHP style specifier for PCRE, e.g. `str_repeat('a', 31) . "\x00"`.
    *
    * A literal of printable ASCII characters is single-quoted, and the others are double-quoted. Unless the style is
    * for bytes, a character is written as its UTF-8 bytes.
    */
  final case class PHP(bytes: Boolean = false) extends Style:
    def literal(s: UString): String =
      val cs = s.chars(true).toSeq
      if cs.forall(isAsciiPrintable) then
        cs.map(c => if c == '\\' || c == '\'' then s"\\${c.toChar}" else c.toChar.toString).mkString("'", "", "'")
      else cs.map(char).mkString("\"", "", "\"")
    def repeat(s: UString, n: Int): String = s"str_repeat(${literal(s)}, $n)"
    def join: String = " . "

    private def char(c: Int): String = c match
      case '\\' | '"' | '$'         => s"\\${c.toChar}"
      case Named(e)                 => e
      case c if isAsciiPrintable(c) => c.toChar.toString
      case c if bytes || c < 0x80   => f"\\x$c%02X"
      case c if isShown(c)          => codePoint(c)
      case c                        => f"\\u{$c%X}"

  /** The escapes common to the languages except JavaScript. */
  private object Named:
    def unapply(c: Int): Option[String] = c match
      case '\t' => Some("\\t")
      case '\n' => Some("\\n")
      case '\r' => Some("\\r")
      case _    => None

  /** The non-ASCII characters written as they are: letters, numbers, punctuation and symbols. */
  private lazy val Shown: CharSet =
    CharSet.union(Seq("L", "N", "P", "S").map(Properties.GeneralCategory.value(_).get.set))

  private def isAsciiPrintable(c: Int): Boolean = 0x20 <= c && c <= 0x7e

  private def isShown(c: Int): Boolean = Shown.contains(c)

  private def isSurrogate(c: Int): Boolean = 0xd800 <= c && c <= 0xdfff

  private def codePoint(c: Int): String = String.valueOf(Character.toChars(c))

  /** Returns the `\uXXXX` escapes of the UTF-16 code units of the code point. */
  private def utf16(c: Int): String = UString(c).encode(UnitMode.Utf16).map(u => f"\\u$u%04X").mkString
