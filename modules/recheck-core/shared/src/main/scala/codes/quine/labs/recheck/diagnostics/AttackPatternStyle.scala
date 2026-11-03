package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.util.NumberFormat
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.PropertyTable
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** AttackPatternStyle is a notation of [[AttackPattern]] and similar classes.
  *
  * Except [[AttackPatternStyle.Superscript]], it writes an expression of the language that evaluates to the string. The
  * JavaScript, Java and C# styles write the UTF-16 encoding of the string, which a string of these languages holds.
  */
sealed abstract class AttackPatternStyle extends Product with Serializable:

  def literal(s: UString): String

  /** Returns an expression repeating the string `n` times. */
  def repeat(s: UString, n: Int): String

  /** Returns a string concatenation operator. */
  def join: String

object AttackPatternStyle:

  /** Returns the style of the host language of the dialect for the unit mode of a pattern. */
  def of(dialect: Dialect, mode: UnitMode): AttackPatternStyle =
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
  case object JavaScript extends AttackPatternStyle:
    def literal(s: UString): String = utf16Chars(s).map(char).mkString("'", "", "'")

    def repeat(s: UString, n: Int): String = s"${literal(s)}.repeat(${n})"

    def join: String = " + "

    private def char(c: UChar): String = c match
      case '\\' | '\''              => s"\\${c.toChar}"
      case Named(e)                 => e
      case 0x0b                     => "\\v"
      case 0x0c                     => "\\f"
      case c if isASCIIPrintable(c) => c.toChar.toString
      case c if isShown(c)          => codePoint(c)
      case c if c <= 0xff           => f"\\x$c%02X"
      case c if c <= 0xffff         => f"\\u$c%04X"
      case c                        => f"\\u{$c%X}"

  /** Superscript is a superscript style specifier for logs, e.g. `'a'³¹ '\x00'`. */
  case object Superscript extends AttackPatternStyle:
    def literal(s: UString): String = JavaScript.literal(s)

    def repeat(s: UString, n: Int): String = s"${literal(s)}${NumberFormat.superscript(n)}"

    def join: String = " "

  /** Python is a Python style specifier, e.g. `'a' * 31 + '\x00'`, or `b'a' * 31 + b'\x00'` for bytes. */
  final case class Python(bytes: Boolean) extends AttackPatternStyle:
    def literal(s: UString): String = s.iterator.map(char).mkString(if bytes then "b'" else "'", "", "'")

    def repeat(s: UString, n: Int): String = s"${literal(s)} * $n"

    def join: String = " + "

    private def char(c: UChar): String = c match
      case '\\' | '\''               => s"\\${c.toChar}"
      case Named(e)                  => e
      case c if isASCIIPrintable(c)  => c.toChar.toString
      case c if !bytes && isShown(c) => codePoint(c)
      case c if c <= 0xff            => f"\\x$c%02x"
      case c if c <= 0xffff          => f"\\u$c%04x"
      case c                         => f"\\U$c%08x"

  /** Ruby is a Ruby style specifier, e.g. `"a" * 31 + "\x00"`, or `"a".b * 31 + "\x00".b` for bytes. */
  final case class Ruby(bytes: Boolean) extends AttackPatternStyle:
    def literal(s: UString): String =
      val cs = s.iterator.toIndexedSeq
      cs.indices
        .map: i =>
          // `#` starts an interpolation before these characters.
          if cs(i) == '#' && i + 1 < cs.size && "{$@".indexOf(cs(i + 1)) >= 0 then "\\#" else char(cs(i))
        .mkString("\"", "", if bytes then "\".b" else "\"")

    def repeat(s: UString, n: Int): String = s"${literal(s)} * $n"

    def join: String = " + "

    private def char(c: UChar): String = c match
      case '\\' | '"'               => s"\\${c.toChar}"
      case Named(e)                 => e
      case c if isASCIIPrintable(c) => c.toChar.toString
      case c if bytes || c < 0x80   => f"\\x$c%02X"
      case c if isSurrogate(c)      => UString(c).encode(UnitMode.Byte).map(b => f"\\x$b%02X").mkString
      case c if isShown(c)          => codePoint(c)
      case c if c <= 0xffff         => f"\\u$c%04X"
      case c                        => f"\\u{$c%X}"

  /** Java is a Java style specifier, e.g. `"a".repeat(31) + "\u0000"`. */
  case object Java extends AttackPatternStyle:
    def literal(s: UString): String = utf16Chars(s).map(char).mkString("\"", "", "\"")

    def repeat(s: UString, n: Int): String = s"${literal(s)}.repeat($n)"

    def join: String = " + "

    // Java translates `\uXXXX` before reading a literal, so a line terminator, `"` and `\` need the other escapes.
    private def char(c: UChar): String = c match
      case '\\' | '"'               => s"\\${c.toChar}"
      case Named(e)                 => e
      case c if isASCIIPrintable(c) => c.toChar.toString
      case c if isShown(c)          => codePoint(c)
      case c                        => utf16(c)

  /** CSharp is a C# style specifier for .NET, e.g. `string.Concat(Enumerable.Repeat("a", 31)) + "\u0000"`.
    *
    * `Enumerable.Repeat` needs `System.Linq`.
    */
  case object CSharp extends AttackPatternStyle:
    def literal(s: UString): String = utf16Chars(s).map(char).mkString("\"", "", "\"")

    def repeat(s: UString, n: Int): String = s"string.Concat(Enumerable.Repeat(${literal(s)}, $n))"

    def join: String = " + "

    private def char(c: UChar): String = c match
      case '\\'                     => "\\\\"
      case '"'                      => "\\\""
      case Named(e)                 => e
      case c if isASCIIPrintable(c) => c.toChar.toString
      case c if isShown(c)          => codePoint(c)
      case c                        => utf16(c)

  /** Perl is a Perl style specifier, e.g. `"a" x 31 . "\x00"`.
    *
    * Its literals are ASCII.
    */
  case object Perl extends AttackPatternStyle:
    def literal(s: UString): String = s.iterator.map(char).mkString("\"", "", "\"")

    def repeat(s: UString, n: Int): String = s"${literal(s)} x $n"

    def join: String = " . "

    // ASCII literals do not depend on `use utf8`.
    private def char(c: UChar): String = c match
      case '\\' | '"' | '$' | '@'   => s"\\${c.toChar}"
      case Named(e)                 => e
      case c if isASCIIPrintable(c) => c.toChar.toString
      case c if c <= 0xff           => f"\\x$c%02X"
      case c                        => f"\\x{$c%X}"

  /** PHP is a PHP style specifier for PCRE, e.g. `str_repeat('a', 31) . "\x00"`.
    *
    * A literal of printable ASCII characters is single-quoted, and the others are double-quoted. Unless the style is
    * for bytes, a character is written as its UTF-8 bytes.
    */
  final case class PHP(bytes: Boolean) extends AttackPatternStyle:
    def literal(s: UString): String =
      val cs = s.iterator.toSeq
      if cs.forall(isASCIIPrintable) then
        cs.map(c => if c == '\\' || c == '\'' then s"\\${c.toChar}" else c.toChar.toString).mkString("'", "", "'")
      else cs.map(char).mkString("\"", "", "\"")

    def repeat(s: UString, n: Int): String = s"str_repeat(${literal(s)}, $n)"

    def join: String = " . "

    private def char(c: UChar): String = c match
      case '\\' | '"' | '$'         => s"\\${c.toChar}"
      case Named(e)                 => e
      case c if isASCIIPrintable(c) => c.toChar.toString
      case c if bytes || c < 0x80   => f"\\x$c%02X"
      case c if isShown(c)          => codePoint(c)
      case c                        => f"\\u{$c%X}"

  /** Returns the characters of the UTF-16 encoding of the string, where a surrogate pair is one character as a language
    * with UTF-16 strings reads it.
    */
  private def utf16Chars(s: UString): Iterator[UChar] = UString.fromString(s.asString).iterator

  /** The escapes common to the languages. */
  private object Named:
    def unapply(c: UChar): Option[String] = c match
      case '\t' => Some("\\t")
      case '\n' => Some("\\n")
      case '\r' => Some("\\r")
      case _    => None

  /** The non-ASCII characters written as they are: letters, numbers, punctuation and symbols. */
  private lazy val Shown: CharSet =
    CharSet.union(Seq("L", "N", "P", "S").map(PropertyTable.GeneralCategory.value(_).get.set))

  private def isASCIIPrintable(c: UChar): Boolean = 0x20 <= c && c <= 0x7e

  private def isShown(c: UChar): Boolean = Shown.contains(c)

  private def isSurrogate(c: UChar): Boolean = 0xd800 <= c && c <= 0xdfff

  private def codePoint(c: UChar): String = String.valueOf(Character.toChars(c))

  /** Returns the `\uXXXX` escapes of the UTF-16 code units of the code point. */
  private def utf16(c: UChar): String = UString(c).encode(UnitMode.UTF16).map(u => f"\\u$u%04X").mkString
