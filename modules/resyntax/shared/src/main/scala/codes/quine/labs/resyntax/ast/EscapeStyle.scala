package codes.quine.labs.resyntax.ast

/** EscapeStyle is a backslash escape style. */
sealed abstract class EscapeStyle extends Product with Serializable

object EscapeStyle {

  /** Single is a single character escape sequence `\n`. */
  final case class Single(char: Char) extends EscapeStyle

  /** Control is `\cX` escape sequence. */
  final case class Control(char: Char) extends EscapeStyle

  /** Octal is `\o{010}` escape sequence. */
  case object Octal extends EscapeStyle

  /** BareOctal is `\010` escape sequence. */
  case object BareOctal extends EscapeStyle

  /** UnicodeHex4 is `\uXXXX` escape sequence. */
  case object UnicodeHex4 extends EscapeStyle

  /** UnicodeHex8 is `\UXXXXXXXX` escape sequence. */
  case object UnicodeHex8 extends EscapeStyle

  /** UnicodeBracket is `\u{XXXX}` escape sequence. */
  case object UnicodeBracket extends EscapeStyle

  /** Hex2 is `\xXX` escape sequence. */
  case object Hex2 extends EscapeStyle

  /** Hex1 is `\xX` escape sequence. */
  case object Hex1 extends EscapeStyle

  /** HexBracket is `\x{XXXX}` escape sequence. */
  case object HexBracket extends EscapeStyle
}
