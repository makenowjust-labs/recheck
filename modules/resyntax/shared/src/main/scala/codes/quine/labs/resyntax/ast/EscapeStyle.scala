package codes.quine.labs.resyntax.ast

sealed abstract class EscapeStyle extends Product with Serializable:

  /** Returns the plain text of the value after the letters of an escape in the style (e.g. `7F` after `\x`). */
  def plainText(value: Int): String = this match
    case EscapeStyle.Single(_)     => ""
    case EscapeStyle.Control(char) => char.toString
    // Ruby takes `?` after `\C-` as DEL, and the value of any other character without its bits 0x60.
    case EscapeStyle.ControlDash => if value == 0x7f then "\\?" else f"\\x$value%02X"
    case EscapeStyle.Meta        => f"\\x${value & 0x7f}%02X"
    case EscapeStyle.Octal       => s"{${Integer.toOctalString(value)}}"
    // Three digits keep a following digit apart.
    case EscapeStyle.BareOctal       => f"$value%03o"
    case EscapeStyle.UnicodeHex4     => f"$value%04X"
    case EscapeStyle.UnicodeHex4Pair =>
      f"${Character.highSurrogate(value).toInt}%04X\\u${Character.lowSurrogate(value).toInt}%04X"
    case EscapeStyle.UnicodeHex8    => f"$value%08X"
    case EscapeStyle.UnicodeBracket => f"{$value%X}"
    case EscapeStyle.NamedHex       => f"{U+$value%X}"
    case EscapeStyle.Hex2           => f"$value%02X"
    case EscapeStyle.Hex1           => f"$value%X"
    case EscapeStyle.Hex0           => ""
    case EscapeStyle.HexBracket     => f"{$value%X}"

object EscapeStyle:

  /** Single is a single character escape sequence `\n`. */
  final case class Single(char: Char) extends EscapeStyle

  /** Control is `\cX` escape sequence. */
  final case class Control(char: Char) extends EscapeStyle

  /** ControlDash is `\C-X` escape sequence. */
  case object ControlDash extends EscapeStyle

  /** Meta is `\M-X` escape sequence. */
  case object Meta extends EscapeStyle

  /** Octal is `\o{010}` escape sequence. */
  case object Octal extends EscapeStyle

  /** BareOctal is `\010` escape sequence. */
  case object BareOctal extends EscapeStyle

  /** UnicodeHex4 is `\uXXXX` escape sequence. */
  case object UnicodeHex4 extends EscapeStyle

  /** UnicodeHex4Pair is `\uXXXX\uXXXX` escape sequence for a surrogate pair. */
  case object UnicodeHex4Pair extends EscapeStyle

  /** UnicodeHex8 is `\UXXXXXXXX` escape sequence. */
  case object UnicodeHex8 extends EscapeStyle

  /** UnicodeBracket is `\u{XXXX}` escape sequence. */
  case object UnicodeBracket extends EscapeStyle

  /** NamedHex is `\N{U+XXXX}` escape sequence. */
  case object NamedHex extends EscapeStyle

  /** Hex2 is `\xXX` escape sequence. */
  case object Hex2 extends EscapeStyle

  /** Hex1 is `\xX` escape sequence. */
  case object Hex1 extends EscapeStyle

  /** Hex0 is `\x` escape sequence without digits. */
  case object Hex0 extends EscapeStyle

  /** HexBracket is `\x{XXXX}` escape sequence. */
  case object HexBracket extends EscapeStyle
