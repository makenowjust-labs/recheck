package codes.quine.labo.redos.data

import com.ibm.icu.lang.UCharacter

/** UChar is a Unicode code point. */
final case class UChar(value: Int) extends AnyVal with Ordered[UChar] {

  /** Checks this code point is valid or not. */
  def isValidCodePoint: Boolean = 0 <= value && value <= 0x10ffff

  /** Compares to other Unicode code point. */
  def compare(that: UChar): Int = this.value.compare(that.value)

  /** Converts to a UTF-16 characters. */
  def toChars: Array[Char] =
    if (value <= 0xffff) Array(value.toChar)
    else {
      val c1 = (value >> 16) & 0xffff
      val c2 = value & 0xffff
      val d1 = (0xd800 | ((c1 - 1) << 6) | (c2 >> 10)).toChar
      val d2 = (0xdc00 | (c2 & 0x3ff)).toChar
      Array(d1, d2)
    }

  /** Show this Unicode point as human readable format. */
  override def toString: String = value match {
    case 0x09                           => "\\t"
    case 0x0a                           => "\\n"
    case 0x0b                           => "\\v"
    case 0x0c                           => "\\f"
    case 0x0d                           => "\\r"
    case c if UCharacter.isPrintable(c) => String.valueOf(toChars)
    case c if c < 0x100                 => f"\\x$c%02X"
    case c if c < 0x10000               => f"\\u$c%04X"
    case c                              => f"\\u{$c%X}"
  }
}
