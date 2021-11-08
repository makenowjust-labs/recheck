package codes.quine.labo.recheck.unicode

import scala.language.implicitConversions

/** UChar is a Unicode code point. */
final case class UChar(value: Int) extends Ordered[UChar] {

  /** Returns the size of the character. */
  def size: Int = if (value >= 0x10000) 2 else 1

  /** Checks this code point is valid or not. */
  def isValidCodePoint: Boolean = 0 <= value && value <= 0x10ffff

  /** Compares to other Unicode code point. */
  override def compare(that: UChar): Int =
    value - that.value

  /** Returns its value as hash code. */
  override def hashCode(): Int = value

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

  /** Gets this Unicode code point value as a string. */
  def asString: String = String.valueOf(toChars)

  override def toString: String = value match {
    case 0x09                         => "\\t"
    case 0x0a                         => "\\n"
    case 0x0b                         => "\\v"
    case 0x0c                         => "\\f"
    case 0x0d                         => "\\r"
    case 0x5c                         => "\\\\"
    case c if 1 <= c && c < 32        => s"\\c${(c + 0x40).toChar}"
    case c if Property.isPrintable(c) => String.valueOf(toChars)
    case c if c < 0x100               => f"\\x$c%02X"
    case c if c < 0x10000             => f"\\u$c%04X"
    case c                            => f"\\u{$c%X}"
  }
}

/** UChar utilities. */
object UChar {

  /** A implicit conversion from the char to a code point. */
  implicit def charToUChar(c: Char): UChar = UChar(c.toInt)

  /** Does canonicalization to the code point. */
  def canonicalize(c: UChar, unicode: Boolean): UChar = {
    val convs = if (unicode) CaseMap.Fold else CaseMap.Upper
    convs.find(_.domain.contains(c)) match {
      case Some(conv) => UChar(c.value + conv.offset)
      case None       => c
    }
  }
}
