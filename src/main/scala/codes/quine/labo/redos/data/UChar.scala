package codes.quine.labo.redos.data

/** UChar is a Unicode code point. */
final case class UChar(value: Int) extends Ordered[UChar] {

  /** Compares to other Unicode code point. */
  def compare(that: UChar): Int = this.value.compare(that.value)

  /** Converts to a UTF-16 representation. */
  override def toString: String =
    if (value <= 0xffff) String.valueOf(Array(value.toChar))
    else {
      val c1 = (value >> 16) & 0xffff
      val c2 = value & 0xffff
      val d1 = (0xd800 | ((c1 - 1) << 6) | (c2 >> 10)).toChar
      val d2 = (0xdc00 | (c2 & 0x3ff)).toChar
      String.valueOf(Array(d1, d2))
    }
}
