package codes.quine.labo.redos.data

/** UString is a wrapper of a sequence of UChar. */
final case class UString(seq: IndexedSeq[UChar]) extends AnyVal {

  /** A size of this string. */
  def size: Int = seq.size

  /** Gets the `n`-th character of this string. */
  def get(n: Int): Option[UChar] =
    if (0 <= n && n < size) Some(seq(n)) else None

  /** Returns substring of this string between `begin` and `end - 1`. */
  def substring(begin: Int, end: Int): UString = UString(seq.slice(begin, end))

  /** Shows this string as human readable format. */
  override def toString: String =
    seq
      .map {
        case UChar('\'') => "\\'"
        case c           => c.toString
      }
      .mkString("'", "", "'")
}

/** UString utilities. */
object UString {

  /** Creates a UString from the actual string. */
  def from(s: String, unicode: Boolean): UString =
    if (unicode) {
      val seq = IndexedSeq.newBuilder[UChar]
      var i = 0
      while (i < s.size) {
        val code = s.codePointAt(i)
        i += Character.charCount(code)
        seq.addOne(UChar(code))
      }
      UString(seq.result())
    } else UString(s.toIndexedSeq.map(UChar(_)))
}
