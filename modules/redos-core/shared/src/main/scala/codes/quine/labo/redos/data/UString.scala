package codes.quine.labo.redos.data

/** UString is a wrapper of a sequence of UChar. */
final case class UString(seq: IndexedSeq[UChar]) extends AnyVal {

  /** A size of this string. */
  def size: Int = seq.size

  /** Tests whether this string is empty or not. */
  def isEmpty: Boolean = seq.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = seq.nonEmpty

  /** Gets the `n`-th character of this string. */
  def get(n: Int): Option[UChar] =
    if (0 <= n && n < size) Some(seq(n)) else None

  /** Returns substring of this string between `begin` and `end - 1`. */
  def substring(begin: Int, end: Int): UString = UString(seq.slice(begin, end))

  /** Inserts a character at the `n`-th character. */
  def insertAt(n: Int, c: UChar): UString =
    replace(n, 0, UString(IndexedSeq(c)))

  /** Inserts a string at the `n`-th character. */
  def insert(n: Int, s: UString): UString =
    replace(n, 0, s)

  /** Deletes `size` characters from the `n`-th character. */
  def delete(n: Int, size: Int): UString =
    replace(n, size, UString.empty)

  /** Replaces a character at the `n`-th character. */
  def replaceAt(n: Int, c: UChar): UString =
    replace(n, 1, UString(IndexedSeq(c)))

  /** Replaces `size` characters with the string. */
  def replace(n: Int, size: Int, s: UString): UString =
    UString(seq.slice(0, n) ++ s.seq ++ seq.slice(n + size, seq.size))

  /** Gets back to usual string. */
  def asString: String = seq.iterator.map(_.asString).mkString

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

  /** An empty string. */
  def empty: UString = UString(IndexedSeq.empty)

  /** Canonicalizes the string. */
  def canonicalize(s: UString, unicode: Boolean): UString =
    UString(s.seq.map(UChar.canonicalize(_, unicode)))
}
