package codes.quine.labo.recheck.data

/** UString is a wrapper of a string with Unicode manipulation. */
final case class UString(asString: String) extends AnyVal {

  /** An alias of `asString.length`. */
  def sizeAsString: Int = asString.length

  /** A size of this string. */
  def size(unicode: Boolean): Int =
    if (unicode) asString.codePointCount(0, sizeAsString) else sizeAsString

  /** Tests whether this string is empty or not. */
  def isEmpty: Boolean = asString.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = asString.nonEmpty

  /** Returns a character at the specified index. */
  def getAt(n: Int, unicode: Boolean): Option[UChar] =
    if (0 <= n && n < sizeAsString) {
      Some(UChar(if (unicode) asString.codePointAt(n) else asString.charAt(n)))
    } else None

  /** Returns a character before the specified index. */
  def getBefore(n: Int, unicode: Boolean): Option[UChar] =
    if (0 < n && n <= sizeAsString) {
      Some(UChar(if (unicode) asString.codePointBefore(n) else asString.charAt(n - 1)))
    } else None

  /** Returns substring of this string between `begin` and `end - 1`. */
  def substring(begin: Int, end: Int): UString = UString(asString.slice(begin, end))

  /** Inserts a character at the `n`-th character. */
  def insertAt(n: Int, c: UChar): UString = {
    val str = new StringBuilder
    str.append(asString.slice(0, n))
    str.append(c.asString)
    str.append(asString.slice(n, sizeAsString))
    UString(str.result())
  }

  /** Inserts a string at the `n`-th character. */
  def insert(n: Int, s: UString): UString = {
    val str = new StringBuilder
    str.append(asString.slice(0, n))
    str.append(s.asString)
    str.append(asString.slice(n, sizeAsString))
    UString(str.result())
  }

  /** Replaces a character at the `n`-th character. */
  def replaceAt(n: Int, c: UChar, unicode: Boolean): UString = {
    val skip = getAt(n, unicode).map(_.size).getOrElse(0)
    val str = new StringBuilder
    str.append(asString.slice(0, n))
    str.append(c.asString)
    str.append(asString.slice(n + skip, sizeAsString))
    UString(str.result())
  }

  /** Iterates the string. */
  def iterator(unicode: Boolean): Iterator[UChar] =
    if (unicode) {
      Iterator.unfold(0) { i =>
        if (i >= sizeAsString) None
        else {
          val c = UChar(asString.codePointAt(i))
          Some((c, i + c.size))
        }
      }
    } else asString.iterator.map(UChar(_))

  override def toString: String =
    asString
      .map {
        case '\'' => "\\'"
        case c    => c.toString
      }
      .mkString("'", "", "'")
}

/** UString utilities. */
object UString {

  /** Creates a string from the character sequence. */
  def from(seq: Seq[UChar]): UString =
    UString(seq.iterator.map(_.asString).mkString)

  /** An empty string. */
  def empty: UString = UString("")

  /** Makes the string canonical. */
  def canonicalize(s: UString, unicode: Boolean): UString =
    UString(s.iterator(unicode).map(UChar.canonicalize(_, unicode)).mkString)
}
