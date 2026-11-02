package codes.quine.labs.resyntax.charset

/** UString is an immutable sequence of code values, used for attack strings and the strings of a [[ClassSet]].
  *
  * The values are usually code points; a string over the bytes alphabet holds the bytes as values in `0..0xFF`.
  */
final class UString private (private val values: Array[UChar]) extends Ordered[UString]:

  def length: Int = values.length

  def isEmpty: Boolean = values.length == 0

  def nonEmpty: Boolean = values.length != 0

  def apply(i: Int): UChar = values(i)

  def codeValues: IArray[UChar] = IArray.unsafeFromArray(values)

  def iterator: Iterator[UChar] = values.iterator

  def ++(that: UString): UString = new UString(values ++ that.values)

  def slice(from: Int, until: Int): UString = new UString(values.slice(from, until))

  def map(f: UChar => UChar): UString = new UString(values.map(f))

  def insert(n: Int, that: UString): UString = slice(0, n) ++ that ++ slice(n, length)

  /** Returns the character at the position of this string of UTF-16 code units.
    *
    * With `combinesSurrogatePairs`, a surrogate pair is read as one code point.
    */
  def getAt(n: Int, combinesSurrogatePairs: Boolean): Option[UChar] =
    Option.when(0 <= n && n < length):
      val c = values(n)
      if combinesSurrogatePairs && UString.isHighSurrogate(c) && n + 1 < length && UString.isLowSurrogate(values(n + 1))
      then Character.toCodePoint(c.toChar, values(n + 1).toChar)
      else c

  /** Returns the character before the position of this string of UTF-16 code units.
    *
    * With `combinesSurrogatePairs`, a surrogate pair is read as one code point.
    */
  def getBefore(n: Int, combinesSurrogatePairs: Boolean): Option[UChar] =
    Option.when(0 < n && n <= length):
      val c = values(n - 1)
      if combinesSurrogatePairs && UString.isLowSurrogate(c) && n - 2 >= 0 && UString.isHighSurrogate(values(n - 2))
      then Character.toCodePoint(values(n - 2).toChar, c.toChar)
      else c

  /** Returns the characters of this string of UTF-16 code units.
    *
    * With `combinesSurrogatePairs`, a surrogate pair is read as one code point.
    */
  def chars(combinesSurrogatePairs: Boolean): Iterator[UChar] =
    if combinesSurrogatePairs then
      Iterator.unfold(0): i =>
        getAt(i, combinesSurrogatePairs).map(c => (c, i + UString.charWidth(c)))
    else iterator

  /** Inserts the UTF-16 code units of the code point into this string of UTF-16 code units. */
  def insertAt(n: Int, c: UChar): UString = insert(n, UString.fromCodePointsAsUTF16(Iterator(c)))

  /** Replaces the character at the position of this string of UTF-16 code units with the UTF-16 code units of the code
    * point.
    *
    * With `combinesSurrogatePairs`, a surrogate pair is replaced as one code point.
    */
  def replaceAt(n: Int, c: UChar, combinesSurrogatePairs: Boolean): UString =
    val skip = getAt(n, combinesSurrogatePairs).map(UString.charWidth).getOrElse(0)
    slice(0, n) ++ UString.fromCodePointsAsUTF16(Iterator(c)) ++ slice(n + skip, length)

  /** Encodes the code points of this string as code units of the mode.
    *
    * `UTF16` gives UTF-16 code units, `CodePoint` gives the code points as they are, and `Byte` gives the UTF-8 bytes
    * (surrogate code points are encoded like other code points).
    */
  def encode(mode: UnitMode): IArray[UChar] = mode match
    case UnitMode.CodePoint => codeValues
    case UnitMode.UTF16     =>
      val out = Array.newBuilder[UChar]
      for c <- values do
        if c < 0x10000 then out.addOne(c)
        else
          out.addOne(0xd800 | ((c - 0x10000) >> 10))
          out.addOne(0xdc00 | (c & 0x3ff))
      IArray.unsafeFromArray(out.result())
    case UnitMode.Byte =>
      val out = Array.newBuilder[UChar]
      for c <- values do
        if c < 0x80 then out.addOne(c)
        else if c < 0x800 then
          out.addOne(0xc0 | (c >> 6))
          out.addOne(0x80 | (c & 0x3f))
        else if c < 0x10000 then
          out.addOne(0xe0 | (c >> 12))
          out.addOne(0x80 | ((c >> 6) & 0x3f))
          out.addOne(0x80 | (c & 0x3f))
        else
          out.addOne(0xf0 | (c >> 18))
          out.addOne(0x80 | ((c >> 12) & 0x3f))
          out.addOne(0x80 | ((c >> 6) & 0x3f))
          out.addOne(0x80 | (c & 0x3f))
      IArray.unsafeFromArray(out.result())

  def asString: String =
    val sb = new java.lang.StringBuilder
    for c <- encode(UnitMode.UTF16) do sb.append(c.toChar)
    sb.toString

  /** Strings are ordered lexicographically by code values. */
  def compare(that: UString): Int =
    val n = math.min(length, that.length)
    var i = 0
    while i < n && values(i) == that.values(i) do i += 1
    if i < n then Integer.compare(values(i), that.values(i)) else Integer.compare(length, that.length)

  override def equals(that: Any): Boolean = that match
    case that: UString => java.util.Arrays.equals(values, that.values)
    case _             => false

  override def hashCode(): Int = java.util.Arrays.hashCode(values)

  override def toString: String =
    values.map(c => "U+" + Integer.toHexString(c).toUpperCase).mkString("UString(", " ", ")")

object UString:

  val empty: UString = new UString(Array.empty)

  def apply(values: UChar*): UString = new UString(values.toArray)

  def from(values: IterableOnce[UChar]): UString = new UString(values.iterator.toArray)

  /** A lone surrogate becomes a code point of itself. */
  def fromString(s: String): UString =
    val out = Array.newBuilder[UChar]
    var i = 0
    while i < s.length do
      val c = s.codePointAt(i)
      out.addOne(c)
      i += Character.charCount(c)
    new UString(out.result())

  /** Surrogate pairs are not combined. */
  def fromUTF16Units(s: String): UString = new UString(s.map(_.toInt).toArray)

  /** Creates a string of the UTF-16 code units encoding the code points. */
  def fromCodePointsAsUTF16(cs: IterableOnce[UChar]): UString = new UString(from(cs).encode(UnitMode.UTF16).toArray)

  /** Returns the number of the UTF-16 code units of the code point, which is two only for a supplementary one. */
  def charWidth(c: UChar): Int = if c >= 0x10000 then 2 else 1

  private def isHighSurrogate(c: UChar): Boolean = 0xd800 <= c && c <= 0xdbff

  private def isLowSurrogate(c: UChar): Boolean = 0xdc00 <= c && c <= 0xdfff
