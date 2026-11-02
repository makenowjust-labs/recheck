package codes.quine.labs.resyntax.charset

/** UString is an immutable sequence of the units of a [[UnitMode]], used for attack strings and the strings of a
  * [[ClassSet]].
  *
  * The units are code points in `CodePoint` mode, where a lone surrogate is a code point of itself. They are UTF-16
  * code units in `UTF16` mode, and bytes in `Byte` mode.
  */
final class UString private (private val values: Array[UChar]) extends Ordered[UString]:

  def length: Int = values.length

  def isEmpty: Boolean = values.length == 0

  def nonEmpty: Boolean = values.length != 0

  def apply(i: Int): UChar = values(i)

  def iterator: Iterator[UChar] = values.iterator

  def ++(that: UString): UString = new UString(values ++ that.values)

  def slice(from: Int, until: Int): UString = new UString(values.slice(from, until))

  def map(f: UChar => UChar): UString = new UString(values.map(f))

  def insert(n: Int, that: UString): UString = slice(0, n) ++ that ++ slice(n, length)

  /** Returns the unit at the position, if any. */
  def getAt(n: Int): Option[UChar] = Option.when(0 <= n && n < length)(values(n))

  /** Returns the unit before the position, if any. */
  def getBefore(n: Int): Option[UChar] = Option.when(0 < n && n <= length)(values(n - 1))

  /** Encodes this string of code points as units of the mode.
    *
    * `UTF16` gives UTF-16 code units, `CodePoint` gives the code points as they are, and `Byte` gives the UTF-8 bytes
    * (surrogate code points are encoded like other code points).
    */
  def encode(mode: UnitMode): IArray[UChar] = mode match
    case UnitMode.CodePoint => IArray.unsafeFromArray(values)
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

  /** Returns this string as a Scala string, encoding it in UTF-16.
    *
    * UTF-16 code units and bytes are kept as they are.
    */
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

  /** Decodes the code points of the Scala string, where a lone surrogate becomes a code point of itself. */
  def fromString(s: String): UString =
    val out = Array.newBuilder[UChar]
    var i = 0
    while i < s.length do
      val c = s.codePointAt(i)
      out.addOne(c)
      i += Character.charCount(c)
    new UString(out.result())

  /** Returns the UTF-16 code units of the Scala string. */
  def fromUTF16Units(s: String): UString = new UString(s.map(_.toInt).toArray)
