package codes.quine.labs.resyntax.charset

/** UString is an immutable sequence of the units of a [[UnitMode]], used for attack strings and the strings of a
  * [[ClassSet]].
  *
  * The units are code points in `CodePoint` mode, where a lone surrogate is a code point of itself. They are UTF-16
  * code units in `UTF16` mode, and bytes in `Byte` mode. [[UnitMode]] converts it from and to a Scala string.
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
