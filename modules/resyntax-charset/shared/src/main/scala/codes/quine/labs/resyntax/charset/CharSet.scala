package codes.quine.labs.resyntax.charset

import scala.collection.mutable

import codes.quine.labs.resyntax.charset.CharSet.*

/** CharSet is an immutable set of non-negative code values.
  *
  * It is represented by strictly increasing interval boundaries `b(0) < b(1) < ...` of even length. The set contains
  * the half-open intervals `[b(0), b(1))`, `[b(2), b(3))`, and so on.
  */
final class CharSet private (private val bounds: Array[Int]):

  def isEmpty: Boolean = bounds.length == 0

  def nonEmpty: Boolean = bounds.length != 0

  def intervalCount: Int = bounds.length / 2

  def intervalStart(i: Int): Int = bounds(2 * i)

  def intervalEnd(i: Int): Int = bounds(2 * i + 1)

  /** Iterates the maximal intervals of this set as half-open pairs `(start, end)` in ascending order. */
  def intervals: Iterator[(Int, Int)] =
    Iterator.range(0, intervalCount).map(i => (intervalStart(i), intervalEnd(i)))

  def boundaries: IArray[Int] = IArray.unsafeFromArray(bounds)

  def iterator: Iterator[Int] = intervals.flatMap((start, end) => Iterator.range(start, end))

  def size: Int =
    var n = 0
    var i = 0
    while i < bounds.length do
      n += bounds(i + 1) - bounds(i)
      i += 2
    n

  def head: Int =
    if isEmpty then throw new NoSuchElementException("CharSet.empty.head")
    bounds(0)

  def last: Int =
    if isEmpty then throw new NoSuchElementException("CharSet.empty.last")
    bounds(bounds.length - 1) - 1

  def contains(value: Int): Boolean = (countLessOrEqual(bounds, value) & 1) == 1

  /** Checks whether this set contains a code value in `[first, last]`. */
  def intersectsRange(first: Int, last: Int): Boolean =
    val i = countLessOrEqual(bounds, first)
    (i & 1) == 1 || i < bounds.length && bounds(i) <= last

  infix def union(that: CharSet): CharSet =
    if isEmpty then that
    else if that.isEmpty then this
    else combine(bounds, that.bounds, UnionTable)

  infix def intersect(that: CharSet): CharSet =
    if isEmpty || that.isEmpty then CharSet.empty
    else combine(bounds, that.bounds, IntersectTable)

  infix def diff(that: CharSet): CharSet =
    if isEmpty || that.isEmpty then this
    else combine(bounds, that.bounds, DiffTable)

  def complement(mode: UnitMode): CharSet = mode.universe.diff(this)

  def restrict(mode: UnitMode): CharSet =
    if isEmpty || bounds(bounds.length - 1) <= mode.limit then this
    else intersect(mode.universe)

  infix def partition(that: CharSet): Partition =
    Partition(intersect(that), diff(that), that.diff(this))

  def subsetOf(that: CharSet): Boolean = diff(that).isEmpty

  def intersects(that: CharSet): Boolean = intersect(that).nonEmpty

  /** Code values shifted below zero are dropped. */
  def shift(offset: Int): CharSet =
    if offset == 0 || isEmpty then this
    else
      val builder = newBuilder
      for (start, end) <- intervals do builder.addRange(math.max(start + offset, 0), math.max(end + offset, 0))
      builder.result()

  /** Calls the function with every non-empty intersection `[start, end)` of an interval of this set and
    * `[from, until)`.
    */
  private[charset] def foreachIntervalWithin(from: Int, until: Int)(f: (Int, Int) => Unit): Unit =
    var i = countLessOrEqual(bounds, from)
    if (i & 1) == 1 then i -= 1
    while i < bounds.length && bounds(i) < until do
      val start = math.max(bounds(i), from)
      val end = math.min(bounds(i + 1), until)
      if start < end then f(start, end)
      i += 2

  override def equals(that: Any): Boolean = that match
    case that: CharSet => java.util.Arrays.equals(bounds, that.bounds)
    case _             => false

  override def hashCode(): Int = java.util.Arrays.hashCode(bounds)

  override def toString: String =
    def hex(value: Int): String = "0x" + Integer.toHexString(value).toUpperCase
    intervals
      .map((start, end) => if start + 1 == end then hex(start) else s"${hex(start)}-${hex(end - 1)}")
      .mkString("CharSet(", ", ", ")")

object CharSet:

  final case class Partition(both: CharSet, onlyThis: CharSet, onlyThat: CharSet)

  /** Builder accumulates intervals in any order to build a set. */
  final class Builder private[CharSet] ():
    private val ranges = mutable.ArrayBuilder.make[Long]

    /** Adds the half-open interval `[start, end)`; an empty interval is ignored. */
    def addRange(start: Int, end: Int): this.type =
      require(start >= 0, s"negative code value: $start")
      if start < end then ranges.addOne((start.toLong << 32) | end.toLong)
      this

    def addOne(value: Int): this.type = addRange(value, value + 1)

    def addAll(set: CharSet): this.type =
      var i = 0
      while i < set.bounds.length do
        addRange(set.bounds(i), set.bounds(i + 1))
        i += 2
      this

    def result(): CharSet =
      val rs = ranges.result()
      java.util.Arrays.sort(rs)
      val out = mutable.ArrayBuilder.make[Int]
      var i = 0
      while i < rs.length do
        val start = (rs(i) >>> 32).toInt
        var end = rs(i).toInt
        i += 1
        while i < rs.length && (rs(i) >>> 32).toInt <= end do
          end = math.max(end, rs(i).toInt)
          i += 1
        out.addOne(start)
        out.addOne(end)
      new CharSet(out.result())

  val empty: CharSet = new CharSet(Array.empty)

  def newBuilder: Builder = new Builder

  def single(value: Int): CharSet =
    require(value >= 0, s"negative code value: $value")
    new CharSet(Array(value, value + 1))

  /** Creates a set containing the code values in `[first, last]`; it is empty if `first > last`. */
  def range(first: Int, last: Int): CharSet =
    require(first >= 0, s"negative code value: $first")
    if first > last then empty else new CharSet(Array(first, last + 1))

  def of(values: Int*): CharSet =
    val builder = newBuilder
    values.foreach(builder.addOne)
    builder.result()

  /** Creates a set from half-open intervals `(start, end)` in any order; they may overlap. */
  def fromIntervals(intervals: IterableOnce[(Int, Int)]): CharSet =
    val builder = newBuilder
    intervals.iterator.foreach((start, end) => builder.addRange(start, end))
    builder.result()

  def fromBoundaries(boundaries: IArray[Int]): CharSet =
    require(boundaries.length % 2 == 0, "odd number of boundaries")
    require(boundaries.isEmpty || boundaries(0) >= 0, "negative code value")
    require((1 until boundaries.length).forall(i => boundaries(i - 1) < boundaries(i)), "unsorted boundaries")
    new CharSet(IArray.genericWrapArray(boundaries).toArray)

  private[charset] def unsafeFromBoundaries(boundaries: Array[Int]): CharSet = new CharSet(boundaries)

  def union(sets: IterableOnce[CharSet]): CharSet =
    val builder = newBuilder
    sets.iterator.foreach(builder.addAll)
    builder.result()

  // Truth tables indexed by `inThis * 2 + inThat`.
  private val UnionTable = 0xe
  private val IntersectTable = 0x8
  private val DiffTable = 0x4

  private def combine(a: Array[Int], b: Array[Int], table: Int): CharSet =
    val out = new Array[Int](a.length + b.length)
    var i = 0
    var j = 0
    var n = 0
    var inA = 0
    var inB = 0
    var inR = 0
    while i < a.length || j < b.length do
      val p =
        if j >= b.length then a(i)
        else if i >= a.length then b(j)
        else math.min(a(i), b(j))
      if i < a.length && a(i) == p then
        inA ^= 1
        i += 1
      if j < b.length && b(j) == p then
        inB ^= 1
        j += 1
      val r = (table >> (inA * 2 + inB)) & 1
      if r != inR then
        out(n) = p
        n += 1
        inR = r
    new CharSet(java.util.Arrays.copyOf(out, n))

  /** The array must be sorted. */
  private[charset] def countLessOrEqual(array: Array[Int], value: Int): Int =
    var lo = 0
    var hi = array.length
    while lo < hi do
      val mid = (lo + hi) >>> 1
      if array(mid) <= value then lo = mid + 1 else hi = mid
    lo
