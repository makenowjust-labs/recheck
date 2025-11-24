package codes.quine.labs.recheck.unicode

import scala.math.Ordering.Implicits.*

import codes.quine.labs.recheck.unicode.IntervalSet.*

/** IntervalSet is set of intervals.
  *
  * [[intervals]] is a sequence of discrete intervals sorted by start value. Each interval is represented by a pair of
  * values `[start, end)`.
  */
final case class IntervalSet[@specialized A](intervals: IndexedSeq[(A, A)]):

  /** Checks whether this interval set is empty or not. */
  def isEmpty: Boolean = intervals.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = intervals.nonEmpty

  /** Computes a union of two interval sets. */
  infix def union(that: IntervalSet[A])(using A: Ordering[A]): IntervalSet[A] =
    // Optimizes for empty interval sets.
    if isEmpty then that
    else if that.isEmpty then this
    else from(intervals ++ that.intervals)

  /** Computes an intersection of two interval sets. */
  infix def intersection(that: IntervalSet[A])(using A: Ordering[A]): IntervalSet[A] =
    partition(that).intersection

  /** Computes a difference interval set. */
  infix def diff(that: IntervalSet[A])(using A: Ordering[A]): IntervalSet[A] =
    partition(that).diffThat

  /** Computes `this intersect that`, `this diff that` and `that diff this` at once. */
  infix def partition(that: IntervalSet[A])(using A: Ordering[A]): Partition[IntervalSet[A]] =
    val lefts = intervals.flatMap((x, y) => Vector(Left(x), Left(y)))
    val rights = that.intervals.flatMap((x, y) => Vector(Right(x), Right(y)))

    val optA = Option.empty[A]
    val seqAA = IndexedSeq.empty[(A, A)]
    val (_, _, is, ls, rs) = (lefts ++ rights)
      .sortBy(_.fold(identity[A], identity[A]))
      .foldLeft((optA, optA, seqAA, seqAA, seqAA)):
        case ((None, None, is, ls, rs), Left(x))           => (Some(x), None, is, ls, rs)
        case ((None, None, is, ls, rs), Right(x))          => (None, Some(x), is, ls, rs)
        case ((Some(x), None, is, ls, rs), Left(y))        => (None, None, is, ls :+ (x, y), rs)
        case ((Some(x1), None, is, ls, rs), Right(x2))     => (Some(x1), Some(x2), is, ls :+ (x1, x2), rs)
        case ((None, Some(x2), is, ls, rs), Left(x1))      => (Some(x1), Some(x2), is, ls, rs :+ (x2, x1))
        case ((None, Some(x), is, ls, rs), Right(y))       => (None, None, is, ls, rs :+ (x, y))
        case ((Some(x1), Some(x2), is, ls, rs), Left(y1))  => (None, Some(y1), is :+ (A.max(x1, x2), y1), ls, rs)
        case ((Some(x1), Some(x2), is, ls, rs), Right(y2)) => (Some(y2), None, is :+ (A.max(x1, x2), y2), ls, rs)

    def nonEmpty(xy: (A, A)): Boolean = xy._1 != xy._2
    Partition(IntervalSet(is.filter(nonEmpty)), IntervalSet(ls.filter(nonEmpty)), IntervalSet(rs.filter(nonEmpty)))

  /** Checks whether this interval set contains the value or not. */
  def contains(value: A)(using A: Ordering[A]): Boolean =
    val i = intervals.search((value, value)).insertionPoint
    def contains(xy: (A, A), z: A): Boolean = xy._1 <= z && z < xy._2
    i < intervals.size && contains(intervals(i), value) || 0 < i && contains(intervals(i - 1), value)

  /** Converts each interval by the mapping.
    *
    * Note that the mapping `f` must be monotonic on each interval.
    */
  def map[B](f: A => B)(using B: Ordering[B]): IntervalSet[B] =
    from(intervals.map((x, y) => (f(x), f(y))))

  /** Merges the mapped intersection and the difference.
    *
    * Note that the mapping `f` must be monotonic on each interval.
    */
  def mapIntersection(that: IntervalSet[A])(f: A => A)(using A: Ordering[A]): IntervalSet[A] =
    val Partition(is, ls, _) = partition(that)
    is.map(f).union(ls)

  /** Returns a string representation of this interval set. */
  override def toString: String =
    intervals.map((x, y) => s"($x, $y)").mkString("IntervalSet(", ", ", ")")

/** IntervalSet utilities. */
object IntervalSet:

  /** Partition is a result type of [[IntervalSet#partition]]. */
  final case class Partition[A](intersection: A, diffThat: A, diffThis: A)

  /** Normalizes an interval sequence.
    *
    * It does two things.
    *
    *   1. it merges overlaps. 2. it removes empty intervals.
    */
  private def normalize[A](intervals: Seq[(A, A)])(using A: Ordering[A]): IndexedSeq[(A, A)] =
    intervals.sorted.foldLeft(IndexedSeq.empty[(A, A)]):
      case (xys, (x, y)) if x >= y                               => xys
      case (Seq(), (x, y))                                       => IndexedSeq((x, y))
      case (xys :+ ((x1, y1)), (x2, y2)) if x1 <= x2 && x2 <= y1 => xys :+ (x1, A.max(y1, y2))
      case (xys, (x, y))                                         => xys :+ (x, y)

  /** Creates an interval set from the intervals. */
  def apply[A](intervals: (A, A)*)(using A: Ordering[A]): IntervalSet[A] =
    from(intervals)

  /** Creates an empty interval set. */
  def empty[A]: IntervalSet[A] = IntervalSet(IndexedSeq.empty)

  /** Creates an interval set from the interval list. */
  def from[A](intervals: Seq[(A, A)])(using A: Ordering[A]): IntervalSet[A] =
    IntervalSet(normalize(intervals))

  given ordering[A](using A: Ordering[A]): Ordering[IntervalSet[A]] =
    Ordering.by(_.intervals)
