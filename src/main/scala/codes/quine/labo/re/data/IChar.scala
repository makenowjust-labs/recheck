package codes.quine.labo.re.data

import IntervalSet._

/** IChar is a code point interval set with extra informations. */
final case class IChar(set: IntervalSet[UChar], isNewline: Boolean, isWord: Boolean) extends Ordered[IChar] {

  /** Checks whether this interval set is empty or not. */
  def isEmpty: Boolean = set.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = set.nonEmpty

  /** Computes a partition of two interval sets. */
  def partition(that: IChar): Partition[IChar] = {
    val Partition(is, ls, rs) = set.partition(that.set)
    val ic = IChar(is, isNewline || that.isNewline, isWord || that.isWord)
    Partition(ic, copy(set = ls), that.copy(set = rs))
  }

  /** Compares to other code point interval set. */
  def compare(that: IChar): Int = IChar.ordering.compare(this, that)
}

/** IChar utilities. */
object IChar {

  /** [[scala.math.Ordering]] instance for [[IChar]]. */
  private val ordering: Ordering[IChar] =
    Ordering.by[IChar, IntervalSet[UChar]](_.set).orElseBy(_.isNewline).orElseBy(_.isWord)

  /** Creates an interval set containing any code points. */
  def any: IChar = IChar(IntervalSet((UChar(0), UChar(0x110000))), false, false)
}
