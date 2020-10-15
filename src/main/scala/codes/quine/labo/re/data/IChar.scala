package codes.quine.labo.re
package data

import IntervalSet._
import unicode.CaseMap
import unicode.CaseMap.Conversion

/** IChar is a code point interval set with extra informations. */
final case class IChar(set: IntervalSet[UChar], isNewline: Boolean = false, isWord: Boolean = false)
    extends Ordered[IChar] {

  /** Checks whether this interval set is empty or not. */
  def isEmpty: Boolean = set.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = set.nonEmpty

  /** Computes a union of two interval sets. */
  def union(that: IChar): IChar =
    IChar(set.union(that.set), isNewline || that.isNewline, isWord || that.isWord)

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

  /** Creates an interval set containing the character only. */
  def apply(ch: Char): IChar = IChar(IntervalSet((UChar(ch), UChar(ch + 1))))

  /** Normalizes the code point interval set. */
  def canonicalize(c: IChar, unicode: Boolean): IChar = {
    val conversions = if (unicode) CaseMap.Fold else CaseMap.Upper
    val set = conversions.foldLeft(c.set) { case (set, Conversion(dom, offset)) =>
      set.mapIntersection(dom)(u => UChar(u.value + offset))
    }
    c.copy(set = set)
  }
}
