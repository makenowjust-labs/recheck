package codes.quine.labo.redos.data

import unicode.IntervalSet._

/** ICharSet is a set of [[IChar]]. */
final case class ICharSet(chars: Seq[IChar]) {

  /** Updates this by adding the [[IChar]]. */
  def add(c: IChar): ICharSet = {
    val (cs, d) = chars.foldLeft((Vector.empty[IChar], c)) { case ((cs, c), d) =>
      val Partition(i, l, r) = c.partition(d)
      (cs ++ Vector(i, r).filter(_.nonEmpty), l)
    }
    ICharSet(cs ++ Vector(d).filter(_.nonEmpty))
  }

  /** Splits the [[IChar]] into refinements on this set.
    *
    * Note the the [[IChar]] must add to the set before this method.
    */
  def refine(c: IChar): Set[IChar] =
    chars.filter(d => c.set.intersection(d.set) == d.set).toSet

  /** Splits the [[IChar]] into inverted refinements on this set. */
  def refineInvert(c: IChar): Set[IChar] = any.diff(refine(c))

  /** A character set for a 'any' pattern. */
  def any: Set[IChar] = chars.toSet

  /** A character set for a 'dot' pattern. */
  def dot: Set[IChar] = any.diff(refine(IChar.LineTerminator).toSet)
}

/** ICharSet utilities. */
object ICharSet {

  /** Creates a [[ICharSet]] containing any [[IChar]]s. */
  def any(ignoreCase: Boolean, unicode: Boolean): ICharSet =
    ICharSet(Vector(IChar.dot(ignoreCase, true, unicode)))
}
