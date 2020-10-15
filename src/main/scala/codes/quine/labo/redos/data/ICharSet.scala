package codes.quine.labo.redos.data

import IntervalSet._

/** ICharSet is a set of [[IChar]]. */
final case class ICharSet(chars: Seq[IChar]) {

  /** Updates this by adding the [[IChar]]. */
  def add(c: IChar): ICharSet = {
    val (cs, d) = chars.foldLeft((Seq.empty[IChar], c)) { case ((cs, c), d) =>
      val Partition(i, l, r) = c.partition(d)
      (cs ++ Seq(i, r).filter(_.nonEmpty), l)
    }
    ICharSet(cs ++ Seq(d).filter(_.nonEmpty))
  }

  /** Splits the [[IChar]] into refinements on this set.
    *
    * Note the the [[IChar]] must add to the set before this method.
    */
  def refine(c: IChar): Seq[IChar] =
    chars.filter(d => c.set.intersection(d.set) == d.set)
}

/** ICharSet utilities. */
object ICharSet {

  /** Creates a [[ICharSet]] containing any [[IChar]]s. */
  def any(ignoreCase: Boolean, unicode: Boolean): ICharSet =
    if (ignoreCase) ICharSet(Seq(IChar.canonicalize(IChar.Any, unicode)))
    else ICharSet(Seq(IChar.Any))
}
