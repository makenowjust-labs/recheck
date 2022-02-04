package codes.quine.labs.recheck.unicode

import codes.quine.labs.recheck.unicode.ICharSet._
import codes.quine.labs.recheck.unicode.IntervalSet._

/** ICharSet is a set of pairs of an interval character and a character kind. */
final case class ICharSet(pairs: Seq[(IChar, CharKind)]) {

  /** Updates this by adding the [[IChar]]. */
  def add(c: IChar, k: CharKind = CharKind.Normal): ICharSet = {
    val (cs, d) = pairs.foldLeft((Vector.empty[(IChar, CharKind)], c)) { case ((cs, c), (d, j)) =>
      val Partition(i, l, r) = c.partition(d)
      (cs ++ Vector((i, k.orElse(j)), (r, j)).filter(_._1.nonEmpty), l)
    }
    ICharSet(cs ++ Vector((d, k)).filter(_._1.nonEmpty))
  }

  /** Splits the specified interval character into refinements on this set.
    *
    * Note the the interval character must be added to the set before this method.
    */
  def refine(c: IChar): Set[(IChar, CharKind)] =
    pairs.iterator.filter(d => c.set.intersection(d._1.set) == d._1.set).toSet

  /** Splits the specified interval character into inverted refinements on this set. */
  def refineInvert(c: IChar): Set[(IChar, CharKind)] = any.diff(refine(c))

  /** A character set for a 'any' pattern. */
  def any: Set[(IChar, CharKind)] = pairs.toSet

  /** A character set for a 'dot' pattern. */
  def dot: Set[(IChar, CharKind)] = any.diff(newline)

  /** A character set for newline characters. */
  def newline: Set[(IChar, CharKind)] = refine(IChar.LineTerminator)
}

/** ICharSet utilities. */
object ICharSet {

  /** Creates a [[ICharSet]] containing any [[IChar]]s. */
  def any(ignoreCase: Boolean, unicode: Boolean): ICharSet =
    ICharSet(Vector((IChar.dot(ignoreCase, true, unicode), CharKind.Normal)))

  /** CharKind is a minimum character information for assertion check. */
  sealed abstract class CharKind extends Product with Serializable {

    /** Returns the left-most not normal character kind. */
    def orElse(that: CharKind): CharKind =
      if (this == CharKind.Normal) that else this
  }

  object CharKind {

    /** A character kind for normal character. */
    case object Normal extends CharKind

    /** A character kind for line terminator character. */
    case object LineTerminator extends CharKind

    /** A character kind for word character. */
    case object Word extends CharKind
  }
}
