package codes.quine.labs.recheck
package automaton

/** Branch is an ordered tree of choices with look-ahead assertions.
  *
  * A backtracking matcher explores the leaves in order, and it stops at the first success.
  */
enum Branch[+Q]:

  /** Leaf is a choice of the state. */
  case Leaf(q: Q)

  /** Succeed is an end of the inner expression of a look-ahead, so it succeeds regardless of the rest of the input. */
  case Succeed

  /** Choice tries the branches in order. */
  case Choice(bs: Seq[Branch[Q]])

  /** LookAhead runs the inner expression atomically, and continues to `cont` if it succeeds (or fails, if negative). */
  case LookAhead(negative: Boolean, inner: Branch[Q], cont: Branch[Q])

  /** Lists the leaves in order. */
  def leaves: Seq[Q] = this match
    case Leaf(q)                   => Vector(q)
    case Succeed                   => Vector.empty
    case Choice(bs)                => bs.flatMap(_.leaves)
    case LookAhead(_, inner, cont) => inner.leaves ++ cont.leaves

  /** Tests whether this branch succeeds when exactly the leaves satisfying `p` succeed. */
  def succeeds(p: Q => Boolean): Boolean = this match
    case Leaf(q)                          => p(q)
    case Succeed                          => true
    case Choice(bs)                       => bs.exists(_.succeeds(p))
    case LookAhead(negative, inner, cont) => inner.succeeds(p) != negative && cont.succeeds(p)

  /** Lists the leaves explored by a backtracking matcher when exactly the leaves satisfying `p` succeed. */
  def explore(p: Q => Boolean): Seq[Q] = exploreAndSucceed(p)._1

  /** Lists the explored leaves, and tests whether this branch succeeds, in one pass. */
  private def exploreAndSucceed(p: Q => Boolean): (Vector[Q], Boolean) = this match
    case Leaf(q)    => (Vector(q), p(q))
    case Succeed    => (Vector.empty, true)
    case Choice(bs) =>
      bs.foldLeft((Vector.empty[Q], false)):
        case (explored @ (_, true), _) => explored
        case ((leaves, false), b)      =>
          val (bLeaves, succeeds) = b.exploreAndSucceed(p)
          (leaves ++ bLeaves, succeeds)
    case LookAhead(negative, inner, cont) =>
      val (innerLeaves, succeeds) = inner.exploreAndSucceed(p)
      if succeeds == negative then (innerLeaves, false)
      else
        val (contLeaves, contSucceeds) = cont.exploreAndSucceed(p)
        (innerLeaves ++ contLeaves, contSucceeds)

  /** Applies the function to the leaves. */
  def map[R](f: Q => R): Branch[R] = flatMap(q => Leaf(f(q)))

  /** Replaces the leaves with branches, and simplifies the result. */
  def flatMap[R](f: Q => Branch[R]): Branch[R] = this match
    case Leaf(q)                          => f(q)
    case Succeed                          => Succeed
    case Choice(bs)                       => Branch.choice(bs.map(_.flatMap(f)))
    case LookAhead(negative, inner, cont) => Branch.lookAhead(negative, inner.flatMap(f), cont.flatMap(f))

/** Branch utilities. */
object Branch:

  /** Fail is a branch without choices. */
  val Fail: Branch[Nothing] = Choice(Vector.empty)

  /** Builds a simplified choice of the branches. */
  def choice[Q](bs: Seq[Branch[Q]]): Branch[Q] =
    val flat = bs.flatMap:
      case Choice(cs) => cs
      case b          => Vector(b)
    // The branches after `Succeed` are unreachable.
    val (before, after) = flat.span(_ != Succeed)
    before ++ after.take(1) match
      case Seq(b) => b
      case cs     => Choice(cs.toVector)

  /** Builds a simplified look-ahead assertion. */
  def lookAhead[Q](negative: Boolean, inner: Branch[Q], cont: Branch[Q]): Branch[Q] =
    if inner == Fail then (if negative then cont else Fail)
    else if inner == Succeed then (if negative then Fail else cont)
    else LookAhead(negative, inner, cont)
