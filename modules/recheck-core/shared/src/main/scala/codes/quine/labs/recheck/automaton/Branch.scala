package codes.quine.labs.recheck
package automaton

/** Branch is an ordered tree of choices with look-ahead assertions and choices in atomic groups.
  *
  * A backtracking matcher explores the leaves in order, and it stops at the first success.
  */
enum Branch[+Q]:

  /** Leaf is a choice of the state. */
  case Leaf(q: Q)

  /** Succeed is an end of the inner expression of a look-ahead or of the test of an atomic group, so it succeeds
    * regardless of the rest of the input.
    */
  case Succeed

  /** Choice tries the branches in order. */
  case Choice(bs: Seq[Branch[Q]])

  /** LookAhead runs the inner expression atomically, and continues to `cont` if it succeeds (or fails, if negative). */
  case LookAhead(negative: Boolean, inner: Branch[Q], cont: Branch[Q])

  /** AtomicChoice is a choice in an atomic group, which tries the alternatives `(test, b)` in order, and commits to the
    * first one whose `test` succeeds, that is, whose `b` reaches the end of the group, or else to `last`.
    *
    * The tests are not explored, but the branches before the committed one are, and they fail within the group. `last`
    * needs no test: it fails within the group anyway if it does not reach the end.
    */
  case AtomicChoice(alternatives: Seq[(Branch[Q], Branch[Q])], last: Branch[Q])

  /** Lists the leaves that a matcher may explore in order. */
  def leaves: Seq[Q] = this match
    case Leaf(q)                          => Vector(q)
    case Succeed                          => Vector.empty
    case Choice(bs)                       => bs.flatMap(_.leaves)
    case LookAhead(_, inner, cont)        => inner.leaves ++ cont.leaves
    case AtomicChoice(alternatives, last) => alternatives.flatMap(_._2.leaves) ++ last.leaves

  /** Lists all the leaves, including the tests of [[AtomicChoice]], in order. */
  def allLeaves: Seq[Q] = this match
    case Leaf(q)                          => Vector(q)
    case Succeed                          => Vector.empty
    case Choice(bs)                       => bs.flatMap(_.allLeaves)
    case LookAhead(_, inner, cont)        => inner.allLeaves ++ cont.allLeaves
    case AtomicChoice(alternatives, last) =>
      alternatives.flatMap((test, b) => test.allLeaves ++ b.allLeaves) ++ last.allLeaves

  /** Tests whether this branch succeeds when exactly the leaves satisfying `p` succeed. */
  def succeeds(p: Q => Boolean): Boolean = this match
    case Leaf(q)                          => p(q)
    case Succeed                          => true
    case Choice(bs)                       => bs.exists(_.succeeds(p))
    case LookAhead(negative, inner, cont) => inner.succeeds(p) != negative && cont.succeeds(p)
    case AtomicChoice(alternatives, last) =>
      alternatives.find(_._1.succeeds(p)).fold(last.succeeds(p))(_._2.succeeds(p))

  /** Returns the least number of the leaves that [[explore]] lists, whichever leaves succeed. */
  def minExplored: Int = this match
    case Leaf(_) => 1
    case Succeed => 0
    // The first branch is explored before the others.
    case Choice(bs)             => bs.headOption.fold(0)(_.minExplored)
    case LookAhead(_, inner, _) => inner.minExplored
    // One of the branches is the committed one.
    case AtomicChoice(alternatives, last) => (alternatives.map(_._2.minExplored) :+ last.minExplored).min

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
    case AtomicChoice(alternatives, last) =>
      val (failed, rest) = alternatives.span(!_._1.succeeds(p))
      val failedLeaves = failed.toVector.flatMap(_._2.exploreAndSucceed(p)._1)
      val (bLeaves, succeeds) = rest.headOption.fold(last)(_._2).exploreAndSucceed(p)
      (failedLeaves ++ bLeaves, succeeds)

  /** Applies the function to the leaves. */
  def map[R](f: Q => R): Branch[R] = flatMap(q => Leaf(f(q)))

  /** Replaces the leaves with branches, and simplifies the result. */
  def flatMap[R](f: Q => Branch[R]): Branch[R] = this match
    case Leaf(q)                          => f(q)
    case Succeed                          => Succeed
    case Choice(bs)                       => Branch.choice(bs.map(_.flatMap(f)))
    case LookAhead(negative, inner, cont) => Branch.lookAhead(negative, inner.flatMap(f), cont.flatMap(f))
    case AtomicChoice(alternatives, last) =>
      Branch.atomicChoice(alternatives.map((test, b) => (test.flatMap(f), b.flatMap(f))), last.flatMap(f))

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

  /** Builds a simplified atomic choice of the alternatives. */
  def atomicChoice[Q](alternatives: Seq[(Branch[Q], Branch[Q])], last: Branch[Q]): Branch[Q] =
    // The alternative of a test that succeeds is the last one tried, and one whose test and branch fail explores nothing.
    val (before, after) = alternatives.span(_._1 != Succeed)
    val tried = before.filter(_ != (Fail, Fail))
    val committed = after.headOption.fold(last)(_._2)
    if tried.isEmpty then committed else AtomicChoice(tried.toVector, committed)
