package codes.quine.labs.recheck
package automaton

/** MarkedStep is a leaf of a transition with the marks of the captures before it (see [[OrderedNFA.markedDelta]]). */
enum MarkedStep[+Q]:

  /** Next goes to the state. */
  case Next(marks: Seq[CaptureMark], to: Q)

  /** End ends the path, which is the inner expression of a look-around, or the match at the end of the input. */
  case End(marks: Seq[CaptureMark])

  /** LookBehind ends the path by a look-behind assertion, whose inner expression succeeds from the initial state
    * `inner` of [[OrderedNFA.LookBehind.inner]].
    */
  case LookBehind(marks: Seq[CaptureMark], inner: Int)

  /** The marks before the step. */
  def marks: Seq[CaptureMark]

  /** Tests whether the step succeeds when exactly the states satisfying `p` succeed. */
  def succeeds(p: Q => Boolean): Boolean = this match
    case Next(_, q) => p(q)
    case _          => true

  /** Applies the function to the state. */
  def map[R](f: Q => R): MarkedStep[R] = this match
    case Next(marks, q)           => Next(marks, f(q))
    case End(marks)               => End(marks)
    case LookBehind(marks, inner) => LookBehind(marks, inner)
