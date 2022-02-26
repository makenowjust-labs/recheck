package codes.quine.labs.resyntax.ast

/** Quantifier is a repetition quantifier node.
  *
  * {{{
  * Quantifier ::=
  *   "*" BacktrackStrategy
  *   "+" BacktrackStrategy
  *   "?" BacktrackStrategy
  *   "{" Int "}" BacktrackStrategy
  *   "{" Int "," Int "}" BacktrackStrategy
  *   "{" Int ",}" BacktrackStrategy
  *   "{," Int "}" BacktrackStrategy
  * }}}
  */
sealed abstract class Quantifier extends Product with Serializable {
  def strategy: BacktrackStrategy
}

object Quantifier {

  /** Star is a star quantifier (e.g. `*`). */
  final case class Star(strategy: BacktrackStrategy) extends Quantifier

  /** Plus is a plus quantifier (e.g. `+`). */
  final case class Plus(strategy: BacktrackStrategy) extends Quantifier

  /** Question is a question quantifier (e.g. `?`). */
  final case class Question(strategy: BacktrackStrategy) extends Quantifier

  /** Exact is a exact number bounded quantifier (e.g. `{2}`). */
  final case class Exact(n: Int, strategy: BacktrackStrategy) extends Quantifier

  /** Bounded is a bounded quantifier (e.g. `{2,3}`). */
  final case class Bounded(min: Int, max: Int, strategy: BacktrackStrategy) extends Quantifier

  /** MaxBounded is an alternative bounded quantifier (e.g. `{,3}`). */
  final case class MaxBounded(max: Int, strategy: BacktrackStrategy) extends Quantifier

  /** Unbounded is a unbounded quantifier (e.g. `{2,}`). */
  final case class Unbounded(min: Int, strategy: BacktrackStrategy) extends Quantifier
}
