package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.BacktrackStrategy

sealed abstract class IRQuantifier extends Product with Serializable

object IRQuantifier {

  /** Exact is a exact number bounded quantifier. */
  final case class Exact(n: Int) extends IRQuantifier

  /** Bounded is a bounded quantifier. */
  final case class Bounded(min: Int, max: Int, strategy: BacktrackStrategy) extends IRQuantifier

  /** Unbounded is a unbounded quantifier. */
  final case class Unbounded(min: Int, strategy: BacktrackStrategy) extends IRQuantifier
}
