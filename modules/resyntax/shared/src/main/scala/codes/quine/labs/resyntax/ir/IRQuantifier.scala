package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.BacktrackStrategy

sealed abstract class IRQuantifier extends Product with Serializable

object IRQuantifier:

  final case class Exact(n: Int) extends IRQuantifier

  final case class Bounded(min: Int, max: Int, strategy: BacktrackStrategy) extends IRQuantifier

  final case class Unbounded(min: Int, strategy: BacktrackStrategy) extends IRQuantifier
