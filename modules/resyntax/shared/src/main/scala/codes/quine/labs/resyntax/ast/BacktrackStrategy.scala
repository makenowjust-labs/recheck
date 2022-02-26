package codes.quine.labs.resyntax.ast

/** BacktrackStrategy is a backtracking strategy for repeat quantifiers.
  *
  * {{{
  * BacktrackStrategy ::=
  *   ""
  *   "?"
  *   "+"
  * }}}
  */
sealed abstract class BacktrackStrategy extends Product with Serializable

object BacktrackStrategy {

  /** Greedy is the default backtracking strategy. */
  case object Greedy extends BacktrackStrategy

  /** Lazy is a lazy backtracking strategy (e.g. `*?`). */
  case object Lazy extends BacktrackStrategy

  /** Possessive is a possessive backtracking strategy (e.g. `*+`). */
  case object Possessive extends BacktrackStrategy
}
