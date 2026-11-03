package codes.quine.labs.resyntax.ast

sealed abstract class AssertNameStyle extends Product with Serializable

object AssertNameStyle:

  /** Symbolic is a symbolic style (e.g. `(?=...)`). */
  case object Symbolic extends AssertNameStyle

  /** Alphabetic is an alphabetic style (e.g. `(?positive_lookahead:...)`). */
  case object Alphabetic extends AssertNameStyle

  /** Abbrev is an abbreviated alphabetic style (e.g. `(?pla:...)`). */
  case object Abbrev extends AssertNameStyle
