package codes.quine.labs.resyntax.ast

/** AssertNameStyle is a style kind for some groups. */
sealed abstract class AssertNameStyle extends Product with Serializable

object AssertNameStyle:

  /** Symbolic is a symbolic style (e.g. `(?=...)`). */
  case object Symbolic extends AssertNameStyle

  /** Alphabetic is a alphabetic style (e.g. `(?positive_lookahead:...)`). */
  case object Alphabetic extends AssertNameStyle

  /** Abbrev is a abbrev alphabetic style (e.g. `(?pla:...)`). */
  case object Abbrev extends AssertNameStyle
