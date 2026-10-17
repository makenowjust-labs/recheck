package codes.quine.labs.resyntax.ir

/** IRAssertKind is a kind of zero-width assertion. */
sealed abstract class IRAssertKind extends Product with Serializable

object IRAssertKind:

  /** LineBegin is a line begin assertion. */
  case object LineBegin extends IRAssertKind

  /** LineEnd is a line end assertion. */
  case object LineEnd extends IRAssertKind

  /** TextBegin is a text begin assertion. */
  case object TextBegin extends IRAssertKind

  /** TextEnd is a text end assertion. */
  case object TextEnd extends IRAssertKind

  /** ChompTextEnd is a text end assertion extended to match chomped text. */
  case object ChompTextEnd extends IRAssertKind
