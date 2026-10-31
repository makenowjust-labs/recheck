package codes.quine.labs.resyntax.ir

/** IRAssertKind is a kind of zero-width assertion. */
sealed abstract class IRAssertKind extends Product with Serializable

object IRAssertKind:

  /** LineBegin is a line begin assertion.
    *
    * @param matchesAfterFinalTerminator
    *   whether it matches after a line terminator at the end of the input
    */
  final case class LineBegin(terminators: IRLineTerminators, matchesAfterFinalTerminator: Boolean) extends IRAssertKind

  /** LineEnd is a line end assertion. */
  final case class LineEnd(terminators: IRLineTerminators) extends IRAssertKind

  /** TextBegin is a text begin assertion. */
  case object TextBegin extends IRAssertKind

  /** TextEnd is a text end assertion. */
  case object TextEnd extends IRAssertKind

  /** ChompTextEnd is a text end assertion that also matches before a line terminator at the end. */
  final case class ChompTextEnd(terminators: IRLineTerminators) extends IRAssertKind

  /** WordBoundary is a word boundary assertion, whose word characters depend on the flags. */
  final case class WordBoundary(flags: IRCharFlags) extends IRAssertKind

  /** NotWordBoundary is a non-word-boundary assertion, whose word characters depend on the flags. */
  final case class NotWordBoundary(flags: IRCharFlags) extends IRAssertKind
