package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.charset.CharSet

/** IRAssertKind is a kind of zero-width assertion. */
sealed abstract class IRAssertKind extends Product with Serializable

object IRAssertKind:

  /** LineBegin is a line begin assertion.
    *
    * @param atEnd
    *   where it matches at the end of the input
    */
  final case class LineBegin(terminators: IRLineTerminators, atEnd: IRLineBeginAtEnd) extends IRAssertKind

  /** LineEnd is a line end assertion. */
  final case class LineEnd(terminators: IRLineTerminators) extends IRAssertKind

  /** TextBegin is a text begin assertion. */
  case object TextBegin extends IRAssertKind

  /** TextEnd is a text end assertion. */
  case object TextEnd extends IRAssertKind

  /** ChompTextEnd is a text end assertion that also matches before a line terminator at the end. */
  final case class ChompTextEnd(terminators: IRLineTerminators) extends IRAssertKind

  /** WordBoundary is a word boundary assertion between a code value in the word set and one not in it. */
  final case class WordBoundary(word: CharSet) extends IRAssertKind

  /** NotWordBoundary is a non-word-boundary assertion with the word set. */
  final case class NotWordBoundary(word: CharSet) extends IRAssertKind

  /** NotBetweenCRLF is an assertion that the position is not between `\r` and `\n`.
    *
    * PCRE's `.` with `(*CRLF)` has it after its character, which then is not `\r` before `\n`.
    */
  case object NotBetweenCRLF extends IRAssertKind

/** IRLineBeginAtEnd is where a line begin assertion matches at the end of the input. */
sealed abstract class IRLineBeginAtEnd extends Product with Serializable

object IRLineBeginAtEnd:

  /** AfterTerminator matches there after a line terminator too, as elsewhere (.NET, JavaScript, Python). */
  case object AfterTerminator extends IRLineBeginAtEnd

  /** InputBegin matches there only at the begin of the input, i.e. in the empty input (PCRE, Perl, Ruby). */
  case object InputBegin extends IRLineBeginAtEnd

  /** Never matches there, even in the empty input (Java). */
  case object Never extends IRLineBeginAtEnd
