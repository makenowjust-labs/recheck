package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.charset.CharSet

/** IRAssertKind is a kind of zero-width assertion. */
sealed abstract class IRAssertKind extends Product with Serializable:

  /** Checks whether it asserts the begin of a line or the text (`^` or `\A`). */
  def isBegin: Boolean = this match
    case IRAssertKind.TextBegin | _: IRAssertKind.LineBegin => true
    case _                                                  => false

  /** Checks whether it asserts the end of a line or the text (`$`, `\z` or `\Z`). */
  def isEnd: Boolean = this match
    case IRAssertKind.TextEnd | _: IRAssertKind.LineEnd | _: IRAssertKind.ChompTextEnd => true
    case _                                                                             => false

object IRAssertKind:

  /** LineBegin is a line begin assertion, which `atEnd` tells where it matches at the end of the input. */
  final case class LineBegin(terminators: IRLineTerminators, atEnd: IRLineBeginAtEnd) extends IRAssertKind

  final case class LineEnd(terminators: IRLineTerminators) extends IRAssertKind

  case object TextBegin extends IRAssertKind

  case object TextEnd extends IRAssertKind

  /** ChompTextEnd is a text end assertion that also matches before a line terminator at the end. */
  final case class ChompTextEnd(terminators: IRLineTerminators) extends IRAssertKind

  /** WordBoundary is a word boundary assertion between a code value in the word set and one not in it. */
  final case class WordBoundary(word: CharSet) extends IRAssertKind

  final case class NotWordBoundary(word: CharSet) extends IRAssertKind

  /** MarkWordBoundary is a word boundary assertion where a mark after a base is a word character too (Java's `\b`
    * without `U`).
    */
  final case class MarkWordBoundary(word: CharSet, marks: IRWordMarks) extends IRAssertKind

  final case class NotMarkWordBoundary(word: CharSet, marks: IRWordMarks) extends IRAssertKind

  /** NotBetweenCRLF is an assertion that the position is not between `\r` and `\n`.
    *
    * PCRE's `.` with `(*CRLF)` has it after its character, which then is not `\r` before `\n`.
    */
  case object NotBetweenCRLF extends IRAssertKind

/** IRWordMarks is the marks that a word boundary assertion takes as word characters after a base, as Java's
  * `Pattern.Bound` does with a non-spacing mark (`hasBaseCharacter`).
  *
  * The text before a position has a base if it ends with a character of `bases` and then any characters of `extenders`.
  * A character of `marks` after such a position is a word character, and so is a character of `extenders` before it.
  */
final case class IRWordMarks(marks: CharSet, extenders: CharSet, bases: CharSet)

/** IRLineBeginAtEnd is where a line begin assertion matches at the end of the input. */
sealed abstract class IRLineBeginAtEnd extends Product with Serializable

object IRLineBeginAtEnd:

  /** AfterTerminator matches there after a line terminator too, as elsewhere (.NET, JavaScript, Python). */
  case object AfterTerminator extends IRLineBeginAtEnd

  /** InputBegin matches there only at the begin of the input, i.e. in the empty input (PCRE, Perl, Ruby). */
  case object InputBegin extends IRLineBeginAtEnd

  /** Never matches there, even in the empty input (Java). */
  case object Never extends IRLineBeginAtEnd
