package codes.quine.labs.resyntax.ir.alphabet

/** CharKind is what the zero-width assertions inspect of a character. */
final case class CharKind(
    /** The bits of the word sets having the character. */
    words: Int = 0,
    /** The bits of the line terminator sets having the character. */
    terminators: Int = 0,
    /** The bits of the word marks whose marks have the character. */
    marks: Int = 0,
    /** The bits of the word marks whose extenders have the character. */
    extenders: Int = 0,
    /** The bits of the word marks whose bases have the character. */
    bases: Int = 0,
    /** Whether it is `\r`, which is distinguished for `\r\n`. */
    cr: Boolean = false,
    /** Whether it is `\n`, which is distinguished for `\r\n`. */
    lf: Boolean = false,
    /** Whether it is a high surrogate, which a low surrogate never follows as another character. */
    high: Boolean = false,
    /** Whether it is a low surrogate, which never follows a high surrogate as another character. */
    low: Boolean = false,
    /** Whether it is the begin or the end of the input instead of a character. */
    boundary: Boolean = false
):

  /** Checks whether it is a character of the `i`-th word set. */
  def isWordOf(i: Int): Boolean = (words & (1 << i)) != 0

  /** Checks whether it is a character of a line terminator set. */
  def isLineTerminator: Boolean = terminators != 0

  /** Checks whether it is a character of the `i`-th line terminator set. */
  def isLineTerminatorOf(i: Int): Boolean = (terminators & (1 << i)) != 0

  /** Checks whether it is a mark of the `i`-th word marks. */
  def isMarkOf(i: Int): Boolean = (marks & (1 << i)) != 0

  /** Checks whether it is an extender of the `i`-th word marks. */
  def isExtenderOf(i: Int): Boolean = (extenders & (1 << i)) != 0

object CharKind:

  /** A character kind for normal character. */
  val Normal: CharKind = CharKind()

  /** A character kind for line terminator character (of the first set). */
  val LineTerminator: CharKind = CharKind(terminators = 1)

  /** A character kind for word character. */
  val Word: CharKind = CharKind(words = 1)

  /** The kind of the begin and the end of the input, where there is no character. */
  val Boundary: CharKind = CharKind(boundary = true)
