package codes.quine.labs.resyntax.ir.alphabet

import scala.util.hashing.MurmurHash3

/** CharKind is a minimum character information for assertion check.
  *
  * The last three are guesses on the context, which the automaton checks with the next (resp. previous) kind.
  */
final case class CharKind(
    /** The bits of the word sets having the character. */
    words: Int = 0,
    /** The bits of the line terminator sets having the character. */
    terminators: Int = 0,
    /** The bits of the sets of the word marks having the character, three for each (the marks, the extenders and the
      * bases).
      */
    marks: Int = 0,
    /** Whether it is `\r`, which is distinguished for `\r\n`. */
    cr: Boolean = false,
    /** Whether it is `\n`, which is distinguished for `\r\n`. */
    lf: Boolean = false,
    /** Whether it is the begin or the end of the input instead of a character. */
    boundary: Boolean = false,
    /** Whether the character is the last of the input. */
    last: Option[Boolean] = None,
    /** Whether `\r` precedes the `\n`. */
    afterCR: Option[Boolean] = None,
    /** What follows the `\r`. */
    follow: Option[CRFollow] = None,
    /** The bits of the word marks whose base the text up to the character has. */
    based: Int = 0
):

  /** Checks whether it is a character of the first word set. */
  def word: Boolean = isWordOf(0)

  /** Checks whether it is a character of the `i`-th word set. */
  def isWordOf(i: Int): Boolean = (words & (1 << i)) != 0

  /** Checks whether it is a character of a line terminator set. */
  def isLineTerminator: Boolean = terminators != 0

  /** Checks whether it is a character of the `i`-th line terminator set. */
  def isLineTerminatorOf(i: Int): Boolean = (terminators & (1 << i)) != 0

  /** Checks whether it is a mark of the `i`-th word marks. */
  def isMarkOf(i: Int): Boolean = (marks & (1 << (3 * i))) != 0

  /** Checks whether it is an extender of the `i`-th word marks. */
  def isExtenderOf(i: Int): Boolean = (marks & (1 << (3 * i + 1))) != 0

  /** Checks whether it is a base of the `i`-th word marks. */
  def isBaseOf(i: Int): Boolean = (marks & (1 << (3 * i + 2))) != 0

  /** Checks whether the text up to the character has a base of the `i`-th word marks. */
  def isBasedOf(i: Int): Boolean = (based & (1 << i)) != 0

  // The named kinds hash by their names, as the enum values did, which keeps the orders of the automaton's states.
  override def hashCode: Int = this match
    case CharKind.Normal | CharKind.LineTerminator | CharKind.Word | CharKind.Boundary => toString.hashCode
    case _                                                                             => MurmurHash3.productHash(this)

  override def toString: String = this match
    case CharKind.Normal         => "Normal"
    case CharKind.LineTerminator => "LineTerminator"
    case CharKind.Word           => "Word"
    case CharKind.Boundary       => "Boundary"
    case _                       =>
      val flags = Seq(
        Option.when(words != 0)(s"words=$words"),
        Option.when(terminators != 0)(s"terminators=$terminators"),
        Option.when(marks != 0)(s"marks=$marks"),
        Option.when(cr)("cr"),
        Option.when(lf)("lf"),
        Option.when(boundary)("boundary"),
        last.map(l => s"last=$l"),
        afterCR.map(a => s"afterCR=$a"),
        follow.map(f => s"follow=$f"),
        Option.when(based != 0)(s"based=$based")
      ).flatten
      flags.mkString("CharKind(", ",", ")")

object CharKind:

  /** A character kind for normal character. */
  val Normal: CharKind = CharKind()

  /** A character kind for line terminator character (of the first set). */
  val LineTerminator: CharKind = CharKind(terminators = 1)

  /** A character kind for word character. */
  val Word: CharKind = CharKind(words = 1)

  /** The kind of the begin and the end of the input, where there is no character. */
  val Boundary: CharKind = CharKind(boundary = true)

  /** The basic kinds, which the original assertions tell about. */
  val basic: Seq[CharKind] = Seq(Normal, LineTerminator, Word)

/** CRFollow is what follows `\r`. */
enum CRFollow:

  /** Neither `\n` nor the end. */
  case Other

  /** `\n` that is not the last character. */
  case LF

  /** `\n` that is the last character. */
  case FinalLF
