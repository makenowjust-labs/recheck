package codes.quine.labs.resyntax.ir.alphabet

import scala.util.hashing.MurmurHash3

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
    /** Whether it is the begin or the end of the input instead of a character. */
    boundary: Boolean = false
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
  def isMarkOf(i: Int): Boolean = (marks & (1 << i)) != 0

  /** Checks whether it is an extender of the `i`-th word marks. */
  def isExtenderOf(i: Int): Boolean = (extenders & (1 << i)) != 0

  /** Checks whether it is a base of the `i`-th word marks. */
  def isBaseOf(i: Int): Boolean = (bases & (1 << i)) != 0

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
        Option.when(extenders != 0)(s"extenders=$extenders"),
        Option.when(bases != 0)(s"bases=$bases"),
        Option.when(cr)("cr"),
        Option.when(lf)("lf"),
        Option.when(boundary)("boundary")
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
