package codes.quine.labs.recheck
package automaton

import scala.util.hashing.MurmurHash3

import codes.quine.labs.resyntax.ir.alphabet.CharKind

/** CharKindState is the state of the text around a position that the zero-width assertions inspect.
  *
  * It has the previous character's kind and the facts behind the position, which follow from the characters read.
  *
  * It also has the guesses on what follows the character, which the next character confirms.
  */
final case class CharKindState(
    /** The kind of the character before the position, or that of the begin of the input. */
    kind: CharKind,
    /** Whether the character is `\n` after `\r`. */
    afterCR: Boolean = false,
    /** The bits of the word marks whose base the text ends with, followed by any extenders. */
    based: Int = 0,
    /** The guess whether the character is the last of the input. */
    last: Option[Boolean] = None,
    /** The guess on what follows the character `\r`. */
    follow: Option[CRFollow] = None
):

  /** Checks whether the text up to the position has a base of the `i`-th word marks. */
  def isBasedOf(i: Int): Boolean = (based & (1 << i)) != 0

  /** Returns the states after a character of the kind.
    *
    * Their facts behind follow from this state, and they tell `\n` after `\r` only with `afterCRLF`.
    *
    * They guess whether the character is the last of the input with `last`, and what follows `\r` with `follow`.
    */
  def advance(next: CharKind, afterCRLF: Boolean, last: Boolean, follow: Boolean): Seq[CharKindState] =
    val state = CharKindState(next, afterCRLF && next.lf && kind.cr, next.bases | (next.extenders & based))
    val lasts =
      if last && (next.isLineTerminator || next.cr || next.lf) then Seq(Some(false), Some(true)) else Seq(None)
    val follows =
      if follow && next.cr then (Seq(CRFollow.Other, CRFollow.LF) ++ Option.when(last)(CRFollow.FinalLF)).map(Some(_))
      else Seq(None)
    for l <- lasts; f <- follows yield state.copy(last = l, follow = f)

  /** Tests whether the next state, or the end of the input, confirms the guesses on what follows the character. */
  def admits(next: CharKindState): Boolean =
    last.forall(_ == next.kind.boundary) && follow.forall:
      case CRFollow.Other   => !next.kind.lf
      case CRFollow.LF      => next.kind.lf && !next.last.contains(true)
      case CRFollow.FinalLF => next.kind.lf && next.last.contains(true)

  /** Forgets the marks of the character except an extender after a base, the only one that later assertions inspect. */
  def forgetMarks: CharKindState =
    copy(kind = kind.copy(marks = 0, extenders = kind.extenders & based, bases = 0))

  // A state with only the kind hashes as the kind, which keeps the orders of the automaton's states.
  override def hashCode: Int =
    if !afterCR && based == 0 && last.isEmpty && follow.isEmpty then kind.hashCode
    else MurmurHash3.caseClassHash(this)

/** CRFollow is what follows `\r`. */
enum CRFollow:

  /** Not `\n`, or the end of the input. */
  case Other

  /** `\n` that is not the last character. */
  case LF

  /** `\n` that is the last character. */
  case FinalLF
