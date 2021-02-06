package codes.quine.labo.recheck
package backtrack

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.data.IChar
import codes.quine.labo.recheck.data.UChar
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.Pattern._
import codes.quine.labo.recheck.util.TryUtil

/** Compiler from RegExp pattern to VM IR. */
object IRCompiler {

  /** Compiles the RegExp pattern to IR. */
  def compile(pattern: Pattern)(implicit ctx: Context): Try[IR] =
    ctx.interrupt(for {
      _ <- Try(()) // Ensures a `Try` context surely.
      capsSize = IRCompiler.capsSize(pattern)
      names <- IRCompiler.names(pattern)
      codes <- {
        import ctx._

        val FlagSet(_, ignoreCase, multiline, dotAll, unicode, _) = pattern.flagSet

        def loop(node: Node, forward: Boolean): Try[State] =
          interrupt(node match {
            case Disjunction(ns) =>
              TryUtil.traverse(ns)(loop(_, forward)).map(_.reduceRight(_.union(_)))
            case Sequence(ns) =>
              TryUtil.traverse(if (forward) ns else ns.reverse)(loop(_, forward)).map { ss =>
                ss.foldLeft(State(IndexedSeq.empty, false))(_.concat(_))
              }
            case Capture(index, n) =>
              loop(n, forward).map(State.capture(index, _, forward))
            case NamedCapture(index, _, n) =>
              loop(n, forward).map(State.capture(index, _, forward))
            case Group(n) => loop(n, forward)
            case Star(nonGreedy, n) =>
              loop(n, forward).map(State.many(nonGreedy, _))
            case Plus(nonGreedy, n) =>
              loop(n, forward).map(State.some(nonGreedy, _))
            case Question(nonGreedy, n) =>
              loop(n, forward).map(State.optional(nonGreedy, _))
            case Repeat(_, min, None, n) =>
              loop(n, forward).map(State.repeatN(min, _))
            case Repeat(nonGreedy, min, Some(None), n) =>
              loop(n, forward).map(s => State.repeatN(min, s).concat(State.many(nonGreedy, s)))
            case Repeat(_, min, Some(Some(max)), _) if min > max =>
              Failure(new InvalidRegExpException("out of order repetition quantifier"))
            case Repeat(nonGreedy, min, Some(Some(max)), n) =>
              loop(n, forward).map(s => State.repeatN(min, s).concat(State.repeatAtMost(max - min, nonGreedy, s)))
            case WordBoundary(invert) =>
              Success(State(IndexedSeq(if (invert) IR.WordBoundaryNot else IR.WordBoundary), false))
            case LineBegin() =>
              Success(State(IndexedSeq(if (multiline) IR.LineBegin else IR.InputBegin), false))
            case LineEnd() =>
              Success(State(IndexedSeq(if (multiline) IR.LineEnd else IR.InputEnd), false))
            case LookAhead(negative, n) =>
              loop(n, true).map(State.lookAround(negative, _))
            case LookBehind(negative, n) =>
              loop(n, false).map(State.lookAround(negative, _))
            case Character(c0) =>
              val c = if (ignoreCase) UChar.canonicalize(c0, unicode) else c0
              Success(State.char(IR.Char(c), forward))
            case node @ CharacterClass(invert, _) =>
              node.toIChar(unicode).map { ch0 =>
                val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
                State.char(if (invert) IR.ClassNot(ch) else IR.Class(ch), forward)
              }
            case node: AtomNode =>
              node.toIChar(unicode).map { ch0 =>
                val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
                State.char(IR.Class(ch), forward)
              }
            case Dot() =>
              Success(State.char(if (dotAll) IR.Any else IR.Dot, forward))
            case BackReference(i) =>
              if (i <= 0 || capsSize < i) Failure(new InvalidRegExpException("invalid back-reference"))
              else Success(State(IndexedSeq(if (forward) IR.Ref(i) else IR.RefBack(i)), false))
            case NamedBackReference(name) =>
              names.get(name) match {
                case Some(i) =>
                  Success(State(IndexedSeq(if (forward) IR.Ref(i) else IR.RefBack(i)), false))
                case None => Failure(new InvalidRegExpException("invalid named back-reference"))
              }
          })

        loop(pattern.node, true).map(State.prelude(pattern.hasLineBeginAtBegin, _))
      }
    } yield IR(capsSize, names, codes))

  /** State is a compiling state. */
  private[backtrack] final case class State(codes: IndexedSeq[IR.OpCode], advance: Boolean) {

    /** Computes a union of two states. */
    def union(that: State): State =
      State(
        IndexedSeq(IR.ForkCont(codes.size + 1)) ++ codes ++ IndexedSeq(IR.Jump(that.codes.size)) ++ that.codes,
        advance && that.advance
      )

    /** Computes a concatenation of two states. */
    def concat(that: State): State =
      State(codes ++ that.codes, advance || that.advance)

    /** Computes a capture range of this state's IR op-codes. */
    def captureRange: Option[(Int, Int)] = {
      val captures = codes.collect {
        case IR.CapBegin(i) => i
        case IR.CapEnd(i)   => i
      }
      if (captures.nonEmpty) Some((captures.min, captures.max))
      else None
    }
  }

  /** State utilities. */
  private[backtrack] object State {

    /** Wraps a state op-codes in prelude codes. */
    def prelude(hasLineBeginAtBegin: Boolean, state: State): IndexedSeq[IR.OpCode] = {
      val begin =
        if (!hasLineBeginAtBegin) IndexedSeq(IR.ForkNext(2), IR.Any, IR.Jump(-3), IR.CapBegin(0))
        else IndexedSeq(IR.CapBegin(0))
      val end = IndexedSeq(IR.CapEnd(0), IR.Done)
      begin ++ state.codes ++ end
    }

    /** Wraps a state as a capture. */
    def capture(index: Int, state: State, forward: Boolean): State =
      State(
        if (forward) IndexedSeq(IR.CapBegin(index)) ++ state.codes ++ IndexedSeq(IR.CapEnd(index))
        else IndexedSeq(IR.CapEnd(index)) ++ state.codes ++ IndexedSeq(IR.CapBegin(index)),
        state.advance
      )

    /** Wraps a state as many repetition. */
    def many(nonGreedy: Boolean, state: State): State = {
      val codes = setupLoop(state)
      State(
        IndexedSeq(if (nonGreedy) IR.ForkNext(codes.size + 1) else IR.ForkCont(codes.size + 1)) ++
          codes ++
          IndexedSeq(IR.Jump(-1 - codes.size - 1)),
        false
      )
    }

    /** Wraps a state as some repetition. */
    def some(nonGreedy: Boolean, state: State): State = {
      val codes = setupLoop(state)
      State(
        state.codes ++
          IndexedSeq(if (nonGreedy) IR.ForkNext(codes.size + 1) else IR.ForkCont(codes.size + 1)) ++
          codes ++
          IndexedSeq(IR.Jump(-1 - codes.size - 1)),
        state.advance
      )
    }

    /** Wraps a state as optional repetition. */
    def optional(nonGreedy: Boolean, state: State): State =
      // Here, `setupLoop(state)` is not needed, because problematic capture is reset by outer repetition.
      State(
        IndexedSeq(if (nonGreedy) IR.ForkNext(state.codes.size) else IR.ForkCont(state.codes.size)) ++ state.codes,
        false
      )

    /** Wraps a state as `n`-times repetition. */
    def repeatN(n: Int, state: State): State = n match {
      case 0 => State(IndexedSeq.empty, false)
      case 1 => state
      case _ =>
        State(
          IndexedSeq(IR.PushCnt(n)) ++ state.codes ++ IndexedSeq(IR.Dec, IR.Loop(-1 - state.codes.size - 1), IR.PopCnt),
          state.advance
        )
    }

    /** Wraps a state as at most `n`-times repetition. */
    def repeatAtMost(n: Int, nonGreedy: Boolean, state: State): State = n match {
      case 0 => State(IndexedSeq.empty, false)
      case 1 => optional(nonGreedy, state)
      case n =>
        val codes = setupLoop(state)
        State(
          IndexedSeq(IR.PushCnt(n), if (nonGreedy) IR.ForkNext(codes.size + 2) else IR.ForkCont(codes.size + 2)) ++
            codes ++
            IndexedSeq(IR.Dec, IR.Loop(-1 - codes.size - 2), IR.PopCnt),
          false
        )
    }

    /** Sets up state op-codes for a loop. */
    def setupLoop(state: State): IndexedSeq[IR.OpCode] = {
      val codes = if (state.advance) state.codes else IndexedSeq(IR.PushPos) ++ state.codes ++ IndexedSeq(IR.EmptyCheck)
      state.captureRange match {
        case Some((min, max)) => IndexedSeq(IR.CapReset(min, max)) ++ codes
        case None             => codes
      }
    }

    /** Wraps a state as a look-around. */
    def lookAround(negative: Boolean, state: State): State = {
      State(
        if (negative)
          IndexedSeq(IR.PushPos, IR.PushProc, IR.ForkCont(state.codes.size + 2)) ++ state.codes ++ IndexedSeq(
            IR.RewindProc,
            IR.Fail,
            IR.PopProc,
            IR.RestorePos
          )
        else IndexedSeq(IR.PushPos, IR.PushProc) ++ state.codes ++ IndexedSeq(IR.RewindProc, IR.RestorePos),
        false
      )
    }

    /** Creates a state with a character op-codes. */
    def char(code: IR.OpCode, forward: Boolean): State =
      State(if (forward) IndexedSeq(code) else IndexedSeq(IR.Back, code, IR.Back), true)
  }

  /** Computes a size of captures of the pattern. */
  private[backtrack] def capsSize(pattern: Pattern): Int = {
    def loop(node: Node): Int = node match {
      case Disjunction(ns)       => ns.map(loop).foldLeft(0)(Math.max)
      case Sequence(ns)          => ns.map(loop).foldLeft(0)(Math.max)
      case Capture(i, n)         => Math.max(i, loop(n))
      case NamedCapture(i, _, n) => Math.max(i, loop(n))
      case Group(n)              => loop(n)
      case Star(_, n)            => loop(n)
      case Plus(_, n)            => loop(n)
      case Question(_, n)        => loop(n)
      case Repeat(_, _, _, n)    => loop(n)
      case LookAhead(_, n)       => loop(n)
      case LookBehind(_, n)      => loop(n)
      case _                     => 0
    }

    loop(pattern.node)
  }

  /** Extracts capture names from the pattern. */
  private[backtrack] def names(pattern: Pattern): Try[Map[String, Int]] = {
    def merge(tm1: Try[Map[String, Int]], m2: Map[String, Int]): Try[Map[String, Int]] =
      tm1.flatMap { m1 =>
        if (m1.keySet.intersect(m2.keySet).nonEmpty) Failure(new InvalidRegExpException("duplicated named capture"))
        else Success(m1 ++ m2)
      }

    def loop(node: Node): Try[Map[String, Int]] = node match {
      case Disjunction(ns) =>
        TryUtil.traverse(ns)(loop).flatMap(_.foldLeft(Try(Map.empty[String, Int]))(merge))
      case Sequence(ns) =>
        TryUtil.traverse(ns)(loop).flatMap(_.foldLeft(Try(Map.empty[String, Int]))(merge))
      case Capture(_, n)                => loop(n)
      case NamedCapture(index, name, n) => loop(n).map(_ + (name -> index))
      case Group(n)                     => loop(n)
      case Star(_, n)                   => loop(n)
      case Plus(_, n)                   => loop(n)
      case Question(_, n)               => loop(n)
      case Repeat(_, _, _, n)           => loop(n)
      case LookAhead(_, n)              => loop(n)
      case LookBehind(_, n)             => loop(n)
      case _                            => Success(Map.empty)
    }

    loop(pattern.node)
  }
}
