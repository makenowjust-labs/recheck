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

        def loop(node: Node, forward: Boolean): Try[IRBlock] =
          interrupt(node match {
            case Disjunction(ns) =>
              TryUtil.traverse(ns)(loop(_, forward)).map(_.reduceRight(_.union(_)))
            case Sequence(ns) =>
              TryUtil.traverse(if (forward) ns else ns.reverse)(loop(_, forward)).map { ss =>
                ss.foldLeft(IRBlock(IndexedSeq.empty, false))(_.concat(_))
              }
            case Capture(index, n) =>
              loop(n, forward).map(IRBlock.capture(index, _, forward))
            case NamedCapture(index, _, n) =>
              loop(n, forward).map(IRBlock.capture(index, _, forward))
            case Group(n) => loop(n, forward)
            case Star(nonGreedy, n) =>
              loop(n, forward).map(IRBlock.many(nonGreedy, _))
            case Plus(nonGreedy, n) =>
              loop(n, forward).map(IRBlock.some(nonGreedy, _))
            case Question(nonGreedy, n) =>
              loop(n, forward).map(IRBlock.optional(nonGreedy, _))
            case Repeat(_, min, None, n) =>
              loop(n, forward).map(IRBlock.repeatN(min, _))
            case Repeat(nonGreedy, min, Some(None), n) =>
              loop(n, forward).map(s => IRBlock.repeatN(min, s).concat(IRBlock.many(nonGreedy, s)))
            case Repeat(_, min, Some(Some(max)), _) if min > max =>
              Failure(new InvalidRegExpException("out of order repetition quantifier"))
            case Repeat(nonGreedy, min, Some(Some(max)), n) =>
              loop(n, forward).map(s => IRBlock.repeatN(min, s).concat(IRBlock.repeatAtMost(max - min, nonGreedy, s)))
            case WordBoundary(invert) =>
              Success(IRBlock(IndexedSeq(if (invert) IR.WordBoundaryNot else IR.WordBoundary), false))
            case LineBegin() =>
              Success(IRBlock(IndexedSeq(if (multiline) IR.LineBegin else IR.InputBegin), false))
            case LineEnd() =>
              Success(IRBlock(IndexedSeq(if (multiline) IR.LineEnd else IR.InputEnd), false))
            case LookAhead(negative, n) =>
              loop(n, true).map(IRBlock.lookAround(negative, _))
            case LookBehind(negative, n) =>
              loop(n, false).map(IRBlock.lookAround(negative, _))
            case Character(c0) =>
              val c = if (ignoreCase) UChar.canonicalize(c0, unicode) else c0
              Success(IRBlock.char(IR.Char(c, node.pos), forward))
            case node @ CharacterClass(invert, _) =>
              node.toIChar(unicode).map { ch0 =>
                val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
                IRBlock.char(if (invert) IR.ClassNot(ch, node.pos) else IR.Class(ch, node.pos), forward)
              }
            case node: AtomNode =>
              node.toIChar(unicode).map { ch0 =>
                val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
                IRBlock.char(IR.Class(ch, node.pos), forward)
              }
            case Dot() =>
              Success(IRBlock.char(if (dotAll) IR.Any(node.pos) else IR.Dot(node.pos), forward))
            case BackReference(i) =>
              if (i <= 0 || capsSize < i) Failure(new InvalidRegExpException("invalid back-reference"))
              else Success(IRBlock(IndexedSeq(if (forward) IR.Ref(i, node.pos) else IR.RefBack(i, node.pos)), false))
            case NamedBackReference(name) =>
              names.get(name) match {
                case Some(i) =>
                  Success(IRBlock(IndexedSeq(if (forward) IR.Ref(i, node.pos) else IR.RefBack(i, node.pos)), false))
                case None => Failure(new InvalidRegExpException("invalid named back-reference"))
              }
          })

        loop(pattern.node, true).map(IRBlock.prelude(pattern.hasLineBeginAtBegin, _))
      }
    } yield IR(capsSize, names, codes))

  /** IRBlock is an IR code block. It is also a compiling state. */
  private[backtrack] final case class IRBlock(codes: IndexedSeq[IR.OpCode], advance: Boolean) {

    /** Computes a union of two blocks. */
    def union(that: IRBlock): IRBlock =
      IRBlock(
        IndexedSeq(IR.ForkCont(codes.size + 1)) ++ codes ++ IndexedSeq(IR.Jump(that.codes.size)) ++ that.codes,
        advance && that.advance
      )

    /** Computes a concatenation of two blocks. */
    def concat(that: IRBlock): IRBlock =
      IRBlock(codes ++ that.codes, advance || that.advance)

    /** Computes a capture range of this block's IR op-codes. */
    def captureRange: Option[(Int, Int)] = {
      val captures = codes.collect {
        case IR.CapBegin(i) => i
        case IR.CapEnd(i)   => i
      }
      if (captures.nonEmpty) Some((captures.min, captures.max))
      else None
    }
  }

  /** IRBlock utilities. */
  private[backtrack] object IRBlock {

    /** Wraps a block op-codes in prelude codes. */
    def prelude(hasLineBeginAtBegin: Boolean, block: IRBlock): IndexedSeq[IR.OpCode] = {
      val begin =
        if (!hasLineBeginAtBegin) IndexedSeq(IR.ForkNext(2), IR.Any(), IR.Jump(-3), IR.CapBegin(0))
        else IndexedSeq(IR.CapBegin(0))
      val end = IndexedSeq(IR.CapEnd(0), IR.Done)
      begin ++ block.codes ++ end
    }

    /** Wraps a block as a capture. */
    def capture(index: Int, block: IRBlock, forward: Boolean): IRBlock =
      IRBlock(
        if (forward) IndexedSeq(IR.CapBegin(index)) ++ block.codes ++ IndexedSeq(IR.CapEnd(index))
        else IndexedSeq(IR.CapEnd(index)) ++ block.codes ++ IndexedSeq(IR.CapBegin(index)),
        block.advance
      )

    /** Wraps a block as many repetition. */
    def many(nonGreedy: Boolean, block: IRBlock): IRBlock = {
      val codes = setupLoop(block)
      IRBlock(
        IndexedSeq(if (nonGreedy) IR.ForkNext(codes.size + 1) else IR.ForkCont(codes.size + 1)) ++
          codes ++
          IndexedSeq(IR.Jump(-1 - codes.size - 1)),
        false
      )
    }

    /** Wraps a block as some repetition. */
    def some(nonGreedy: Boolean, block: IRBlock): IRBlock = {
      val codes = setupLoop(block)
      IRBlock(
        block.codes ++
          IndexedSeq(if (nonGreedy) IR.ForkNext(codes.size + 1) else IR.ForkCont(codes.size + 1)) ++
          codes ++
          IndexedSeq(IR.Jump(-1 - codes.size - 1)),
        block.advance
      )
    }

    /** Wraps a block as optional repetition. */
    def optional(nonGreedy: Boolean, block: IRBlock): IRBlock =
      // Here, `setupLoop(state)` is not needed, because problematic capture is reset by outer repetition.
      IRBlock(
        IndexedSeq(if (nonGreedy) IR.ForkNext(block.codes.size) else IR.ForkCont(block.codes.size)) ++ block.codes,
        false
      )

    /** Wraps a block as `n`-times repetition. */
    def repeatN(n: Int, block: IRBlock): IRBlock = n match {
      case 0 => IRBlock(IndexedSeq.empty, false)
      case 1 => block
      case _ =>
        IRBlock(
          IndexedSeq(IR.PushCnt(n)) ++ block.codes ++ IndexedSeq(IR.Dec, IR.Loop(-1 - block.codes.size - 1), IR.PopCnt),
          block.advance
        )
    }

    /** Wraps a block as at most `n`-times repetition. */
    def repeatAtMost(n: Int, nonGreedy: Boolean, block: IRBlock): IRBlock = n match {
      case 0 => IRBlock(IndexedSeq.empty, false)
      case 1 => optional(nonGreedy, block)
      case n =>
        val codes = setupLoop(block)
        IRBlock(
          IndexedSeq(IR.PushCnt(n), if (nonGreedy) IR.ForkNext(codes.size + 2) else IR.ForkCont(codes.size + 2)) ++
            codes ++
            IndexedSeq(IR.Dec, IR.Loop(-1 - codes.size - 2), IR.PopCnt),
          false
        )
    }

    /** Sets up block op-codes for a loop. */
    def setupLoop(block: IRBlock): IndexedSeq[IR.OpCode] = {
      val codes = if (block.advance) block.codes else IndexedSeq(IR.PushPos) ++ block.codes ++ IndexedSeq(IR.EmptyCheck)
      block.captureRange match {
        case Some((min, max)) => IndexedSeq(IR.CapReset(min, max)) ++ codes
        case None             => codes
      }
    }

    /** Wraps a block as a look-around. */
    def lookAround(negative: Boolean, block: IRBlock): IRBlock = {
      IRBlock(
        if (negative)
          IndexedSeq(IR.PushPos, IR.PushProc, IR.ForkCont(block.codes.size + 2)) ++ block.codes ++ IndexedSeq(
            IR.RewindProc,
            IR.Fail,
            IR.PopProc,
            IR.RestorePos
          )
        else IndexedSeq(IR.PushPos, IR.PushProc) ++ block.codes ++ IndexedSeq(IR.RewindProc, IR.RestorePos),
        false
      )
    }

    /** Creates a block with a character op-codes. */
    def char(code: IR.OpCode, forward: Boolean): IRBlock =
      IRBlock(if (forward) IndexedSeq(code) else IndexedSeq(IR.Back, code, IR.Back), true)
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
