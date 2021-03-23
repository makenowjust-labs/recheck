package codes.quine.labo.recheck.vm

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.IChar.LineTerminator
import codes.quine.labo.recheck.data.IChar.Word
import codes.quine.labo.recheck.data.UChar
import codes.quine.labo.recheck.data.UString
import codes.quine.labo.recheck.vm.Inst.AssertKind
import codes.quine.labo.recheck.vm.Inst.ReadKind

/** Interpreter is an interpreter of a program and an input. */
class Interpreter(program: Program, input: UString)(implicit ctx: Context) {

  /** Frame is a stack frame. */
  class Frame(
      var label: Label,
      var pos: Int,
      var captures: Vector[Int],
      var counters: Vector[Int],
      var canaries: Vector[Int],
      var rollback: Option[Frame],
      var fallback: Option[Frame]
  ) {

    /** Gets a previous character. */
    def previousChar: Option[UChar] = input.get(pos - 1)

    /** Gets a current character. */
    def currentChar: Option[UChar] = input.get(pos)

    /** Captures a beginning position of the index. */
    def capBegin(index: Int): Unit = {
      captures = captures.updated(index * 2, pos)
    }

    /** Captures an ending position of the index. */
    def capEnd(index: Int): Unit = {
      captures = captures.updated(index * 2, pos)
    }

    /** Resets captures between `from` and `to`. */
    def capReset(from: Int, to: Int): Unit = {
      for (index <- from to to) {
        captures = captures.updated(index * 2, -1)
        captures = captures.updated(index * 2 + 1, -1)
      }
    }

    /** Returns a capture string. */
    def capture(index: Int): Option[UString] =
      (captures(index * 2), captures(index * 2 + 1)) match {
        case (-1, _)      => None
        case (_, -1)      => None
        case (begin, end) => Some(input.substring(begin, end))
      }

    override def clone(): Frame = new Frame(label, pos, captures, counters, canaries, rollback, fallback)

    override def toString: String = s"Frame($label, $pos, $captures, $counters, $canaries, $rollback, $fallback)"
  }

  /** Runs matching from a position. */
  def run(pos: Int): Boolean = {
    var frame = new Frame(
      program.blocks.head._1,
      pos,
      Vector.fill((program.meta.capturesSize + 1) * 2)(0),
      Vector.fill(program.meta.countersSize)(0),
      Vector.fill(program.meta.canariesSize)(0),
      None,
      None
    )

    while (true) ctx.interrupt {
      if (block(frame, frame.label.block)) {
        frame.label.block.terminator match {
          case Inst.Ok => return true
          case Inst.Jmp(next) =>
            frame.label = next
          case Inst.Try(next, fallback) =>
            val fallbackFrame = frame.clone()
            fallbackFrame.label = fallback
            frame.label = next
            frame.fallback = Some(fallbackFrame)
          case Inst.Cmp(reg, n, lt, ge) =>
            val r = frame.counters(reg.index)
            val next = if (r < n) lt else ge
            frame.label = next
          case Inst.Rollback =>
            frame.rollback match {
              case Some(rollback) =>
                frame = rollback
              case None => sys.error("unreachable")
            }
          case Inst.Tx(next, rollback, fallback) =>
            val rollbackFrame = rollback match {
              case Some(rollback) =>
                val rollbackFrame = frame.clone()
                rollbackFrame.label = rollback
                Some(rollbackFrame)
              case None => frame.fallback
            }
            val fallbackFrame = fallback match {
              case Some(fallback) =>
                val fallbackFrame = frame.clone()
                fallbackFrame.label = fallback
                Some(fallbackFrame)
              case None => frame.fallback
            }
            frame.label = next
            frame.rollback = rollbackFrame
            frame.fallback = fallbackFrame
        }
      } else {
        frame.fallback match {
          case Some(fallback) =>
            frame = fallback
          case None => return false
        }
      }
    }

    sys.error("unreachable")
  }

  /** Runs a given block. */
  def block(frame: Frame, block: Block): Boolean = {
    val it = block.insts.iterator
    while (it.hasNext) {
      if (!inst(frame, it.next())) return false
    }
    true
  }

  /** Runs a given instruction. */
  def inst(frame: Frame, inst: Inst.NonTerminator): Boolean =
    inst match {
      case Inst.SetCanary(reg) =>
        frame.canaries = frame.canaries.updated(reg.index, frame.pos)
        true
      case Inst.CheckCanary(reg) =>
        frame.canaries(reg.index) != frame.pos
      case Inst.Reset(reg) =>
        frame.counters = frame.counters.updated(reg.index, 0)
        true
      case Inst.Inc(reg) =>
        frame.counters = frame.counters.updated(reg.index, frame.counters(reg.index) + 1)
        true
      case Inst.Assert(kind) =>
        kind match {
          case AssertKind.WordBoundary =>
            val c1 = frame.previousChar
            val c2 = frame.currentChar
            val w1 = c1.exists(Word.contains)
            val w2 = c2.exists(Word.contains)
            w1 && !w2 || !w1 && w2
          case AssertKind.WordBoundaryNot =>
            val c1 = frame.previousChar
            val c2 = frame.currentChar
            val w1 = c1.exists(Word.contains)
            val w2 = c2.exists(Word.contains)
            !(w1 && !w2 || !w1 && w2)
          case AssertKind.LineBegin =>
            val c1 = frame.previousChar
            c1.forall(LineTerminator.contains)
          case AssertKind.LineEnd =>
            val c2 = frame.currentChar
            c2.forall(LineTerminator.contains)
          case AssertKind.InputBegin =>
            frame.pos == 0
          case AssertKind.InputEnd =>
            frame.pos == input.size
        }
      case Inst.Read(ReadKind.Ref(index), _) =>
        val s = frame.capture(index) match {
          case Some(s) => s
          case None    => UString.empty
        }
        if (s == input.substring(frame.pos, frame.pos + s.size)) {
          frame.pos += s.size
          true
        } else false
      case Inst.Read(kind, _) =>
        if (read(frame.currentChar, kind)) {
          frame.pos += 1
          true
        } else false
      case Inst.ReadBack(ReadKind.Ref(index), _) =>
        val s = frame.capture(index) match {
          case Some(s) => s
          case None    => UString.empty
        }
        if (s == input.substring(frame.pos - s.size, frame.pos)) {
          frame.pos -= s.size
          true
        } else false
      case Inst.ReadBack(kind, _) =>
        if (read(frame.previousChar, kind)) {
          frame.pos -= 1
          true
        } else false
      case Inst.CapBegin(index) =>
        frame.capBegin(index)
        true
      case Inst.CapEnd(index) =>
        frame.capEnd(index)
        true
      case Inst.CapReset(from, to) =>
        frame.capReset(from, to)
        true
    }

  /** Checks to match `read` instruction. */
  def read(c: Option[UChar], kind: ReadKind): Boolean =
    c match {
      case Some(c) =>
        kind match {
          case ReadKind.Any         => true
          case ReadKind.Dot         => !LineTerminator.contains(c)
          case ReadKind.Char(d)     => c == d
          case ReadKind.Class(s)    => s.contains(c)
          case ReadKind.ClassNot(s) => !s.contains(c)
          case ReadKind.Ref(_)      => sys.error("unreachable")
        }
      case None => false
    }
}
