package codes.quine.labo.redos
package backtrack

import scala.collection.mutable

import IR._
import common.Context
import data.IChar.LineTerminator
import data.IChar.Word
import data.UChar
import data.UString

/** VM utilities. */
object VM {

  /** Executes the IR on the input from the initial position.
    *
    * Note that the input string should be canonical.
    */
  def execute(ir: IR, input: UString, pos: Int, tracer: Tracer = Tracer.NoTracer)(implicit
      ctx: Context
  ): Option[Match] =
    ctx.interrupt(new VM(ir, input, pos, tracer).execute())
}

/** VM is a RegExp matching VM. */
private[backtrack] final class VM(
    private[this] val ir: IR,
    private[this] val input: UString,
    initPos: Int,
    private[this] val tracer: Tracer = Tracer.NoTracer
)(implicit private[this] val ctx: Context) {

  import ctx._

  /** An execution process list (stack). */
  private[backtrack] val procs: mutable.Stack[Proc] = {
    val proc = new Proc(
      IndexedSeq.fill((ir.capsSize + 1) * 2)(-1),
      Seq.empty,
      Seq.empty,
      Seq.empty,
      initPos,
      0
    )
    mutable.Stack(proc)
  }

  /** Executes this VM. */
  def execute(): Option[Match] = interrupt {
    while (procs.nonEmpty)
      step() match {
        case Some(m) => return Some(m)
        case None    => ()
      }
    None
  }

  /** Runs this VM in a step. */
  private[backtrack] def step(): Option[Match] = interrupt {
    val proc = procs.top
    val code = ir.codes(proc.pc)
    var backtrack = false

    // Saves pos and pc for tracing.
    val oldPos = proc.pos
    val oldPc = proc.pc

    // Steps forward pc before execution.
    proc.pc += 1

    code match {
      case Any =>
        proc.currentChar match {
          case Some(_) => proc.pos += 1
          case _       => backtrack = true
        }
      case Back =>
        if (proc.pos > 0) proc.pos -= 1
        else backtrack = true
      case CapBegin(n) =>
        proc.captureBegin(n, proc.pos)
      case CapEnd(n) =>
        proc.captureEnd(n, proc.pos)
      case CapReset(i, j) =>
        proc.captureReset(i, j)
      case Char(c) =>
        proc.currentChar match {
          case Some(d) if c == d => proc.pos += 1
          case _                 => backtrack = true
        }
      case Class(s) =>
        proc.currentChar match {
          case Some(c) if s.contains(c) => proc.pos += 1
          case _                        => backtrack = true
        }
      case ClassNot(s) =>
        proc.currentChar match {
          case Some(c) if !s.contains(c) => proc.pos += 1
          case _                         => backtrack = true
        }
      case Dec =>
        proc.cntStack = (proc.cntStack.head - 1) +: proc.cntStack.tail
      case Done =>
        return Some(Match(input, ir.names, proc.caps))
      case Dot =>
        proc.currentChar match {
          case Some(c) if !LineTerminator.contains(c) => proc.pos += 1
          case _                                      => backtrack = true
        }
      case EmptyCheck =>
        backtrack = proc.popPosStack() == proc.pos
      case Fail =>
        backtrack = true
      case ForkCont(next) =>
        val newProc = proc.clone()
        procs.push(newProc)
        proc.pc += next
      case ForkNext(next) =>
        val newProc = proc.clone()
        procs.push(newProc)
        newProc.pc += next
      case InputBegin =>
        if (proc.pos != 0) backtrack = true
      case InputEnd =>
        if (proc.pos != input.size) backtrack = true
      case Jump(cont) =>
        proc.pc += cont
      case LineBegin =>
        val c = proc.previousChar
        backtrack = c.exists(!LineTerminator.contains(_))
      case LineEnd =>
        val c = proc.currentChar
        backtrack = c.exists(!LineTerminator.contains(_))
      case Loop(cont) =>
        val n = proc.cntStack.head
        if (n > 0) proc.pc += cont
      case PopCnt =>
        proc.popCntStack()
      case PopProc =>
        proc.popProcStack()
      case PushCnt(n) =>
        proc.pushCntStack(n)
      case PushPos =>
        proc.pushPosStack(proc.pos)
      case PushProc =>
        proc.pushProcStack(procs.size)
      case Ref(n) =>
        val s = proc.capture(n).getOrElse(UString.empty)
        val t = input.substring(proc.pos, proc.pos + s.size)
        if (s == t) proc.pos += t.size
        else backtrack = true
      case RefBack(n) =>
        val s = proc.capture(n).getOrElse(UString.empty)
        val t = input.substring(proc.pos - s.size, proc.pos)
        if (s == t) proc.pos -= t.size
        else backtrack = true
      case RestorePos =>
        proc.pos = proc.popPosStack()
      case RewindProc =>
        val size = proc.popProcStack()
        while (size <= procs.size) procs.pop()
        procs.push(proc)
      case WordBoundary =>
        val c1 = proc.previousChar
        val c2 = proc.currentChar
        val w1 = c1.exists(Word.contains)
        val w2 = c2.exists(Word.contains)
        backtrack = !(w1 && !w2 || !w1 && w2)
      case WordBoundaryNot =>
        val c1 = proc.previousChar
        val c2 = proc.currentChar
        val w1 = c1.exists(Word.contains)
        val w2 = c2.exists(Word.contains)
        backtrack = w1 && !w2 || !w1 && w2
    }

    if (backtrack) procs.pop()

    // Traces this step.
    tracer.trace(oldPos, oldPc, backtrack, proc.captures, proc.cntStack)

    None
  }

  /** Proc is an internal state of VM execution. */
  final class Proc(
      var caps: IndexedSeq[Int],
      var posStack: Seq[Int],
      var cntStack: Seq[Int],
      var procStack: Seq[Int],
      var pos: Int,
      var pc: Int
  ) {

    /** Returns the current character of this proc. */
    def currentChar: Option[UChar] = input.get(pos)

    /** Returns the previous character of this proc. */
    def previousChar: Option[UChar] = input.get(pos - 1)

    /** A size as capture list. */
    def size: Int = caps.size / 2 - 1

    /** Gets the `n`-th capture string.
      *
      * An index `0` is whole match string, and indexes
      * between `1` and `size` are capture groups.
      */
    def capture(n: Int): Option[UString] =
      if (0 <= n && n <= size) {
        val begin = caps(n * 2)
        val end = caps(n * 2 + 1)
        if (begin >= 0 && end >= 0) Some(input.substring(begin, end)) else None
      } else None

    /** Returns a lambda from a capture index to capture string. */
    def captures: Int => Option[UString] = capture

    /** Updates a begin position of the `n`-th capture. */
    def captureBegin(n: Int, pos: Int): Unit = {
      caps = caps.updated(n * 2, pos)
    }

    /** Updates an end position of the `n`-th capture. */
    def captureEnd(n: Int, pos: Int): Unit = {
      caps = caps.updated(n * 2 + 1, pos)
    }

    /** Resets captures between `i` and `j`. */
    def captureReset(i: Int, j: Int): Unit = {
      for (k <- i to j) {
        captureBegin(k, -1)
        captureEnd(k, -1)
      }
    }

    /** Pops a value from cnt stack. */
    def popCntStack(): Int = {
      val cnt = cntStack.head
      cntStack = cntStack.tail
      cnt
    }

    /** Pops a value from pos stack. */
    def popPosStack(): Int = {
      val pos = posStack.head
      posStack = posStack.tail
      pos
    }

    /** Pops a value from proc stack. */
    def popProcStack(): Int = {
      val size = procStack.head
      procStack = procStack.tail
      size
    }

    /** Pushes a value to cnt stack. */
    def pushCntStack(n: Int): Unit = {
      cntStack = n +: cntStack
    }

    /** Pushes a value to pos stack. */
    def pushPosStack(n: Int): Unit = {
      posStack = n +: posStack
    }

    /** Pushes a value to pos stack. */
    def pushProcStack(n: Int): Unit = {
      procStack = n +: procStack
    }

    /** Returns a copy of this proc.
      *
      * It copies mutable states deeply.
      */
    override def clone(): Proc =
      new Proc(caps, posStack, cntStack, procStack, pos, pc)

    /** Shows this proc. */
    override def toString: String = s"Proc($caps, $posStack, $cntStack, $procStack, $pos, $pc)"
  }
}
