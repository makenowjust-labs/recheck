package codes.quine.labo.redos
package backtrack

import scala.collection.mutable

import IR._
import data.IChar.{LineTerminator, Word}
import data.UChar
import data.UString

/** VM utilities. */
object VM {

  /** Executes the IR on the input from the initial position.
    *
    * Note that the input string should be canonical.
    */
  def execute(
      ir: IR,
      input: UString,
      pos: Int,
      tracer: Tracer = Tracer.NoTracer()
  ): Option[Match] =
    new VM(ir, input, pos, tracer).execute()
}

/** VM is a RegExp matching VM. */
private[backtrack] final class VM(
    private[this] val ir: IR,
    private[this] val input: UString,
    initPos: Int,
    private[this] val tracer: Tracer = Tracer.NoTracer()
) {

  /** An execution process list (stack). */
  private[backtrack] val procs: mutable.Stack[Proc] = {
    val proc = new Proc(
      mutable.IndexedSeq.fill((ir.capsSize + 1) * 2)(-1),
      mutable.Stack.empty,
      mutable.Stack.empty,
      mutable.Stack.empty,
      initPos,
      0
    )
    mutable.Stack(proc)
  }

  /** Executes this VM. */
  def execute(): Option[Match] = {
    while (procs.nonEmpty)
      step() match {
        case Some(m) => return Some(m)
        case None    => ()
      }
    None
  }

  /** Runs this VM in a step. */
  private[backtrack] def step(): Option[Match] = {
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
        proc.cntStack(0) -= 1
      case Done =>
        return Some(Match(input, ir.names, proc.caps.toIndexedSeq))
      case Dot =>
        proc.currentChar match {
          case Some(c) if !LineTerminator.contains(c) => proc.pos += 1
          case _                                      => backtrack = true
        }
      case EmptyCheck =>
        backtrack = proc.posStack.pop() == proc.pos
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
        val n = proc.cntStack.top
        if (n > 0) proc.pc += cont
      case PopCnt =>
        proc.cntStack.pop()
      case PopProc =>
        proc.procStack.pop()
      case PushCnt(n) =>
        proc.cntStack.push(n)
      case PushPos =>
        proc.posStack.push(proc.pos)
      case PushProc =>
        proc.procStack.push(procs.size)
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
        proc.pos = proc.posStack.pop()
      case RewindProc =>
        val size = proc.procStack.pop()
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
    tracer.trace(oldPos, oldPc, backtrack, proc.captures, proc.cntStack.toSeq)

    None
  }

  /** Proc is an internal state of VM execution. */
  final class Proc(
      val caps: mutable.IndexedSeq[Int],
      val posStack: mutable.Stack[Int],
      val cntStack: mutable.Stack[Int],
      val procStack: mutable.Stack[Int],
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
    def captureBegin(n: Int, pos: Int): Unit = caps.update(n * 2, pos)

    /** Updates an end position of the `n`-th capture. */
    def captureEnd(n: Int, pos: Int): Unit = caps.update(n * 2 + 1, pos)

    /** Resets captures between `i` and `j`. */
    def captureReset(i: Int, j: Int): Unit = {
      for (k <- (i to j)) {
        captureBegin(k, -1)
        captureEnd(k, -1)
      }
    }

    /** Returns a copy of this proc.
      *
      * It copies mutable states deeply.
      */
    override def clone(): Proc =
      new Proc(caps.clone(), posStack.clone(), cntStack.clone(), procStack.clone(), pos, pc)

    /** Shows this proc. */
    override def toString: String = s"Proc($caps, $posStack, $cntStack, $procStack, $pos, $pc)"
  }
}
