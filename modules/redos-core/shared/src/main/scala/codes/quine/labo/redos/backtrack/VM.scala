package codes.quine.labo.redos
package backtrack

import scala.collection.mutable

import IR._
import VM._
import data.IChar.{LineTerminator, Word}
import data.UChar
import data.UString

/** VM utilities. */
object VM {

  /** Executes the IR on the input from the initial position. */
  def execute(
      ir: IR,
      input: UString,
      pos: Int,
      tracer: Tracer = Tracer.NoTracer()
  ): Option[Match] =
    new VM(ir, input, pos, tracer).execute()

  /** Proc is a state of VM execution.
    *
    * It behaves a mutable [[CaptureList]] also.
    */
  private[backtrack] final class Proc(
      val input: UString,
      val names: Map[String, Int],
      val caps: mutable.IndexedSeq[Int],
      val poses: mutable.Stack[Int],
      val cnts: mutable.Stack[Int],
      val procs: mutable.Stack[Int],
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
      new Proc(input, names, caps.clone(), poses.clone(), cnts.clone(), procs.clone(), pos, pc)

    /** Shows this proc. */
    override def toString: String = s"Proc($input, $names, $caps, $poses, $cnts, $procs, $pos, $pc)"
  }
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
    val canon = if (ir.ignoreCase) UString.canonicalize(input, ir.unicode) else input
    val proc = new Proc(
      canon,
      ir.names,
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
        proc.cnts(0) -= 1
      case Done =>
        // Use `input` instead of `proc.input` here,
        // because `proc.input` is canonical.
        return Some(Match(input, proc.names, proc.caps.toIndexedSeq))
      case Dot =>
        proc.currentChar match {
          case Some(c) if !LineTerminator.contains(c) => proc.pos += 1
          case _                                      => backtrack = true
        }
      case EmptyCheck =>
        backtrack = proc.poses.pop() == proc.pos
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
        if (proc.pos != proc.input.size) backtrack = true
      case Jump(cont) =>
        proc.pc += cont
      case LineBegin =>
        val c = proc.previousChar
        backtrack = c.exists(!LineTerminator.contains(_))
      case LineEnd =>
        val c = proc.currentChar
        backtrack = c.exists(!LineTerminator.contains(_))
      case Loop(cont) =>
        val n = proc.cnts.top
        if (n > 0) proc.pc += cont
      case PopCnt =>
        proc.cnts.pop()
      case PopProc =>
        proc.procs.pop()
      case PushCnt(n) =>
        proc.cnts.push(n)
      case PushPos =>
        proc.poses.push(proc.pos)
      case PushProc =>
        proc.procs.push(procs.size)
      case Ref(n) =>
        val s = proc.capture(n).getOrElse(UString.empty)
        val t = proc.input.substring(proc.pos, proc.pos + s.size)
        if (s == t) proc.pos += t.size
        else backtrack = true
      case RefBack(n) =>
        val s = proc.capture(n).getOrElse(UString.empty)
        val t = proc.input.substring(proc.pos - s.size, proc.pos)
        if (s == t) proc.pos -= t.size
        else backtrack = true
      case RestorePos =>
        proc.pos = proc.poses.pop()
      case RewindProc =>
        val size = proc.procs.pop()
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
    tracer.trace(oldPos, oldPc, backtrack, proc.captures, proc.cnts.toSeq)

    None
  }
}
