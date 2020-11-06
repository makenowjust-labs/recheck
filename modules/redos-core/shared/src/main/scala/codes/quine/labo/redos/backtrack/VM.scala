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
  def execute(ir: IR, input: UString, pos: Int): Option[Match] =
    new VM(ir, input, pos).execute()

  /** Proc is a state of VM execution.
    *
    * It behaves a mutable [[CaptureList]] also.
    */
  private[backtrack] final class Proc(
      val input: UString,
      val names: Map[String, Int],
      val capsSeq: mutable.IndexedSeq[Int],
      val stack: mutable.Stack[Int],
      var pos: Int,
      var pc: Int
  ) extends CaptureList {

    /** Returns the current character of this proc. */
    def currentChar: Option[UChar] = input.get(pos)

    /** Returns the previous character of this proc. */
    def previousChar: Option[UChar] = input.get(pos - 1)

    /** A size as capture list. */
    def size: Int = capsSeq.size / 2 - 1

    /** A capture indexes. */
    def caps(i: Int): Int = capsSeq(i)

    /** Updates a begin position of the `n`-th capture. */
    def captureBegin(n: Int, pos: Int): Unit = capsSeq.update(n * 2, pos)

    /** Updates an end position of the `n`-th capture. */
    def captureEnd(n: Int, pos: Int): Unit = capsSeq.update(n * 2 + 1, pos)

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
    override def clone(): Proc = new Proc(input, names, capsSeq.clone(), stack.clone(), pos, pc)

    /** Shows this proc. */
    override def toString: String = s"Proc($input, $names, $capsSeq, $stack, $pos, $pc)"
  }
}

/** VM is a RegExp matching VM. */
private[backtrack] final class VM(
    private[this] val ir: IR,
    private[this] val input: UString,
    initPos: Int
) {

  /** An execution process list (stack). */
  private[backtrack] val procs: mutable.Stack[Proc] = {
    val canonicalized = if (ir.ignoreCase) UString.canonicalize(input, ir.unicode) else input
    val proc = new Proc(
      canonicalized,
      ir.names,
      mutable.IndexedSeq.fill((ir.capsSize + 1) * 2)(-1),
      mutable.Stack.empty,
      initPos,
      0
    )
    mutable.Stack(proc)
  }

  /** Eexecutes this VM. */
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
        proc.stack(0) -= 1
      case Done =>
        // Use `input` instead of `proc.input` here,
        // because `proc.input` is canonicalized.
        return Some(Match(input, proc.names, proc.capsSeq.toIndexedSeq))
      case Dot =>
        proc.currentChar match {
          case Some(c) if !LineTerminator.contains(c) => proc.pos += 1
          case _                                      => backtrack = true
        }
      case EmptyCheck =>
        backtrack = proc.stack.pop() == proc.pos
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
        val n = proc.stack.top
        if (n > 0) proc.pc += cont
      case Pop =>
        proc.stack.pop()
      case Push(n) =>
        proc.stack.push(n)
      case PushPos =>
        proc.stack.push(proc.pos)
      case PushProc =>
        proc.stack.push(procs.size)
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
        proc.pos = proc.stack.pop()
      case RewindProc =>
        val size = proc.stack.pop()
        while (size < procs.size) procs.pop()
        procs.push(proc)
      case WordBoundary =>
        val c1 = proc.previousChar
        val c2 = proc.currentChar
        val w1 = c1.exists(Word.contains(_))
        val w2 = c2.exists(Word.contains(_))
        backtrack = !(w1 && !w2 || !w1 && w2)
      case WordBoundaryNot =>
        val c1 = proc.previousChar
        val c2 = proc.currentChar
        val w1 = c1.exists(Word.contains(_))
        val w2 = c2.exists(Word.contains(_))
        backtrack = w1 && !w2 || !w1 && w2
    }

    if (backtrack) procs.pop()

    None
  }
}
