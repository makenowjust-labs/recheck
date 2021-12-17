package codes.quine.labo.recheck.vm

import scala.annotation.nowarn
import scala.collection.mutable
import scala.concurrent.duration.Duration

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.TimeoutException
import codes.quine.labo.recheck.regexp.Pattern.Location
import codes.quine.labo.recheck.unicode.IChar.LineTerminator
import codes.quine.labo.recheck.unicode.IChar.Word
import codes.quine.labo.recheck.unicode.UChar
import codes.quine.labo.recheck.unicode.UString
import codes.quine.labo.recheck.vm.Inst.AssertKind
import codes.quine.labo.recheck.vm.Inst.ReadKind
import codes.quine.labo.recheck.vm.Interpreter.CoverageItem
import codes.quine.labo.recheck.vm.Interpreter.CoverageLocation
import codes.quine.labo.recheck.vm.Interpreter.Diff
import codes.quine.labo.recheck.vm.Interpreter.FailedPoint
import codes.quine.labo.recheck.vm.Interpreter.Junction
import codes.quine.labo.recheck.vm.Interpreter.MatchState
import codes.quine.labo.recheck.vm.Interpreter.Options
import codes.quine.labo.recheck.vm.Interpreter.Result
import codes.quine.labo.recheck.vm.Interpreter.Status

/** Interpreter executes a program on a input. */
object Interpreter {

  /** Runs a program on a given input.
    *
    * Note that we assume an input string is normalized here.
    */
  def run(program: Program, input: UString, pos: Int, options: Options)(implicit ctx: Context): Result = {
    val interpreter = new Interpreter(program, input, options)
    interpreter.run(pos)
  }

  /** Runs a program on a given input and a timeout. */
  def runWithTimeout(program: Program, input: UString, pos: Int, options: Options, timeout: Duration)(implicit
      ctx: Context
  ): Result = {
    timeout match {
      case d if d < Duration.Zero => Result(Status.Timeout, None, 0, Seq.empty, Set.empty, Set.empty, Map.empty)
      case _: Duration.Infinite   => run(program, input, pos, options)
      case d =>
        if ((ctx.deadline ne null) && ctx.deadline.timeLeft < d) run(program, input, pos, options)
        val newCtx = Context(d, Option(ctx.token))

        val interpreter = new Interpreter(program, input, options)(newCtx)
        try interpreter.run(pos)
        catch {
          case _: TimeoutException => interpreter.result(Status.Timeout, None)
        }
    }
  }

  /** Options is an options for running an interpreter. */
  final case class Options(
      limit: Int = Int.MaxValue,
      usesAcceleration: Boolean = false,
      needsLoopAnalysis: Boolean = false,
      needsFailedPoints: Boolean = false,
      needsCoverage: Boolean = false,
      needsHeatmap: Boolean = false
  )

  /** Result is a matching result. */
  final case class Result(
      status: Status,
      captures: Option[Seq[Int]],
      steps: Int,
      loops: Seq[(Int, Int)],
      failedPoints: Set[FailedPoint],
      coverage: Set[CoverageItem],
      heatmap: Map[Location, Int]
  )

  /** Status is a matching status. */
  sealed abstract class Status extends Product with Serializable

  object Status {

    /** Matching is succeeded. */
    case object Ok extends Status {
      override def toString: String = "ok"
    }

    /** Matching is failed. */
    case object Fail extends Status {
      override def toString: String = "fail"
    }

    /** A limit is exceeded on matching. */
    case object Limit extends Status {
      override def toString: String = "limit"
    }

    case object Timeout extends Status {
      override def toString: String = "timeout"
    }
  }

  /** CoverageLocation is a location in coverage. */
  final case class CoverageLocation(instID: Int, counters: Vector[Int])

  /** CoverageItem is an item of matching coverage. */
  final case class CoverageItem(loc: CoverageLocation, ok: Boolean)

  /** FailedPoint is a failed point on matching. */
  final case class FailedPoint(target: CoverageLocation, pos: Int, kind: ReadKind, capture: Option[UString])

  /** MatchState is a state of matching. */
  private[vm] final case class MatchState(blockID: Int, pos: Int, counters: Vector[Int], captures: Option[Vector[Int]])

  /** Junction is a state for a junction block. */
  private[vm] final case class Junction(state: MatchState, steps: Int, heatmap: Option[Map[Location, Int]])

  /** Diff is a difference of state on matching. */
  private[vm] final case class Diff(steps: Int, heatmap: Option[Map[Location, Int]])
}

/** Interpreter is an interpreter of a program and an input. */
private[vm] class Interpreter(program: Program, input: UString, options: Options)(implicit ctx: Context) {

  /** Frame is a stack frame. */
  private[this] class Frame(
      var label: Label,
      var pos: Int,
      var captures: Vector[Int],
      var counters: Vector[Int],
      var canaries: Vector[Int],
      var rollback: Option[Rollback],
      var fallback: Option[Frame],
      var junctions: Vector[Junction]
  ) {

    /** Gets a previous character. */
    def previousChar: Option[UChar] = input.getBefore(pos, program.meta.unicode)

    /** Gets a current character. */
    def currentChar: Option[UChar] = input.getAt(pos, program.meta.unicode)

    /** Captures a beginning position of the index. */
    def capBegin(index: Int): Unit = {
      captures = captures.updated(index * 2, pos)
    }

    /** Captures an ending position of the index. */
    def capEnd(index: Int): Unit = {
      captures = captures.updated(index * 2 + 1, pos)
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
        case (b, e) if b == -1 || e == -1 => None
        case (b, e)                       => Some(input.substring(b, e))
      }

    /** Converts this frame into a corresponding state. */
    def toState: MatchState =
      Interpreter.MatchState(label.index, pos, counters, if (program.meta.hasRef) Some(captures) else None)

    override def clone(): Frame =
      new Frame(label, pos, captures, counters, canaries, rollback, fallback, junctions)

    override def toString: String =
      s"Frame($label, $pos, $captures, $counters, $canaries, $rollback, $fallback, $junctions)"
  }

  /** Rollback holds an information for rollbacks. */
  private[this] sealed abstract class Rollback extends Product with Serializable

  @nowarn // Ignores 'The outer reference in this type test cannot be checked at run time.' error.
  private[this] object Rollback {

    /** HasSuccessor is a pair of a label and a position for rollback when `rollback` instruction has rollback successor
      * label.
      */
    final case class HasSuccessor private (
        next: Label,
        pos: Int,
        rollback: Option[Rollback],
        fallback: Option[Frame]
    ) extends Rollback

    /** Fallback is a fallback frame for rollback when `rollback` instruction has no rollback successor label. */
    final case class Fallback private (fallback: Option[Frame]) extends Rollback
  }

  /** A number of current execution step. */
  private[this] var steps: Int = 0

  /** A memoization table. */
  private[this] val memoTable: mutable.Map[MatchState, Diff] = mutable.Map.empty

  /** A loop map table from block ID to positions. */
  private[this] val loops: mutable.Map[Int, Seq[Int]] = mutable.Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)

  /** A coverage set. */
  private[this] val coverage: mutable.Set[CoverageItem] = mutable.Set.empty

  /** A failed points list */
  private[this] val failedPoints: mutable.Set[FailedPoint] = mutable.Set.empty

  /** A heatmap on matching. */
  private[this] var heatmap: Map[Location, Int] = Map.empty[Location, Int].withDefaultValue(0)

  /** Constructs a result of matching. */
  private[vm] def result(status: Status, captures: Option[Seq[Int]]): Result = {
    val loops = this.loops.iterator.flatMap { case (_, seq) =>
      seq.sliding(2).collect { case Seq(i, j) if i < j => (i, j) }
    }.toSeq
    Result(status, captures, steps, loops, failedPoints.toSet, coverage.toSet, heatmap)
  }

  /** Runs matching from a position. */
  def run(pos: Int): Result = {
    var frame = new Frame(
      program.blocks.head._1,
      pos,
      Vector.fill((program.meta.capturesSize + 1) * 2)(-1),
      Vector.fill(program.meta.countersSize)(0),
      Vector.fill(program.meta.canariesSize)(0),
      None,
      None,
      Vector.empty
    )

    // $COVERAGE-OFF$
    while (true) ctx.interrupt {
      // $COVERAGE-ON$

      if (options.limit <= steps) return result(Status.Limit, None)

      // When acceleration mode is enabled and current block is junction,
      // it tries to retrieve a result from memoization table, or adds a junction information.
      var memoized = false
      if (options.usesAcceleration && program.meta.predecessors(frame.label.index).size >= 2) {
        val state = frame.toState
        memoTable.get(state) match {
          case Some(diff) =>
            memoized = true
            steps += diff.steps
            if (options.needsHeatmap) {
              for ((loc, n) <- diff.heatmap.get) {
                heatmap = heatmap.updated(loc, heatmap(loc) + n)
              }
            }
          case None =>
            val junction = Junction(state, steps, if (options.needsHeatmap) Some(heatmap) else None)
            frame.junctions = junction +: frame.junctions
        }
      }

      if (options.needsLoopAnalysis) {
        val ps = program.meta.predecessors(frame.label.index)
        val isLoop = ps.exists(_.index >= frame.label.index) && ps.exists(_.index < frame.label.index)
        if (isLoop) {
          loops(frame.label.index) :+= frame.pos
        }
      }

      val failed = memoized || !block(frame, frame.label.block) ||
        (frame.label.block.terminator match {
          case Inst.Ok => return result(Status.Ok, Some(frame.captures))
          case Inst.Jmp(next) =>
            frame.label = next
            false
          case Inst.Try(next, fallback) =>
            val fallbackFrame = frame.clone()
            fallbackFrame.label = fallback
            frame.label = next
            frame.fallback = Some(fallbackFrame)
            false
          case Inst.Cmp(reg, n, lt, ge) =>
            val r = frame.counters(reg.index)
            val next = if (r < n) lt else ge
            frame.label = next
            false
          case Inst.Rollback =>
            @nowarn // Ignores 'The outer reference in this type test cannot be checked at run time.' error.
            val failed = frame.rollback.get match {
              case Rollback.HasSuccessor(next, pos, rollback, fallback) =>
                frame.label = next
                frame.pos = pos
                frame.rollback = rollback
                frame.fallback = fallback
                false
              case Rollback.Fallback(fallback) =>
                frame.fallback = fallback
                true
            }
            failed
          case Inst.Tx(next, rollback, fallback) =>
            val nextRollback = rollback match {
              case Some(rollback) => Rollback.HasSuccessor(rollback, frame.pos, frame.rollback, frame.fallback)
              case None           => Rollback.Fallback(frame.fallback)
            }
            val nextFallback = fallback match {
              case Some(fallback) =>
                val fallbackFrame = frame.clone()
                fallbackFrame.label = fallback
                Some(fallbackFrame)
              case None => frame.fallback
            }
            frame.label = next
            frame.rollback = Some(nextRollback)
            frame.fallback = nextFallback
            false
        })

      if (failed) {
        frame.fallback match {
          case Some(fallback) =>
            if (options.usesAcceleration && frame.junctions.size > fallback.junctions.size) {
              val d = frame.junctions.size - fallback.junctions.size
              for (jx <- frame.junctions.take(d)) {
                val diffHeatmap = if (options.needsHeatmap) {
                  val jxHeatmap = jx.heatmap.get
                  val builder = Map.newBuilder[Location, Int]
                  for ((loc, n) <- heatmap) {
                    val m = jxHeatmap(loc)
                    if (n != m) builder.addOne(loc -> (n - m))
                  }
                  Some(builder.result())
                } else None
                memoTable(jx.state) = Diff(steps - jx.steps, diffHeatmap)
              }
            }
            frame = fallback
          case None => return result(Status.Fail, None)
        }
      }
    }

    // $COVERAGE-OFF$
    sys.error("unreachable")
    // $COVERAGE-ON$
  }

  /** Runs a given block. */
  private def block(frame: Frame, block: Block): Boolean = {
    val it = block.insts.iterator
    while (it.hasNext) {
      if (!inst(frame, it.next())) return false
    }
    true
  }

  /** Runs a given instruction. */
  private def inst(frame: Frame, inst: Inst.NonTerminator): Boolean =
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
        val ok = kind match {
          case AssertKind.WordBoundary =>
            val c1 = frame.previousChar
            val c2 = frame.currentChar
            val w1 = c1.exists(Word.contains)
            val w2 = c2.exists(Word.contains)
            w1 != w2
          case AssertKind.WordBoundaryNot =>
            val c1 = frame.previousChar
            val c2 = frame.currentChar
            val w1 = c1.exists(Word.contains)
            val w2 = c2.exists(Word.contains)
            w1 == w2
          case AssertKind.LineBegin =>
            val c1 = frame.previousChar
            c1.forall(LineTerminator.contains)
          case AssertKind.LineEnd =>
            val c2 = frame.currentChar
            c2.forall(LineTerminator.contains)
          case AssertKind.InputBegin =>
            frame.pos == 0
          case AssertKind.InputEnd =>
            frame.pos == input.sizeAsString
        }
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        ok
      case Inst.Read(kind @ ReadKind.Ref(index), loc) =>
        val s = frame.capture(index) match {
          case Some(s) => s
          case None    => UString.empty
        }
        val ok = s == input.substring(frame.pos, frame.pos + s.sizeAsString)
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        if (ok) {
          if (options.needsHeatmap && loc.isDefined) {
            heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
          }
          steps += 1
          frame.pos += s.sizeAsString
          true
        } else {
          if (options.needsFailedPoints) {
            val point = FailedPoint(CoverageLocation(inst.id, frame.counters), frame.pos, kind, Some(s))
            failedPoints.add(point)
          }
          false
        }
      case Inst.Read(kind, loc) =>
        val c = frame.currentChar
        val ok = read(c, kind)
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        if (ok) {
          if (options.needsHeatmap && loc.isDefined) {
            heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
          }
          steps += 1
          frame.pos += c.size
          true
        } else {
          if (options.needsFailedPoints) {
            val point = FailedPoint(CoverageLocation(inst.id, frame.counters), frame.pos, kind, None)
            failedPoints.add(point)
          }
          false
        }
      case Inst.ReadBack(kind @ ReadKind.Ref(index), loc) =>
        val s = frame.capture(index) match {
          case Some(s) => s
          case None    => UString.empty
        }
        val ok = s == input.substring(frame.pos - s.sizeAsString, frame.pos)
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        if (ok) {
          if (options.needsHeatmap && loc.isDefined) {
            heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
          }
          steps += 1
          frame.pos -= s.sizeAsString
          true
        } else {
          if (options.needsFailedPoints) {
            val point = FailedPoint(CoverageLocation(inst.id, frame.counters), frame.pos, kind, Some(s))
            failedPoints.add(point)
          }
          false
        }
      case Inst.ReadBack(kind, loc) =>
        val c = frame.previousChar
        val ok = read(c, kind)
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        if (read(frame.previousChar, kind)) {
          if (options.needsHeatmap && loc.isDefined) {
            heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
          }
          steps += 1
          frame.pos -= c.size
          true
        } else {
          if (options.needsFailedPoints) {
            val point = FailedPoint(CoverageLocation(inst.id, frame.counters), frame.pos, kind, None)
            failedPoints.add(point)
          }
          false
        }
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
  private def read(c: Option[UChar], kind: ReadKind): Boolean =
    c match {
      case Some(c) =>
        kind match {
          case ReadKind.Any         => true
          case ReadKind.Dot         => !LineTerminator.contains(c)
          case ReadKind.Char(d)     => c == d
          case ReadKind.Class(s)    => s.contains(c)
          case ReadKind.ClassNot(s) => !s.contains(c)
          case ReadKind.Ref(_)      =>
            // $COVERAGE-OFF$
            sys.error("unreachable")
          // $COVERAGE-ON$
        }
      case None => false
    }
}
