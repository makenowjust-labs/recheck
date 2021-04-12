package codes.quine.labo.recheck.vm

import scala.collection.mutable

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.IChar.LineTerminator
import codes.quine.labo.recheck.data.IChar.Word
import codes.quine.labo.recheck.data.UChar
import codes.quine.labo.recheck.data.UString
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
      heatmap: Map[(Int, Int), Int]
  )

  /** Status is a matching status. */
  sealed abstract class Status extends Product with Serializable

  object Status {

    /** Matching is succeeded. */
    case object Ok extends Status

    /** Matching is failed. */
    case object Fail extends Status

    /** A limit is exceeded on matching. */
    case object Limit extends Status
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
  private[vm] final case class Junction(state: MatchState, steps: Int, heatmap: Option[Map[(Int, Int), Int]])

  /** Diff is a difference of state on matching. */
  private[vm] final case class Diff(steps: Int, heatmap: Option[Map[(Int, Int), Int]])
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
      var rollback: Option[Frame],
      var fallback: Option[Frame],
      var junctions: Vector[Junction]
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
        case (b, e) => Some(input.substring(b, e))
      }

    /** Converts this frame into a corresponding state. */
    def toState: MatchState =
      Interpreter.MatchState(label.index, pos, counters, if (program.meta.hasRef) Some(captures) else None)

    override def clone(): Frame =
      new Frame(label, pos, captures, counters, canaries, rollback, fallback, junctions)

    override def toString: String =
      s"Frame($label, $pos, $captures, $counters, $canaries, $rollback, $fallback, $junctions)"
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
  private[this] var heatmap: Map[(Int, Int), Int] = Map.empty[(Int, Int), Int].withDefaultValue(0)

  /** Constructs a result of matching. */
  private def result(status: Status, captures: Option[Seq[Int]]): Result = {
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

      if (!memoized && block(frame, frame.label.block)) {
        frame.label.block.terminator match {
          case Inst.Ok => return result(Status.Ok, Some(frame.captures))
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
                val captures = frame.captures
                frame = rollback
                frame.captures = captures
              case None => return result(Status.Fail, None)
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
            if (options.usesAcceleration && frame.junctions.size > fallback.junctions.size) {
              val d = frame.junctions.size - fallback.junctions.size
              for (jx <- frame.junctions.take(d)) {
                val diffHeatmap = if (options.needsHeatmap) {
                  val jxHeatmap = jx.heatmap.get
                  val builder = Map.newBuilder[(Int, Int), Int]
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
            frame.pos == input.size
        }
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        ok
      case Inst.Read(kind @ ReadKind.Ref(index), loc) =>
        val s = frame.capture(index) match {
          case Some(s) => s
          case None    => UString.empty
        }
        val ok = s == input.substring(frame.pos, frame.pos + s.size)
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        if (ok) {
          if (options.needsHeatmap && loc.isDefined) {
            heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
          }
          steps += 1
          frame.pos += s.size
          true
        } else {
          if (options.needsFailedPoints) {
            val point = FailedPoint(CoverageLocation(inst.id, frame.counters), frame.pos, kind, Some(s))
            failedPoints.add(point)
          }
          false
        }
      case Inst.Read(kind, loc) =>
        val ok = read(frame.currentChar, kind)
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        if (ok) {
          if (options.needsHeatmap && loc.isDefined) {
            heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
          }
          steps += 1
          frame.pos += 1
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
        val ok = s == input.substring(frame.pos - s.size, frame.pos)
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        if (ok) {
          if (options.needsHeatmap && loc.isDefined) {
            heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
          }
          steps += 1
          frame.pos -= s.size
          true
        } else {
          if (options.needsFailedPoints) {
            val point = FailedPoint(CoverageLocation(inst.id, frame.counters), frame.pos, kind, Some(s))
            failedPoints.add(point)
          }
          false
        }
      case Inst.ReadBack(kind, loc) =>
        val ok = read(frame.previousChar, kind)
        if (options.needsCoverage) coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        if (read(frame.previousChar, kind)) {
          if (options.needsHeatmap && loc.isDefined) {
            heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
          }
          steps += 1
          frame.pos -= 1
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
