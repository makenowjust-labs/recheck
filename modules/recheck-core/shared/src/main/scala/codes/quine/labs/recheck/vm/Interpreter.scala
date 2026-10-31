package codes.quine.labs.recheck.vm

import scala.collection.mutable
import scala.concurrent.duration.Duration

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.regexp.Location
import codes.quine.labs.recheck.util.UStringUtil.*
import codes.quine.labs.recheck.vm.Inst.AssertKind
import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.recheck.vm.Interpreter.CoverageItem
import codes.quine.labs.recheck.vm.Interpreter.CoverageLocation
import codes.quine.labs.recheck.vm.Interpreter.Diff
import codes.quine.labs.recheck.vm.Interpreter.FailedPoint
import codes.quine.labs.recheck.vm.Interpreter.Junction
import codes.quine.labs.recheck.vm.Interpreter.MatchState
import codes.quine.labs.recheck.vm.Interpreter.Options
import codes.quine.labs.recheck.vm.Interpreter.Result
import codes.quine.labs.recheck.vm.Interpreter.Status
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.IRBackReferenceChoice
import codes.quine.labs.resyntax.ir.IRCaseEquivalence
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.IRLineTerminators

/** Interpreter executes a program on a input. */
object Interpreter:

  /** Runs a program on a given input.
    *
    * Note that we assume an input string is normalized here.
    */
  def run(program: Program, input: UString, pos: Int, options: Options)(using ctx: Context): Result =
    val interpreter = Interpreter(program, input, options)
    interpreter.run(pos)

  /** Runs a program on a given input and a timeout. */
  def runWithTimeout(program: Program, input: UString, pos: Int, options: Options, timeout: Duration)(using
      ctx: Context
  ): Result =
    timeout match
      case d if d < Duration.Zero => Result(Status.Timeout, None, 0, Seq.empty, Set.empty, Set.empty, Map.empty)
      case _: Duration.Infinite   => run(program, input, pos, options)
      case d                      =>
        if (ctx.deadline ne null) && ctx.deadline.timeLeft < d then run(program, input, pos, options)
        val newCtx = Context(d, Option(ctx.token))

        val interpreter = new Interpreter(program, input, options)(using newCtx)
        try interpreter.run(pos)
        catch case _: TimeoutException => interpreter.result(Status.Timeout, None)

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

  object Status:

    /** Matching is succeeded. */
    case object Ok extends Status:
      override def toString: String = "ok"

    /** Matching is failed. */
    case object Fail extends Status:
      override def toString: String = "fail"

    /** A limit is exceeded on matching. */
    case object Limit extends Status:
      override def toString: String = "limit"

    case object Timeout extends Status:
      override def toString: String = "timeout"

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

/** Interpreter is an interpreter of a program and an input. */
private[vm] class Interpreter(program: Program, input: UString, options: Options)(using ctx: Context):

  /** Frame is a stack frame. */
  private class Frame(
      var label: Label,
      var pos: Int,
      var captures: Vector[Int],
      var counters: Vector[Int],
      var canaries: Vector[Int],
      var rollback: Option[Rollback],
      var fallback: Option[Frame],
      var junctions: Vector[Junction]
  ):

    /** Gets a previous character. */
    def previousChar: Option[Int] = input.getBefore(pos, program.meta.unicode)

    /** Gets a current character. */
    def currentChar: Option[Int] = input.getAt(pos, program.meta.unicode)

    /** Captures a beginning position of the index. */
    def capBegin(index: Int): Unit =
      captures = captures.updated(index * 2, pos)

    /** Captures an ending position of the index. */
    def capEnd(index: Int): Unit =
      captures = captures.updated(index * 2 + 1, pos)

    /** Resets captures between `from` and `to`. */
    def capReset(from: Int, to: Int): Unit =
      for index <- from to to do
        captures = captures.updated(index * 2, -1)
        captures = captures.updated(index * 2 + 1, -1)

    /** Returns a capture string. */
    def capture(index: Int): Option[UString] =
      (captures(index * 2), captures(index * 2 + 1)) match
        case (b, e) if b == -1 || e == -1 => None
        case (b, e)                       => Some(input.slice(b, e))

    /** Converts this frame into a corresponding state. */
    def toState: MatchState =
      Interpreter.MatchState(label.index, pos, counters, if program.meta.hasRef then Some(captures) else None)

    override def clone(): Frame =
      new Frame(label, pos, captures, counters, canaries, rollback, fallback, junctions)

    override def toString: String =
      s"Frame($label, $pos, $captures, $counters, $canaries, $rollback, $fallback, $junctions)"

  /** Rollback holds an information for rollbacks. */
  private sealed abstract class Rollback extends Product with Serializable

  // @nowarn // Ignores 'The outer reference in this type test cannot be checked at run time.' error.
  private object Rollback:

    /** HasSuccessor is a pair of a label and a position for rollback when `rollback` instruction has rollback successor
      * label.
      */
    final case class HasSuccessor(
        next: Label,
        pos: Int,
        rollback: Option[Rollback],
        fallback: Option[Frame]
    ) extends Rollback

    /** Fallback is a fallback frame for rollback when `rollback` instruction has no rollback successor label. */
    final case class Fallback(fallback: Option[Frame]) extends Rollback

  /** A number of current execution step. */
  private var steps: Int = 0

  /** A memoization table. */
  private val memoTable: mutable.Map[MatchState, Diff] = mutable.Map.empty

  /** A loop map table from block ID to positions. */
  private val loops: mutable.Map[Int, Seq[Int]] = mutable.Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)

  /** A coverage set. */
  private val coverage: mutable.Set[CoverageItem] = mutable.Set.empty

  /** A failed points list */
  private val failedPoints: mutable.Set[FailedPoint] = mutable.Set.empty

  /** A heatmap on matching. */
  private var heatmap: Map[Location, Int] = Map.empty[Location, Int].withDefaultValue(0)

  /** Constructs a result of matching. */
  private[vm] def result(status: Status, captures: Option[Seq[Int]]): Result =
    val loops = this.loops.iterator
      .flatMap:
        case (_, seq) => seq.sliding(2).collect { case Seq(i, j) if i < j => (i, j) }
      .toSeq
    Result(status, captures, steps, loops, failedPoints.toSet, coverage.toSet, heatmap)

  /** Runs matching from a position. */
  def run(pos: Int): Result =
    val frame = Frame(
      program.blocks.head._1,
      pos,
      Vector.fill((program.meta.capturesSize + 1) * 2)(-1),
      Vector.fill(program.meta.countersSize)(0),
      Vector.fill(program.meta.canariesSize)(0),
      None,
      None,
      Vector.empty
    )

    try runLoop(frame)
    catch case _: ArithmeticException => result(Status.Limit, None)

  /** Runs matching loop. */
  private def runLoop(initialFrame: Frame): Result =
    var frame = initialFrame

    // $COVERAGE-OFF$
    while true do
      ctx.interrupt:
        // $COVERAGE-ON$

        if options.limit <= steps then return result(Status.Limit, None)

        // When acceleration mode is enabled and current block is junction,
        // it tries to retrieve a result from memoization table, or adds a junction information.
        var memoized = false
        if options.usesAcceleration && program.meta.predecessors(frame.label.index).size >= 2 then
          val state = frame.toState
          memoTable.get(state) match
            case Some(diff) =>
              memoized = true
              steps = Math.addExact(steps, diff.steps)
              if options.needsHeatmap then
                for (loc, n) <- diff.heatmap.get do heatmap = heatmap.updated(loc, heatmap(loc) + n)
            case None =>
              val junction = Junction(state, steps, if options.needsHeatmap then Some(heatmap) else None)
              frame.junctions = junction +: frame.junctions

        if options.needsLoopAnalysis then
          val ps = program.meta.predecessors(frame.label.index)
          val isLoop = ps.exists(_.index >= frame.label.index) && ps.exists(_.index < frame.label.index)
          if isLoop then loops(frame.label.index) :+= frame.pos

        val failed = memoized || !block(frame, frame.label.block) ||
          (frame.label.block.terminator match
            case Inst.Ok        => return result(Status.Ok, Some(frame.captures))
            case Inst.Jmp(next) =>
              frame.label = next
              false
            case Inst.Try(next, fallback) =>
              val fallbackFrame = frame.clone()
              fallbackFrame.label = fallback
              frame.label = next
              frame.fallback = Some(fallbackFrame)
              false
            case Inst.TryLA(read, next, fallback) =>
              val oldPos = frame.pos
              if instRead(frame, read) then
                val fallbackFrame = frame.clone()
                fallbackFrame.pos = oldPos
                fallbackFrame.label = fallback
                frame.label = next
                frame.fallback = Some(fallbackFrame)
                false
              else
                frame.label = fallback
                false
            case Inst.TryLB(read, next, fallback) =>
              val oldPos = frame.pos
              if instReadBack(frame, read) then
                val fallbackFrame = frame.clone()
                fallbackFrame.pos = oldPos
                fallbackFrame.label = fallback
                frame.label = next
                frame.fallback = Some(fallbackFrame)
                false
              else
                frame.label = fallback
                false
            case Inst.Cmp(reg, n, lt, ge) =>
              val r = frame.counters(reg.index)
              val next = if r < n then lt else ge
              frame.label = next
              false
            case Inst.Rollback =>
              val failed = frame.rollback.get match
                case Rollback.HasSuccessor(next, pos, rollback, fallback) =>
                  frame.label = next
                  frame.pos = pos
                  frame.rollback = rollback
                  frame.fallback = fallback
                  false
                case Rollback.Fallback(fallback) =>
                  frame.fallback = fallback
                  true
              failed
            case Inst.Commit =>
              frame.rollback.get match
                case Rollback.HasSuccessor(next, _, rollback, fallback) =>
                  frame.label = next
                  frame.rollback = rollback
                  frame.fallback = fallback
                  false
                // $COVERAGE-OFF$
                case Rollback.Fallback(_) => sys.error("unreachable")
              // $COVERAGE-ON$
            case Inst.Tx(next, rollback, fallback) =>
              val nextRollback = rollback match
                case Some(rollback) => Rollback.HasSuccessor(rollback, frame.pos, frame.rollback, frame.fallback)
                case None           => Rollback.Fallback(frame.fallback)
              val nextFallback = fallback match
                case Some(fallback) =>
                  val fallbackFrame = frame.clone()
                  fallbackFrame.label = fallback
                  Some(fallbackFrame)
                case None => frame.fallback
              frame.label = next
              frame.rollback = Some(nextRollback)
              frame.fallback = nextFallback
              false)

        if failed then
          frame.fallback match
            case Some(fallback) =>
              if options.usesAcceleration && frame.junctions.size > fallback.junctions.size then
                val d = frame.junctions.size - fallback.junctions.size
                for jx <- frame.junctions.take(d) do
                  val diffHeatmap = Option.when(options.needsHeatmap):
                    val jxHeatmap = jx.heatmap.get
                    val builder = Map.newBuilder[Location, Int]
                    for (loc, n) <- heatmap do
                      val m = jxHeatmap(loc)
                      if n != m then builder.addOne(loc -> (n - m))
                    builder.result()
                  memoTable(jx.state) = Diff(steps - jx.steps, diffHeatmap)
              frame = fallback
            case None => return result(Status.Fail, None)

    // $COVERAGE-OFF$
    sys.error("unreachable")
    // $COVERAGE-ON$

  /** Runs a given block. */
  private def block(frame: Frame, block: Block): Boolean =
    val it = block.insts.iterator
    while it.hasNext do if !inst(frame, it.next()) then return false
    true

  /** Runs a given instruction. */
  private def inst(frame: Frame, inst: Inst.NonTerminator): Boolean =
    inst match
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
        val ok = kind match
          case AssertKind.WordBoundary(word) =>
            frame.previousChar.exists(word.contains) != frame.currentChar.exists(word.contains)
          case AssertKind.WordBoundaryNot(word) =>
            frame.previousChar.exists(word.contains) == frame.currentChar.exists(word.contains)
          case AssertKind.LineBegin(terminators, atEnd) =>
            if frame.pos == input.length then
              atEnd match
                case IRLineBeginAtEnd.AfterTerminator => frame.pos == 0 || afterTerminator(frame.pos, terminators)
                case IRLineBeginAtEnd.InputBegin      => frame.pos == 0
                case IRLineBeginAtEnd.Never           => false
            else frame.pos == 0 || afterTerminator(frame.pos, terminators)
          case AssertKind.LineEnd(terminators) =>
            frame.pos == input.length || beforeTerminator(frame.pos, terminators).isDefined
          case AssertKind.InputBegin =>
            frame.pos == 0
          case AssertKind.InputEnd =>
            frame.pos == input.length
          case AssertKind.ChompInputEnd(terminators) =>
            frame.pos == input.length || beforeTerminator(frame.pos, terminators).contains(input.length)
        if options.needsCoverage then coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
        ok
      case read: Inst.Read      => instRead(frame, read)
      case read: Inst.ReadBack  => instReadBack(frame, read)
      case Inst.CapBegin(index) =>
        frame.capBegin(index)
        true
      case Inst.CapEnd(index) =>
        frame.capEnd(index)
        true
      case Inst.CapReset(from, to) =>
        frame.capReset(from, to)
        true

  /** Tests whether the position is after a line terminator. */
  private def afterTerminator(pos: Int, terminators: IRLineTerminators): Boolean =
    val c = input.getBefore(pos, program.meta.unicode)
    val single = c.exists(terminators.chars.contains)
    val crlf =
      terminators.crlf != IRLineTerminators.CRLF.Ignored && c.contains(0x0a) && pos >= 2 && input(pos - 2) == 0x0d
    (single || crlf) && !betweenCRLF(pos, terminators)

  /** Returns the end position of a line terminator at the position, if any. */
  private def beforeTerminator(pos: Int, terminators: IRLineTerminators): Option[Int] =
    if betweenCRLF(pos, terminators) then None
    else if terminators.crlf != IRLineTerminators.CRLF.Ignored && pos + 1 < input.length &&
      input(pos) == 0x0d && input(pos + 1) == 0x0a
    then Some(pos + 2)
    else
      input
        .getAt(pos, program.meta.unicode)
        .filter(terminators.chars.contains)
        .map(c => pos + (if c >= 0x10000 then 2 else 1))

  /** Tests whether the position is between `\r` and `\n`, where no line assertion matches with `CRLF.Atomic`. */
  private def betweenCRLF(pos: Int, terminators: IRLineTerminators): Boolean =
    terminators.crlf == IRLineTerminators.CRLF.Atomic && 0 < pos && pos < input.length &&
      input(pos - 1) == 0x0d && input(pos) == 0x0a

  /** Returns the captures that a back-reference compares in turn, where `None` matches the empty string. */
  private def refCandidates(frame: Frame, ref: ReadKind.Ref): Seq[Option[UString]] =
    val captures = ref.indices.flatMap(frame.capture)
    ref.semantics.choice match
      case IRBackReferenceChoice.FirstSet =>
        captures.headOption match
          case Some(s)                                 => Seq(Some(s))
          case None if ref.semantics.unsetMatchesEmpty => Seq(None)
          case None                                    => Seq.empty
      case IRBackReferenceChoice.LastMatching =>
        if captures.isEmpty && ref.semantics.unsetMatchesEmpty then Seq(None) else captures.reverse.map(Some(_))

  /** Compares the capture with the input from the position forward (or backward), and returns the compared count and
    * the matched width of the input if matched.
    */
  private def compare(s: UString, pos: Int, back: Boolean, equivalence: IRCaseEquivalence): (Int, Option[Int]) =
    if equivalence == IRCaseEquivalence.Identity then
      var count = 0
      if back then
        while count < s.length && pos - count - 1 >= 0 && s(s.length - count - 1) == input(pos - count - 1) do
          count += 1
      else while count < s.length && pos + count < input.length && s(count) == input(pos + count) do count += 1
      (count, Option.when(count == s.length)(s.length))
    else
      val unicode = program.meta.unicode
      def width(c: Int): Int = if unicode && c >= 0x10000 then 2 else 1
      var i = 0
      var j = 0
      var continues = true
      while continues && i < s.length do
        val c1 = if back then s.getBefore(s.length - i, unicode) else s.getAt(i, unicode)
        val c2 = if back then input.getBefore(pos - j, unicode) else input.getAt(pos + j, unicode)
        (c1, c2) match
          case (Some(c1), Some(c2)) if c1 == c2 || equivalence.equiv(c1, c2) =>
            i += width(c1)
            j += width(c2)
          case _ => continues = false
      (i, Option.when(i == s.length)(j))

  /** Runs a `read` (or `read_back`) instruction of a back-reference. */
  private def instReadRef(frame: Frame, inst: Inst, ref: ReadKind.Ref, loc: Option[Location], back: Boolean): Boolean =
    val candidates = refCandidates(frame, ref)
    var width: Option[Int] = None
    val it = candidates.iterator
    while width.isEmpty && it.hasNext do
      it.next() match
        case Some(s) =>
          val (count, w) = compare(s, frame.pos, back, ref.equivalence)
          steps = Math.addExact(steps, count)
          width = w
        case None => width = Some(0)
    val ok = width.isDefined

    if options.needsCoverage then coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
    if ok then
      if options.needsHeatmap && loc.isDefined then heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
      frame.pos += (if back then -width.get else width.get)
      true
    else
      if options.needsFailedPoints then
        val capture = candidates.headOption.flatten
        val point = FailedPoint(CoverageLocation(inst.id, frame.counters), frame.pos, ref, capture)
        failedPoints.add(point)
      false

  /** Runs a given `read` instruction. */
  private def instRead(frame: Frame, inst: Inst.Read): Boolean = inst match
    case Inst.Read(ref: ReadKind.Ref, loc) => instReadRef(frame, inst, ref, loc, false)
    case Inst.Read(kind, loc)              =>
      val c = frame.currentChar
      val ok = read(c, kind)
      if options.needsCoverage then coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
      if ok then
        if options.needsHeatmap && loc.isDefined then heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
        val width = c.map(c => if c >= 0x10000 then 2 else 1).getOrElse(0)
        steps = Math.addExact(steps, width)
        frame.pos += width
        true
      else
        if options.needsFailedPoints then
          val point = FailedPoint(CoverageLocation(inst.id, frame.counters), frame.pos, kind, None)
          failedPoints.add(point)
        false

  /** Runs a given `read-back` instruction. */
  private def instReadBack(frame: Frame, inst: Inst.ReadBack): Boolean = inst match
    case Inst.ReadBack(ref: ReadKind.Ref, loc) => instReadRef(frame, inst, ref, loc, true)
    case Inst.ReadBack(kind, loc)              =>
      val c = frame.previousChar
      val ok = read(c, kind)
      if options.needsCoverage then coverage.add(CoverageItem(CoverageLocation(inst.id, frame.counters), ok))
      if ok then
        if options.needsHeatmap && loc.isDefined then heatmap = heatmap.updated(loc.get, heatmap(loc.get) + 1)
        val width = c.map(c => if c >= 0x10000 then 2 else 1).getOrElse(0)
        steps = Math.addExact(steps, width)
        frame.pos -= width
        true
      else
        if options.needsFailedPoints then
          val point = FailedPoint(CoverageLocation(inst.id, frame.counters), frame.pos, kind, None)
          failedPoints.add(point)
        false

  /** Checks to match `read` instruction. */
  private def read(c: Option[Int], kind: ReadKind): Boolean =
    c match
      case Some(c) =>
        kind match
          case ReadKind.Any         => true
          case ReadKind.Char(d)     => c == d
          case ReadKind.Class(s)    => s.contains(c)
          case ReadKind.ClassNot(s) => !s.contains(c)
          case _: ReadKind.Ref      =>
            // $COVERAGE-OFF$
            sys.error("unreachable")
          // $COVERAGE-ON$
      case None => false
