package codes.quine.labs.recheck.vm

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.util.Try

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.regexp.Location
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.regexp.PatternExtensions.*
import codes.quine.labs.recheck.vm.Inst.AssertKind
import codes.quine.labs.recheck.vm.Inst.Barrier
import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.recheck.vm.Inst.VerbKind
import codes.quine.labs.recheck.vm.Program.Meta
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRAssertKind
import codes.quine.labs.resyntax.ir.IRBacktrackControl
import codes.quine.labs.resyntax.ir.IRBuilder
import codes.quine.labs.resyntax.ir.IRCondition
import codes.quine.labs.resyntax.ir.IRLookBehindMatch
import codes.quine.labs.resyntax.ir.IRLookBehindStarts
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRNotEmpty
import codes.quine.labs.resyntax.ir.IRQuantifier

/** ProgramBuilder is a builder of a program from a RegExp pattern. */
object ProgramBuilder:

  /** Builds a corresponding program from the RegExp pattern. */
  def build(pattern: ParsedPattern)(using ctx: Context): Try[Program] =
    for
      _ <- Try(())
      capturesSize = pattern.capturesSize
      builder = new ProgramBuilder(pattern, capturesSize)
      program <- Try(builder.build())
    yield program

/** Keep is the captures that a fallback restores in Perl, where the others keep the values of the failed matching, or
  * `None` for a fallback restoring all.
  */
private[vm] type Keep = Option[Option[(Int, Int)]]

/** AcceptScope is what `(*ACCEPT)` ends successfully. */
private[vm] enum AcceptScope:

  /** The whole match. */
  case Top

  /** A look-around assertion or a condition, which succeeds (or fails for a negative one). */
  case LookAround

  /** Perl's atomic group, which Perl runs as a sub-match. */
  case Atomic

private[vm] class ProgramBuilder(
    private val pattern: ParsedPattern,
    private val capturesSize: Int
)(using ctx: Context):
  import ctx._

  /** The unit mode of the pattern. */
  private val mode = pattern.ir.mode

  /** Whether captures in a repetition are reset on each iteration, which only JavaScript does. */
  private val resetsCaptures = pattern.dialect == Dialect.JavaScript

  /** Whether captures follow Perl's backtracking, which back-references and capture tests only see. */
  private val perlCaptures =
    pattern.dialect == Dialect.Perl && pattern.node.exists:
      case _: IRNodeData.BackReference                           => true
      case IRNodeData.Conditional(_: IRCondition.Captured, _, _) => true
      case _                                                     => false

  /** Whether the captures in an atomic group or a look-around are held over backtracking once it matches, as in Java,
    * which back-references only see.
    */
  private val holdsCaptures =
    pattern.dialect == Dialect.Java && pattern.node.exists(_.isInstanceOf[IRNodeData.BackReference])

  /** Whether `(*THEN)` ends at a look-around assertion or a condition, as in PCRE but not in Perl. */
  private val assertionIsThenScope = pattern.dialect != Dialect.Perl

  /** Whether the pattern has the verbs skipping backtracking points, which need the barriers. */
  private val hasCut = pattern.node.exists:
    case IRNodeData.BacktrackControl(
          IRBacktrackControl.Commit | IRBacktrackControl.Prune | _: IRBacktrackControl.Skip | IRBacktrackControl.Then
        ) =>
      true
    case _ => false

  /** The names that `(*SKIP:NAME)` looks for, whose marks need backtracking points. */
  private val skipNames = pattern.node.collect { case IRNodeData.BacktrackControl(IRBacktrackControl.Skip(Some(n))) =>
    n
  }.toSet

  /** The scope that `(*THEN)` at the current position takes, or `None` to act as `(*PRUNE)`. */
  private var thenScope: Option[Int] = None

  /** A next scope ID of `(*THEN)`. */
  private var nextThenScope: Int = 0

  /** The scope that `(*ACCEPT)` at the current position ends. */
  private var acceptScope: AcceptScope = AcceptScope.Top

  /** The captures open in [[acceptScope]], the innermost first, which `(*ACCEPT)` closes. */
  private var openCaptures: List[Int] = Nil

  /** The number of the atomic groups in [[acceptScope]], whose transactions `(*ACCEPT)` drops. */
  private var atomicDepth: Int = 0

  /** The blocks emitted after the main blocks: the fallbacks keeping the captures in Perl, and the failing barriers. */
  private val keepBlocks: mutable.Buffer[(Label, Seq[Inst.NonTerminator], Inst.Terminator)] = mutable.Buffer.empty

  /** A buffer of created counter registers. */
  private val counters: mutable.Buffer[CounterReg] = mutable.Buffer.empty

  /** A pool of free counter registers. */
  private val countersPool: mutable.Stack[CounterReg] = mutable.Stack.empty

  /** A buffer of created canary registers. */
  private val canaries: mutable.Buffer[CanaryReg] = mutable.Buffer.empty

  /** A pool of free canary registers. */
  private val canariesPool: mutable.Stack[CanaryReg] = mutable.Stack.empty

  /** A buffer collecting predecessors of a block. */
  private val predecessorsBuffer: mutable.Buffer[mutable.Set[Label]] = mutable.Buffer.empty

  /** A next instruction ID. */
  private var nextInstID: Int = 0

  /** A buffer of processed labels. */
  private val labelsBuffer: mutable.Buffer[Label] = mutable.Buffer.empty

  /** Current processing block's label. It may be `null` when a block is not entered. */
  private var currentLabel: Label = uninitialized

  /** A buffer of current processing block's instructions. */
  private val instsBuffer: mutable.Buffer[Inst.NonTerminator] = mutable.Buffer.empty

  /** A program has a back reference information. */
  private var hasRef: Boolean = false

  /** A program has `verb` instructions. */
  private var hasVerb: Boolean = false

  /** A program has `leave` instructions. */
  private var hasLeave: Boolean = false

  /** Whether a matching direction is backward or not. */
  private var back: Boolean = false

  /** Allocates a new counter register, or reuses a free register. */
  def allocateCounter(): CounterReg =
    if countersPool.nonEmpty then countersPool.pop()
    else
      val index = counters.size
      val reg = CounterReg(index)
      counters.append(reg)
      reg

  /** Marks the counter register is free. */
  def freeCounter(reg: CounterReg): Unit =
    countersPool.push(reg)

  /** Allocates a new canary register, or reuses a free register. */
  def allocateCanary(): CanaryReg =
    if canariesPool.nonEmpty then canariesPool.pop()
    else
      val index = canaries.size
      val reg = CanaryReg(index)
      canaries.append(reg)
      reg

  /** Marks the canary register is free. */
  def freeCanary(reg: CanaryReg): Unit =
    canariesPool.push(reg)

  /** Allocates a new label with an unique ID. */
  def allocateLabel(name: String): Label =
    val index = predecessorsBuffer.size
    predecessorsBuffer.append(mutable.Set.empty)
    Label(name, index)

  /** Starts a block of the label. */
  def enterBlock(label: Label): Unit =
    // $COVERAGE-OFF$
    if (currentLabel ne null) || instsBuffer.nonEmpty then throw IllegalStateException("unterminated block is found")
    if label.block ne null then throw IllegalStateException("a block cannot be re-entered")
    // $COVERAGE-ON$

    currentLabel = label

  /** Adds an instruction to a block. */
  def emitInst(inst: Inst.NonTerminator): Unit =
    // $COVERAGE-OFF$
    if currentLabel eq null then throw IllegalStateException("block is not entered")
    // $COVERAGE-ON$

    // Updates a meta information.
    inst match
      case Inst.Read(_: ReadKind.Ref, _) | _: Inst.CheckNotEmpty => hasRef = true
      case _: Inst.Verb                                          => hasVerb = true
      case _                                                     => // Nothing to do

    instsBuffer.append(inst)

    // Assigns an unique ID to `inst`.
    val id = nextInstID
    nextInstID += 1
    inst.id = id

  /** Adds a terminator instruction to a block, and finishes the block. */
  def emitTerminator(inst: Inst.Terminator): Unit =
    // $COVERAGE-OFF$
    if currentLabel eq null then throw IllegalStateException("block is not entered")
    // $COVERAGE-ON$

    // Updates a predecessor information.
    for successor <- inst.successors do predecessorsBuffer(successor.index).add(currentLabel)
    if inst.isInstanceOf[Inst.IfCap] then hasRef = true
    if inst == Inst.Leave then hasLeave = true

    val block = Block(instsBuffer.toSeq, inst)
    currentLabel.block = block
    labelsBuffer.append(currentLabel)

    currentLabel = null
    instsBuffer.clear()

    // Assigns an unique ID to `inst`.
    val id = nextInstID
    nextInstID += 1
    inst.id = id

  /** Allocates a new label, or reuses the current label if it is empty.
    *
    * It is an utility for an entrance label of a loop, with reducing a label allocation.
    */
  def allocateEntrance(name: String): Label =
    if instsBuffer.isEmpty then currentLabel
    else
      val label = allocateLabel(name)
      emitTerminator(Inst.Jmp(label))
      enterBlock(label)
      label

  /** Returns a built program. */
  def result(): Program =
    for label <- labelsBuffer do
      val block = label.block
      block.terminator match
        case t: Inst.Try =>
          val next = t.next
          if predecessorsBuffer(next.index) == Set(label) then
            next.block.insts.headOption match
              case Some(read: Inst.Read) =>
                label.block = Block(block.insts, Inst.TryLA(read, t.next, t.fallback))
                next.block = Block(next.block.insts.tail, next.block.terminator)
              case Some(read: Inst.ReadBack) =>
                label.block = Block(block.insts, Inst.TryLB(read, t.next, t.fallback))
                next.block = Block(next.block.insts.tail, next.block.terminator)
              case _ => () // nothing to do
        case _ => () // nothing to do

    val blocks = labelsBuffer.iterator.map(label => (label, label.block)).toVector
    val predecessors = predecessorsBuffer.iterator.map(_.toSet).toVector
    val meta = Meta(mode, hasRef, hasVerb, capturesSize, counters.size, canaries.size, predecessors, hasLeave)

    Program(blocks, meta)

  /** Builds a program from the RegExp pattern. */
  def build(): Program =
    val main = allocateLabel("main")
    enterBlock(main)

    // Adds `.*` part if the pattern does not start with `^`.
    if pattern.needsSigmaStarAtBegin then
      val loop = allocateLabel("loop")
      val cont = allocateLabel("cont")

      emitTerminator(if hasCut then Inst.TryBarrier(cont, loop, Barrier.Search) else Inst.Try(cont, loop))

      enterBlock(loop)
      emitInst(Inst.Read(ReadKind.Any, None))
      emitTerminator(Inst.Jmp(main))

      enterBlock(cont)

    emitInst(Inst.CapBegin(0))
    build(pattern.node)
    emitOk()

    for (label, insts, terminator) <- keepBlocks do
      enterBlock(label)
      insts.foreach(emitInst)
      emitTerminator(terminator)

    result()

  /** Emits the end of a successful match, which rejects an empty match with `(*NOTEMPTY)`. */
  def emitOk(): Unit =
    pattern.ir.notEmpty match
      case IRNotEmpty.Never    => ()
      case IRNotEmpty.Anywhere => emitInst(Inst.CheckNotEmpty(false))
      case IRNotEmpty.AtStart  => emitInst(Inst.CheckNotEmpty(true))
    emitInst(Inst.CapEnd(0))
    emitTerminator(Inst.Ok)

  /** Builds a program from a node. */
  def build(node: IRNode): Unit = interrupt:
    node.data match
      case IRNodeData.Disjunction(children)         => buildDisjunction(children)
      case IRNodeData.Sequence(children)            => buildSequence(children)
      case IRNodeData.Empty                         => ()
      case IRNodeData.Capture(index, _, child)      => buildCapture(index, child)
      case IRNodeData.Repeat(child, q)              => buildRepeat(q, child)
      case IRNodeData.Atomic(child)                 => buildAtomic(child)
      case IRNodeData.Assert(kind)                  => emitAssert(assertKind(kind))
      case IRNodeData.LookAhead(false, child)       => wrapLookAhead(buildPositiveLookAround(child))
      case IRNodeData.LookAhead(true, child)        => wrapLookAhead(buildNegativeLookAround(child))
      case IRNodeData.LookBehind(false, child, m)   => buildLookBehind(m)(buildPositiveLookAround(child, _))
      case IRNodeData.LookBehind(true, child, m)    => buildLookBehind(m)(buildNegativeLookAround(child, _))
      case IRNodeData.NonAtomicLookAhead(child)     => wrapLookAhead(buildNonAtomicLookAround(child))
      case IRNodeData.NonAtomicLookBehind(child, m) => buildLookBehind(m)(buildNonAtomicLookAround(child, _))
      case IRNodeData.Conditional(cond, yes, no)    => buildConditional(cond, yes, no)
      // `\K` sets the start of the whole match, which is the capture `0`.
      case IRNodeData.ResetMatchStart        => emitInst(Inst.CapBegin(0))
      case IRNodeData.BacktrackControl(verb) => buildBacktrackControl(verb)
      case IRNodeData.Chars(set)             => emitRead(readKind(set), Location.from(node.loc))
      case IRNodeData.Strings(set)           =>
        val loc = Location.from(node.loc)
        // The strings are tried from the longest, and then the characters.
        val strings = set.strings.toSeq
          .sortBy(-_.length)
          .map: s =>
            () =>
              for c <- if back then s.iterator.toSeq.reverse else s.iterator.toSeq do emitRead(ReadKind.Char(c), loc)
        val chars = Option.when(set.chars.nonEmpty)(() => emitRead(charsKind(set.chars), loc))
        buildAlternatives(strings ++ chars)
      case IRNodeData.BackReference(indices, flags) =>
        val equivalence = IRBuilder.caseEquivalence(pattern.dialect, mode, flags)
        emitRead(ReadKind.Ref(indices, pattern.ir.backReference, equivalence), Location.from(node.loc))
      case IRNodeData.Unsupported(_) => throw new UnsupportedException(pattern.unsupportedMessage(node))

  /** Returns the read kind of a set of a character node. */
  def readKind(set: CharSet): ReadKind =
    if set == mode.universe then ReadKind.Any
    else if prefersComplement(set, mode) then ReadKind.ClassNot(set.complement(mode))
    else charsKind(set)

  /** Returns the read kind of a set, which is a character if it has only one. */
  def charsKind(set: CharSet): ReadKind =
    if set.size == 1 then ReadKind.Char(set.head) else ReadKind.Class(set)

  /** Returns the assertion kind. */
  def assertKind(kind: IRAssertKind): AssertKind = kind match
    case IRAssertKind.WordBoundary(word)    => AssertKind.WordBoundary(word)
    case IRAssertKind.NotWordBoundary(word) => AssertKind.WordBoundaryNot(word)
    case IRAssertKind.LineBegin(t, atEnd)   => AssertKind.LineBegin(t, atEnd)
    case IRAssertKind.LineEnd(t)            => AssertKind.LineEnd(t)
    case IRAssertKind.TextBegin             => AssertKind.InputBegin
    case IRAssertKind.TextEnd               => AssertKind.InputEnd
    case IRAssertKind.ChompTextEnd(t)       => AssertKind.ChompInputEnd(t)

  /** A builder for a disjunction node. */
  def buildDisjunction(children: Seq[IRNode]): Unit = buildDisjunction(children, i => build(children(i)))

  /** A builder for a disjunction node, where `run` builds the alternative of the index.
    *
    * When `(*THEN)` in it takes the next alternative, the fallback of each alternative is a barrier, and so is a
    * failing fallback before them, which `(*THEN)` in the last alternative takes.
    */
  def buildDisjunction(children: Seq[IRNode], run: Int => Unit): Unit =
    val scope = Option.when(children.exists(hasThen))(newThenScope())
    for id <- scope do
      val next = allocateLabel("next")
      emitTerminator(Inst.TryBarrier(next, failLabel(), Barrier.Then(id)))
      enterBlock(next)
    val barrier = scope.map(Barrier.Then(_))
    withThenScope(scope.orElse(thenScope)):
      if perlCaptures then buildPerlDisjunction(children, barrier, run)
      else buildAlternatives(children.indices.map(i => () => run(i)), barrier)

  /** Returns a new scope ID of `(*THEN)`. */
  def newThenScope(): Int =
    val id = nextThenScope
    nextThenScope += 1
    id

  /** Runs with the scope of `(*THEN)`. */
  def withThenScope(scope: Option[Int])(run: => Unit): Unit =
    val saved = thenScope
    thenScope = scope
    run
    thenScope = saved

  /** Allocates a block failing at once, for a barrier only skipped to. */
  def failLabel(): Label =
    val label = allocateLabel("fail")
    keepBlocks.append((label, Seq.empty, Inst.Fail))
    label

  /** Checks whether the node has `(*THEN)` whose scope is the enclosing one, not in an inner alternation or assertion.
    */
  def hasThen(node: IRNode): Boolean = node.data match
    case IRNodeData.BacktrackControl(IRBacktrackControl.Then) => true
    case _: IRNodeData.Disjunction                            => false
    case _: IRNodeData.LookAhead | _: IRNodeData.LookBehind | _: IRNodeData.NonAtomicLookAhead |
        _: IRNodeData.NonAtomicLookBehind =>
      !assertionIsThenScope && node.children.exists(hasThen)
    case IRNodeData.Conditional(condition, yes, no) =>
      val inCondition = condition match
        case IRCondition.LookAhead(_, n)     => !assertionIsThenScope && hasThen(n)
        case IRCondition.LookBehind(_, n, _) => !assertionIsThenScope && hasThen(n)
        case IRCondition.Captured(_)         => false
      inCondition || hasThen(yes) || hasThen(no)
    case _ => node.children.exists(hasThen)

  /** Returns the scope of `(*THEN)` that an assertion makes, if it has `(*THEN)` in it. */
  def assertionThenScope(child: IRNode): Option[Int] =
    Option.when(assertionIsThenScope && hasThen(child))(newThenScope())

  /** A builder for a disjunction node of Perl, which unsets the captures of an alternative when it fails. */
  def buildPerlDisjunction(children: Seq[IRNode], barrier: Option[Barrier], run: Int => Unit): Unit =
    val labels = children.map(_ => (allocateLabel("left"), allocateLabel("right")))
    val cont = allocateLabel("cont")

    for ((child, (l, r)), i) <- children.zip(labels).zipWithIndex do
      val last = i == children.size - 1
      val captures = child.captureRange.range
      if last && captures.isEmpty then
        run(i)
        emitTerminator(Inst.Jmp(cont))
      else
        emitTry(l, allocateKeep(None, captures, Option.when(!last)(r)), barrier.filter(_ => !last))

        enterBlock(l)
        run(i)
        emitTerminator(Inst.Jmp(cont))

        if !last then enterBlock(r)

    enterBlock(cont)

  /** Returns how Perl repeats the node by counting (`CURLY`, `CURLYN` and `CURLYM`), if it is a fixed-length node
    * without captures in it, with the index of the capture of the node itself.
    */
  def perlCountedRepeat(child: IRNode): Option[Option[Int]] =
    val (index, body) = child.data match
      case IRNodeData.Capture(i, _, n) => (Some(i), n)
      case _                           => (None, child)
    Option.when(body.captureRange.range.isEmpty && fixedWidth(body).exists(_ > 0))(index)

  /** Returns the width of the node if it is fixed. */
  def fixedWidth(node: IRNode): Option[Int] = node.data match
    case IRNodeData.Empty | _: IRNodeData.Assert | _: IRNodeData.LookAhead | _: IRNodeData.LookBehind => Some(0)
    case _: IRNodeData.Chars                                                                          => Some(1)
    case IRNodeData.Sequence(ns)    => ns.foldLeft(Option(0))((w, n) => w.zip(fixedWidth(n)).map(_ + _))
    case IRNodeData.Disjunction(ns) =>
      ns.map(fixedWidth).distinct match
        case Seq(w) => w
        case _      => None
    case IRNodeData.Capture(_, _, n)                 => fixedWidth(n)
    case IRNodeData.Atomic(n)                        => fixedWidth(n)
    case IRNodeData.Repeat(n, IRQuantifier.Exact(m)) => fixedWidth(n).map(_ * m)
    case _                                           => None

  /** Allocates a block for a fallback in Perl, which keeps the captures of the failed matching except `restore`, and
    * unsets `reset`, then continues to `next` or fails.
    */
  def allocateKeep(restore: Option[(Int, Int)], reset: Option[(Int, Int)], next: Option[Label]): Label =
    val label = allocateLabel("keep")
    val insts = Inst.CapKeep(restore) +: reset.map((from, to) => Inst.CapReset(from, to)).toSeq
    keepBlocks.append((label, insts, next.fold[Inst.Terminator](Inst.Fail)(Inst.Jmp(_))))
    label

  /** Returns the fallback of a `try` to the label, which goes through a block keeping the captures in Perl. */
  def fallbackOf(label: Label, keep: Keep): Label =
    keep.fold(label)(restore => allocateKeep(restore, None, Some(label)))

  /** Emits `try`, or `try_barrier` with the barrier. */
  def emitTry(next: Label, fallback: Label, barrier: Option[Barrier]): Unit =
    emitTerminator(barrier.fold(Inst.Try(next, fallback))(Inst.TryBarrier(next, fallback, _)))

  /** A builder for alternatives tried in the order, where the fallback of each is the barrier. */
  def buildAlternatives(alternatives: Seq[() => Unit], barrier: Option[Barrier] = None): Unit =
    val labels = alternatives.init.map(_ => (allocateLabel("left"), allocateLabel("right")))
    val cont = allocateLabel("cont")

    for (run, (l, r)) <- alternatives.init.zip(labels) do
      emitTry(l, r, barrier)

      enterBlock(l)
      run()
      emitTerminator(Inst.Jmp(cont))

      enterBlock(r)

    alternatives.last()
    emitTerminator(Inst.Jmp(cont))

    enterBlock(cont)

  /** A builder for a sequence of nodes. */
  def buildSequence(children: Seq[IRNode]): Unit =
    for n <- if back then children.reverse else children do build(n)

  /** A builder for a capture node. */
  def buildCapture(index: Int, children: IRNode): Unit =
    emitInst(if back then Inst.CapEnd(index) else Inst.CapBegin(index))
    val saved = openCaptures
    openCaptures = index :: openCaptures
    build(children)
    openCaptures = saved
    emitInst(if back then Inst.CapBegin(index) else Inst.CapEnd(index))

  /** A builder for `x*` node. */
  def buildStar(isLazy: Boolean, child: IRNode, keep: Keep = None, barrier: Boolean = false): Unit =
    val begin = allocateEntrance("begin")
    val loop = allocateLabel("loop")
    val cont = allocateLabel("cont")

    enterLoopBlock(isLazy, loop, cont, keep)
    wrapCanary(child)(wrapReset(child)(buildIteration(child, isLazy && barrier)))
    emitTerminator(Inst.Jmp(begin))

    enterBlock(cont)

  /** A builder for `x+` node. */
  def buildPlus(isLazy: Boolean, child: IRNode, keep: Keep = None, barrier: Boolean = false): Unit =
    buildIteration(child, barrier)
    buildStar(isLazy, child, keep, barrier)

  /** A builder for `x?` node. */
  def buildQuestion(isLazy: Boolean, child: IRNode, keep: Keep = None, barrier: Boolean = false): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(
      if isLazy then Inst.Try(cont, fallbackOf(main, keep)) else Inst.Try(main, fallbackOf(cont, keep))
    )

    enterBlock(main)
    wrapReset(child)(buildIteration(child, isLazy && barrier))
    emitTerminator(Inst.Jmp(cont))

    enterBlock(cont)

  /** A builder for `x{n,m}` node. */
  def buildRepeat(quantifier: IRQuantifier, child: IRNode): Unit =
    def isLazy(strategy: BacktrackStrategy): Boolean = strategy == BacktrackStrategy.Lazy
    val counted = if perlCaptures then perlCountedRepeat(child) else None
    val keep = counted.map(_.map(i => (i, i)))
    val barrier = perlCaptures && counted.isEmpty
    val min = quantifier match
      case IRQuantifier.Exact(n)         => n
      case IRQuantifier.Unbounded(n, _)  => n
      case IRQuantifier.Bounded(n, _, _) => n
    // Perl unsets the capture of a counted repetition matching nothing.
    for case Some(i) <- counted if min == 0 do emitInst(Inst.CapReset(i, i))
    quantifier match
      case IRQuantifier.Exact(0)               => // nothing to do
      case IRQuantifier.Exact(1)               => buildIteration(child, barrier)
      case IRQuantifier.Exact(n)               => buildRepeatN(n, child, barrier)
      case IRQuantifier.Unbounded(0, strategy) => buildStar(isLazy(strategy), child, keep, barrier)
      case IRQuantifier.Unbounded(1, strategy) => buildPlus(isLazy(strategy), child, keep, barrier)
      case IRQuantifier.Unbounded(n, strategy) =>
        buildRepeatN(n, child, barrier)
        buildStar(isLazy(strategy), child, keep, barrier)
      case IRQuantifier.Bounded(0, 1, strategy) => buildQuestion(isLazy(strategy), child, keep, barrier)
      case IRQuantifier.Bounded(0, n, strategy) => buildRepeatAtMostN(isLazy(strategy), n, child, keep, barrier)
      case IRQuantifier.Bounded(1, 2, strategy) =>
        buildIteration(child, barrier)
        buildQuestion(isLazy(strategy), child, keep, barrier)
      case IRQuantifier.Bounded(1, m, strategy) =>
        buildIteration(child, barrier)
        buildRepeatAtMostN(isLazy(strategy), m - 1, child, keep, barrier)
      case IRQuantifier.Bounded(n, m, strategy) if m - n == 1 =>
        buildRepeatN(n, child, barrier)
        buildQuestion(isLazy(strategy), child, keep, barrier)
      case IRQuantifier.Bounded(n, m, strategy) =>
        buildRepeatN(n, child, barrier)
        buildRepeatAtMostN(isLazy(strategy), m - n, child, keep, barrier)

  /** A builder for `x{n}` node. */
  def buildRepeatN(n: Int, child: IRNode, barrier: Boolean = false): Unit =
    val loop = allocateEntrance("loop")
    val cont = allocateLabel("cont")

    val reg = allocateCounter()

    // Note that `wrapCanary` is not needed.
    wrapReset(child)(buildIteration(child, barrier))
    closeLoopBlock(reg, n, loop, cont)

  /** A builder for `x{0,n}` node. */
  def buildRepeatAtMostN(isLazy: Boolean, n: Int, child: IRNode, keep: Keep = None, barrier: Boolean = false): Unit =
    val begin = allocateEntrance("begin")
    val loop = allocateLabel("loop")
    val cont = allocateLabel("cont")

    val reg = allocateCounter()

    enterLoopBlock(isLazy, loop, cont, keep)
    wrapCanary(child)(wrapReset(child)(buildIteration(child, isLazy && barrier)))
    closeLoopBlock(reg, n, begin, cont)

  /** Builds an iteration of a repetition, which Perl wraps to restore the captures when the iteration fails.
    *
    * A greedy optional iteration needs no wrapping, since its fallback restores them.
    */
  def buildIteration(child: IRNode, barrier: Boolean): Unit =
    if barrier then
      val next = allocateLabel("next")
      val restore = allocateLabel("restore")
      emitTerminator(Inst.Try(next, restore))
      enterBlock(restore)
      emitTerminator(Inst.Fail)
      enterBlock(next)
    build(child)

  /** Emits a loop block entering instructions. */
  def enterLoopBlock(isLazy: Boolean, loop: Label, cont: Label, keep: Keep = None): Unit =
    emitTerminator(
      if isLazy then Inst.Try(cont, fallbackOf(loop, keep)) else Inst.Try(loop, fallbackOf(cont, keep))
    )
    enterBlock(loop)

  /** Emits a loop block closing instructions. */
  def closeLoopBlock(reg: CounterReg, n: Int, begin: Label, cont: Label): Unit =
    emitInst(Inst.Inc(reg))
    emitTerminator(Inst.Cmp(reg, n, begin, cont))

    enterBlock(cont)
    emitInst(Inst.Reset(reg)) // A counter should be reset after the loop for working memoization well.

    freeCounter(reg)

  /** Inserts a canary around a loop body if needed. */
  def wrapCanary(child: IRNode)(run: => Unit): Unit =
    if child.canMatchEmpty then
      val reg = allocateCanary()
      emitInst(Inst.SetCanary(reg))
      run
      emitInst(Inst.CheckCanary(reg))
      freeCanary(reg)
    else run

  /** Emits a `cap-reset` if captures are reset on each iteration, then execute a `run`. */
  def wrapReset(child: IRNode)(run: => Unit): Unit =
    if resetsCaptures then for (min, max) <- child.captureRange.range do emitInst(Inst.CapReset(min, max))
    run

  /** A builder for an atomic group, which commits the first match of the child.
    *
    * Perl runs an atomic group as a sub-match, which `(*ACCEPT)` in it ends.
    */
  def buildAtomic(child: IRNode): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(Inst.Tx(main, Some(cont), None))

    enterBlock(main)
    if pattern.dialect == Dialect.Perl then withAcceptScope(AcceptScope.Atomic)(build(child))
    else
      atomicDepth += 1
      build(child)
      atomicDepth -= 1
    emitHold(child)
    emitTerminator(Inst.Commit)

    enterBlock(cont)

  /** Emits `cap_hold` of the captures in the node, if they are held over backtracking. */
  def emitHold(node: IRNode): Unit =
    if holdsCaptures then for (from, to) <- node.captureRange.range do emitInst(Inst.CapHold(from, to))

  /** Runs with the scope of `(*ACCEPT)`. */
  def withAcceptScope(scope: AcceptScope)(run: => Unit): Unit =
    val saved = (acceptScope, openCaptures, atomicDepth)
    acceptScope = scope
    openCaptures = Nil
    atomicDepth = 0
    run
    acceptScope = saved._1
    openCaptures = saved._2
    atomicDepth = saved._3

  /** Builds the body of an assertion, which is the scope of `(*ACCEPT)` and maybe of `(*THEN)`. */
  def buildAssertion(child: IRNode, scope: Option[Int], body: IRNode => Unit): Unit =
    withThenScope(scope.orElse(thenScope))(withAcceptScope(AcceptScope.LookAround)(body(child)))

  /** A builder for positive look-around assertion.
    *
    * With `(*THEN)` in it, a failing fallback is the barrier, which makes the assertion fail.
    */
  def buildPositiveLookAround(child: IRNode, body: IRNode => Unit = build): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")
    val scope = assertionThenScope(child)

    emitTerminator(Inst.Tx(main, Some(cont), scope.map(_ => failLabel()), scope.map(Barrier.Then(_))))

    enterBlock(main)
    buildAssertion(child, scope, body)
    emitHold(child)
    emitTerminator(Inst.Rollback)

    enterBlock(cont)

  /** A builder for negative look-around assertion, whose fallback makes the assertion succeed. */
  def buildNegativeLookAround(child: IRNode, body: IRNode => Unit = build): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")
    val scope = assertionThenScope(child)

    emitTerminator(Inst.Tx(main, None, Some(cont), Option.when(hasCut)(Barrier.Assertion(scope))))

    enterBlock(main)
    buildAssertion(child, scope, body)
    emitHold(child)
    emitTerminator(Inst.Rollback)

    enterBlock(cont)

  /** A builder for non-atomic positive look-around assertion, which leaves the backtracking points in it.
    *
    * `(*ACCEPT)` in it ends it atomically.
    */
  def buildNonAtomicLookAround(child: IRNode, body: IRNode => Unit = build): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")
    val scope = assertionThenScope(child)

    emitTerminator(Inst.Tx(main, Some(cont), scope.map(_ => failLabel()), scope.map(Barrier.Then(_))))

    enterBlock(main)
    buildAssertion(child, scope, body)
    emitTerminator(Inst.Leave)

    enterBlock(cont)

  /** A builder for a conditional.
    *
    * A look-around condition is atomic, and it keeps the captures in it even if it is negative, except in .NET, which
    * restores them as a negative look-around does. Its fallback is the barrier, so `(*COMMIT)`, `(*PRUNE)` and
    * `(*SKIP)` in it make it false (or true for a negative one).
    */
  def buildConditional(condition: IRCondition, yes: IRNode, no: IRNode): Unit =
    val yesLabel = allocateLabel("yes")
    val noLabel = allocateLabel("no")
    val cont = allocateLabel("cont")

    def buildTest(negative: Boolean, child: IRNode, body: IRNode => Unit): Unit =
      val main = allocateLabel("main")
      val scope = assertionThenScope(child)
      val restores = negative && pattern.dialect == Dialect.DotNet
      val (rollback, fallback) = if negative && !restores then (noLabel, yesLabel) else (yesLabel, noLabel)
      emitTerminator(Inst.Tx(main, Some(rollback), Some(fallback), Option.when(hasCut)(Barrier.Assertion(scope))))
      enterBlock(main)
      if restores then buildNegativeLookAround(child, body) else buildAssertion(child, scope, body)
      emitTerminator(Inst.Rollback)

    // The register of the end of a look-behind matched forward, which is reset after the condition.
    var end: Option[CounterReg] = None
    condition match
      case IRCondition.Captured(indices)      => emitTerminator(Inst.IfCap(indices, yesLabel, noLabel))
      case IRCondition.LookAhead(negative, n) => wrapLookAhead(buildTest(negative, n, build))
      case IRCondition.LookBehind(negative, n, IRLookBehindMatch.Backward) =>
        wrapLookBehind(buildTest(negative, n, build))
      case IRCondition.LookBehind(negative, n, IRLookBehindMatch.Forward(starts)) =>
        val reg = allocateCounter()
        end = Some(reg)
        wrapLookAhead(buildTest(negative, n, buildForwardBody(_, starts, reg)))

    for (label, branch) <- Seq((yesLabel, yes), (noLabel, no)) do
      enterBlock(label)
      for reg <- end do emitInst(Inst.Reset(reg))
      build(branch)
      emitTerminator(Inst.Jmp(cont))

    enterBlock(cont)
    end.foreach(freeCounter)

  /** A builder for a backtracking control verb. */
  def buildBacktrackControl(verb: IRBacktrackControl): Unit = verb match
    case IRBacktrackControl.Accept     => buildAccept()
    case IRBacktrackControl.Commit     => emitInst(Inst.Verb(VerbKind.Commit))
    case IRBacktrackControl.Prune      => emitInst(Inst.Verb(VerbKind.Prune))
    case IRBacktrackControl.Skip(name) => emitInst(Inst.Verb(VerbKind.Skip(name)))
    case IRBacktrackControl.Then       => emitInst(Inst.Verb(VerbKind.Then(thenScope)))
    // A mark has no effect on matching without `(*SKIP)` of the name.
    case IRBacktrackControl.Mark(name) => if skipNames.contains(name) then emitInst(Inst.Verb(VerbKind.Mark(name)))

  /** A builder for `(*ACCEPT)`, which closes the open captures and ends its scope successfully.
    *
    * In a look-behind, which PCRE and Perl match forward, it ends the look-behind before reaching the position.
    */
  def buildAccept(): Unit =
    for index <- openCaptures do emitInst(Inst.CapEnd(index))
    acceptScope match
      case AcceptScope.Top        => emitOk()
      case AcceptScope.LookAround =>
        for _ <- 0 until atomicDepth do emitInst(Inst.TxDrop)
        emitTerminator(Inst.Rollback)
      case AcceptScope.Atomic =>
        for _ <- 0 until atomicDepth do emitInst(Inst.TxDrop)
        emitTerminator(Inst.Commit)
    // The rest of the sequence is not reachable.
    enterBlock(allocateLabel("accepted"))

  /** Sets a matching direction fot look-ahead assertion. */
  def wrapLookAhead(run: => Unit): Unit =
    val oldBack = back
    back = false
    run
    back = oldBack

  /** Builds a look-behind with the builder of the assertion, which takes the builder of the body.
    *
    * The body matches backward, or forward from the starts that the engine tries before the position.
    */
  def buildLookBehind(matching: IRLookBehindMatch)(run: (IRNode => Unit) => Unit): Unit =
    matching match
      case IRLookBehindMatch.Backward        => wrapLookBehind(run(build))
      case IRLookBehindMatch.Forward(starts) =>
        val end = allocateCounter()
        wrapLookAhead(run(buildForwardBody(_, starts, end)))
        emitInst(Inst.Reset(end))
        freeCounter(end)

  /** Builds the body of a look-behind matched forward from each start, which must end at the position held by `end`.
    *
    * With the starts of each alternative, the body is a disjunction of them.
    */
  def buildForwardBody(child: IRNode, starts: Seq[IRLookBehindStarts], end: CounterReg): Unit =
    def attempt(node: IRNode, starts: IRLookBehindStarts): Unit =
      val start = allocateLabel("start")
      emitTerminator(Inst.StepBack(end, starts, start))
      enterBlock(start)
      build(node)
      emitAssert(AssertKind.LookBehindEnd(end))
    (starts, child.data) match
      case (Seq(s), _)                                                     => attempt(child, s)
      case (_, IRNodeData.Disjunction(nodes)) if nodes.size == starts.size =>
        buildDisjunction(nodes, i => attempt(nodes(i), starts(i)))
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException("look-behind starts do not match the alternatives")
      // $COVERAGE-ON$

  /** Sets a matching direction fot look-behind assertion. */
  def wrapLookBehind(run: => Unit): Unit =
    val oldBack = back
    back = true
    run
    back = oldBack

  /** Adds an `assert` instruction. */
  def emitAssert(kind: AssertKind): Unit =
    emitInst(Inst.Assert(kind))

  /** Adds a `read` (or `read_back`) instruction. */
  def emitRead(kind: ReadKind, loc: Option[Location]): Unit =
    emitInst(if back then Inst.ReadBack(kind, loc) else Inst.Read(kind, loc))
