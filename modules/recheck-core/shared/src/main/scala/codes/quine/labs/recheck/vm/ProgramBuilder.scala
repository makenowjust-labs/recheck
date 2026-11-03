package codes.quine.labs.recheck.vm

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.util.Try

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.vm.Inst.AssertKind
import codes.quine.labs.recheck.vm.Inst.Barrier
import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.recheck.vm.Inst.VerbKind
import codes.quine.labs.recheck.vm.Program.Meta
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRAssertKind
import codes.quine.labs.resyntax.ir.IRBacktrackControl
import codes.quine.labs.resyntax.ir.IRCondition
import codes.quine.labs.resyntax.ir.IREmptyIteration
import codes.quine.labs.resyntax.ir.IRLookBehindMatch
import codes.quine.labs.resyntax.ir.IRLookBehindStarts
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRNotEmpty
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.IRQuantifier
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** ProgramBuilder is a builder of a program from a RegExp pattern. */
object ProgramBuilder:

  /** Builds a corresponding program from the RegExp pattern, which runs as the engine of its dialect. */
  def build(pattern: IRPattern)(using ctx: Context): Try[Program] =
    for
      _ <- Try(())
      capturesSize = pattern.captureCount
      builder = new ProgramBuilder(pattern, capturesSize)
      program <- Try(builder.build())
    yield program

/** AcceptScope is what `(*ACCEPT)` ends successfully. */
private[vm] enum AcceptScope:

  /** The whole match. */
  case Top

  /** A look-around assertion or a condition, which succeeds (or fails for a negative one). */
  case LookAround

  /** An atomic group, which the engine runs as a sub-match. */
  case Atomic

private[vm] class ProgramBuilder(
    private val pattern: IRPattern,
    private val capturesSize: Int
)(using ctx: Context):
  import ctx._

  private val universe = pattern.universe

  /** Whether captures in a repetition are reset on each iteration. */
  private val resetsCaptures = pattern.captures.resetsEachIteration

  /** Whether backtracking keeps the captures of the failed matching, which back-references and capture tests only see.
    */
  private val keepsCaptures =
    pattern.captures.keptOnBacktracking && pattern.node.exists:
      case _: IRNodeData.BackReference                           => true
      case IRNodeData.Conditional(_: IRCondition.Captured, _, _) => true
      case _                                                     => false

  /** Whether the captures in an atomic group or a look-around are held over backtracking once it matches, which
    * back-references only see.
    */
  private val holdsCaptures =
    pattern.captures.heldAfterAtomicMatch && pattern.node.exists(_.isInstanceOf[IRNodeData.BackReference])

  /** Whether `(*THEN)` ends at a look-around assertion or a condition. */
  private val assertionIsThenScope = !pattern.backtrackControl.thenPassesAssertions

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

  private var nextThenScope: Int = 0

  /** The scope that `(*ACCEPT)` at the current position ends. */
  private var acceptScope: AcceptScope = AcceptScope.Top

  /** The captures open in [[acceptScope]], the innermost first, which `(*ACCEPT)` closes. */
  private var openCaptures: List[Int] = Nil

  /** The number of the atomic groups in [[acceptScope]], whose transactions `(*ACCEPT)` drops. */
  private var atomicDepth: Int = 0

  /** The blocks emitted after the main blocks, which the main flow only jumps or falls back to. */
  private val trailingBlocks: mutable.Buffer[(Label, Seq[Inst.NonTerminator], Inst.Terminator)] = mutable.Buffer.empty

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

  /** Whether the program reads the captures. */
  private var hasRef: Boolean = false

  /** Whether the program has `verb` instructions. */
  private var hasVerb: Boolean = false

  /** Whether the program has `leave` instructions. */
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

  /** Marks the counter register as free. */
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

  /** Marks the canary register as free. */
  def freeCanary(reg: CanaryReg): Unit =
    canariesPool.push(reg)

  /** Allocates a new label with a unique ID. */
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
      case Inst.Read(_: ReadKind.Ref, _) | _: Inst.RejectEmpty => hasRef = true
      case _: Inst.Verb                                        => hasVerb = true
      case _                                                   => // Nothing to do

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
    val meta = Meta(hasRef, hasVerb, hasLeave, capturesSize, counters.size, canaries.size, predecessors)

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

    for (label, insts, terminator) <- trailingBlocks do
      enterBlock(label)
      insts.foreach(emitInst)
      emitTerminator(terminator)

    result()

  /** Emits the end of a successful match, which rejects an empty match with `(*NOTEMPTY)`. */
  def emitOk(): Unit =
    pattern.notEmpty match
      case IRNotEmpty.Never    => ()
      case IRNotEmpty.Anywhere => emitInst(Inst.RejectEmpty(false))
      case IRNotEmpty.AtStart  => emitInst(Inst.RejectEmpty(true))
    emitInst(Inst.CapEnd(0))
    emitTerminator(Inst.Ok)

  /** Builds a program from a node. */
  def build(node: IRNode): Unit = interrupt:
    node.data match
      case IRNodeData.Disjunction(children) =>
        val alternatives = children.toIndexedSeq
        buildDisjunction(alternatives, i => build(alternatives(i)))
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
      case IRNodeData.Chars(set)             => emitRead(readKind(set), node.loc.toOption)
      case IRNodeData.Strings(set)           =>
        val loc = node.loc.toOption
        // The strings are tried from the longest, and then the characters.
        val strings = set.strings.toSeq
          .sortBy(-_.length)
          .map: s =>
            () =>
              for c <- if back then s.iterator.toSeq.reverse else s.iterator.toSeq do emitRead(ReadKind.Char(c), loc)
        val chars = Option.when(set.chars.nonEmpty)(() => emitRead(charsKind(set.chars), loc))
        buildAlternatives(strings ++ chars)
      case IRNodeData.BackReference(indices, equivalence, folding) =>
        emitRead(ReadKind.Ref(indices, pattern.backReference, equivalence, folding), node.loc.toOption)
      case IRNodeData.Unsupported(_) => throw new UnsupportedException(node.unsupportedMessage)

  /** Returns the read kind of a set of a character node. */
  def readKind(set: CharSet): ReadKind =
    if set == universe then ReadKind.Any
    else if PatternAlphabet.prefersComplement(set, universe) then ReadKind.ClassNot(set.complement(universe))
    else charsKind(set)

  def charsKind(set: CharSet): ReadKind =
    if set.size == 1 then ReadKind.Char(set.head) else ReadKind.Class(set)

  def assertKind(kind: IRAssertKind): AssertKind = kind match
    case IRAssertKind.WordBoundary(word)               => AssertKind.WordBoundary(word)
    case IRAssertKind.NotWordBoundary(word)            => AssertKind.WordBoundaryNot(word)
    case IRAssertKind.MarkWordBoundary(word, marks)    => AssertKind.MarkWordBoundary(word, marks)
    case IRAssertKind.NotMarkWordBoundary(word, marks) => AssertKind.MarkWordBoundaryNot(word, marks)
    case IRAssertKind.LineBegin(t, atEnd)              => AssertKind.LineBegin(t, atEnd)
    case IRAssertKind.LineEnd(t)                       => AssertKind.LineEnd(t)
    case IRAssertKind.TextBegin                        => AssertKind.InputBegin
    case IRAssertKind.TextEnd                          => AssertKind.InputEnd
    case IRAssertKind.ChompTextEnd(t)                  => AssertKind.ChompInputEnd(t)
    case IRAssertKind.NotBetweenCRLF                   => AssertKind.NotBetweenCRLF

  /** A builder for a disjunction node, where `run` builds the alternative of the index. */
  def buildDisjunction(children: IndexedSeq[IRNode], run: Int => Unit): Unit =
    // When `(*THEN)` in it takes the next alternative, the fallback of each alternative is a barrier, and so is a
    // failing fallback before them, which `(*THEN)` in the last alternative takes.
    val scope = Option.when(children.exists(hasThen))(newThenScope())
    for id <- scope do
      val next = allocateLabel("next")
      emitTerminator(Inst.TryBarrier(next, failLabel(), Barrier.Then(id)))
      enterBlock(next)
    val barrier = scope.map(Barrier.Then(_))
    withThenScope(scope.orElse(thenScope)):
      if keepsCaptures then buildKeepingDisjunction(children, barrier, run)
      else buildAlternatives(children.indices.map(i => () => run(i)), barrier)

  def newThenScope(): Int =
    val id = nextThenScope
    nextThenScope += 1
    id

  def withThenScope(scope: Option[Int])(run: => Unit): Unit =
    val saved = thenScope
    thenScope = scope
    run
    thenScope = saved

  /** Allocates a block failing at once, for a barrier only skipped to. */
  def failLabel(): Label =
    val label = allocateLabel("fail")
    trailingBlocks.append((label, Seq.empty, Inst.Fail))
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

  /** A builder for a disjunction node keeping the captures on backtracking, which unsets the captures of an alternative
    * when it fails.
    */
  def buildKeepingDisjunction(children: IndexedSeq[IRNode], barrier: Option[Barrier], run: Int => Unit): Unit =
    val labels = children.map(_ => (allocateLabel("left"), allocateLabel("right")))
    val cont = allocateLabel("cont")

    for ((child, (l, r)), i) <- children.zip(labels).zipWithIndex do
      val last = i == children.size - 1
      val captures = child.captureRange
      if last && captures.isEmpty then
        run(i)
        emitTerminator(Inst.Jmp(cont))
      else
        emitTry(l, allocateKeep(captures, Option.when(!last)(r)), barrier.filter(_ => !last))

        enterBlock(l)
        run(i)
        emitTerminator(Inst.Jmp(cont))

        if !last then enterBlock(r)

    enterBlock(cont)

  /** Allocates a block for a fallback keeping the captures on backtracking, which keeps the captures of the failed
    * matching, and unsets `reset`, then continues to `next` or fails.
    */
  def allocateKeep(reset: Option[(Int, Int)], next: Option[Label]): Label =
    val label = allocateLabel("keep")
    val insts = Inst.CapKeep +: reset.map((from, to) => Inst.CapReset(from, to)).toSeq
    trailingBlocks.append((label, insts, next.fold[Inst.Terminator](Inst.Fail)(Inst.Jmp(_))))
    label

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
  def buildStar(
      isLazy: Boolean,
      child: IRNode,
      empty: IREmptyIteration
  ): Unit =
    val begin = allocateEntrance("begin")
    val loop = allocateLabel("loop")
    val cont = allocateLabel("cont")

    enterLoopBlock(isLazy, loop, cont)
    wrapCanary(child, empty, cont)(wrapReset(child)(buildIteration(child, isLazy && keepsCaptures)))
    emitTerminator(Inst.Jmp(begin))

    enterBlock(cont)

  /** A builder for `x?` node, whose iteration is optional. */
  def buildQuestion(
      isLazy: Boolean,
      child: IRNode,
      empty: IREmptyIteration
  ): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(if isLazy then Inst.Try(cont, main) else Inst.Try(main, cont))

    enterBlock(main)
    wrapCanary(child, empty, cont)(wrapReset(child)(buildIteration(child, isLazy && keepsCaptures)))
    emitTerminator(Inst.Jmp(cont))

    enterBlock(cont)

  /** A builder for `x{n,m}` node. */
  def buildRepeat(quantifier: IRQuantifier, child: IRNode): Unit =
    def isLazy(strategy: BacktrackStrategy): Boolean = strategy == BacktrackStrategy.Lazy

    val empty = if child.canMatchEmpty then emptyIteration(quantifier) else IREmptyIteration.Ignore
    // An empty mandatory iteration ending the repetition jumps to `exit`.
    val exit = allocateLabel("exit")
    var exits = false
    def mandatory(n: Int): Unit =
      empty match
        case IREmptyIteration.ExitAll =>
          exits = true
          buildRepeatN(n, child, Some(exit))
        case IREmptyIteration.ExitFromMin if n > 0 =>
          exits = true
          buildRepeatN(n - 1, child)
          wrapCanary(child, IREmptyIteration.ExitFromMin, exit)(wrapReset(child)(buildIteration(child)))
        case _ => buildRepeatN(n, child)

    quantifier match
      case IRQuantifier.Exact(n) if empty == IREmptyIteration.ExitAll => mandatory(n)
      case IRQuantifier.Exact(n)                                      => buildRepeatN(n, child)
      case IRQuantifier.Unbounded(n, strategy)                        =>
        mandatory(n)
        buildStar(isLazy(strategy), child, empty)
      case IRQuantifier.Bounded(n, m, strategy) =>
        mandatory(n)
        if m - n == 1 then buildQuestion(isLazy(strategy), child, empty)
        else buildRepeatAtMostN(isLazy(strategy), m - n, child, empty)
    if exits then
      emitTerminator(Inst.Jmp(exit))
      enterBlock(exit)

  def emptyIteration(quantifier: IRQuantifier): IREmptyIteration = quantifier match
    case _: IRQuantifier.Unbounded => pattern.emptyIteration.unbounded
    case _                         => pattern.emptyIteration.bounded

  /** A builder for `x{n}` node, where an empty iteration jumps to `exit` if it is given. */
  def buildRepeatN(n: Int, child: IRNode, exit: Option[Label] = None): Unit =
    if n == 1 then
      exit match
        case Some(exit) =>
          wrapCanary(child, IREmptyIteration.ExitAll, exit)(wrapReset(child)(buildIteration(child)))
        case None => buildIteration(child)
    else if n > 1 then
      val loop = allocateEntrance("loop")
      val cont = allocateLabel("cont")

      val reg = allocateCounter()

      exit match
        case Some(exit) =>
          val leave = allocateLabel("leave")
          trailingBlocks.append((leave, Seq(Inst.Reset(reg)), Inst.Jmp(exit)))
          wrapCanary(child, IREmptyIteration.ExitAll, leave)(wrapReset(child)(buildIteration(child)))
        case None => wrapReset(child)(buildIteration(child))
      closeLoopBlock(reg, n, loop, cont)

  /** A builder for `x{0,n}` node. */
  def buildRepeatAtMostN(
      isLazy: Boolean,
      n: Int,
      child: IRNode,
      empty: IREmptyIteration
  ): Unit =
    val begin = allocateEntrance("begin")
    val loop = allocateLabel("loop")
    val cont = allocateLabel("cont")

    val reg = allocateCounter()

    enterLoopBlock(isLazy, loop, cont)
    wrapCanary(child, empty, cont)(wrapReset(child)(buildIteration(child, isLazy && keepsCaptures)))
    closeLoopBlock(reg, n, begin, cont)

  /** Builds an iteration of a repetition, which is wrapped to restore the captures when the iteration fails if
    * backtracking keeps them.
    *
    * A greedy optional iteration needs no wrapping, since its fallback restores them.
    */
  def buildIteration(child: IRNode, wraps: Boolean = keepsCaptures): Unit =
    if wraps then
      val next = allocateLabel("next")
      val restore = allocateLabel("restore")
      emitTerminator(Inst.Try(next, restore))
      enterBlock(restore)
      emitTerminator(Inst.Fail)
      enterBlock(next)
    build(child)

  /** Emits a loop block entering instructions. */
  def enterLoopBlock(isLazy: Boolean, loop: Label, cont: Label): Unit =
    emitTerminator(if isLazy then Inst.Try(cont, loop) else Inst.Try(loop, cont))
    enterBlock(loop)

  /** Emits a loop block closing instructions. */
  def closeLoopBlock(reg: CounterReg, n: Int, begin: Label, cont: Label): Unit =
    emitInst(Inst.Inc(reg))
    emitTerminator(Inst.Cmp(reg, n, begin, cont))

    enterBlock(cont)
    emitInst(Inst.Reset(reg)) // A counter should be reset after the loop for working memoization well.

    freeCounter(reg)

  /** Inserts a canary around an iteration if needed, where an empty iteration fails, or jumps to `exit`. */
  def wrapCanary(child: IRNode, empty: IREmptyIteration, exit: Label)(run: => Unit): Unit =
    if child.canMatchEmpty && empty != IREmptyIteration.Ignore then
      val reg = allocateCanary()
      emitInst(Inst.SetCanary(reg))
      run
      if empty == IREmptyIteration.Reject then emitInst(Inst.RejectEmptyIteration(reg))
      else
        val next = allocateLabel("next")
        emitTerminator(Inst.CmpCanary(reg, exit, next))
        enterBlock(next)
      freeCanary(reg)
    else run

  /** Emits a `cap-reset` if captures are reset on each iteration, and then runs `run`. */
  def wrapReset(child: IRNode)(run: => Unit): Unit =
    if resetsCaptures then for (min, max) <- child.captureRange do emitInst(Inst.CapReset(min, max))
    run

  /** A builder for an atomic group, which commits the first match of the child. */
  def buildAtomic(child: IRNode): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(Inst.Tx(main, Some(cont), None))

    enterBlock(main)
    // When `(*ACCEPT)` ends only an atomic group, the engine runs it as a sub-match.
    if pattern.backtrackControl.acceptEndsAtomic then withAcceptScope(AcceptScope.Atomic)(build(child))
    else
      atomicDepth += 1
      build(child)
      atomicDepth -= 1
    emitHold(child)
    emitTerminator(Inst.Commit)

    enterBlock(cont)

  /** Emits `cap_hold` of the captures in the node, if they are held over backtracking. */
  def emitHold(node: IRNode): Unit =
    // Java does not restore the captures in an atomic group or a look-around, once it matches.
    if holdsCaptures then for (from, to) <- node.captureRange do emitInst(Inst.CapHold(from, to))

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

  /** A builder for positive look-around assertion. */
  def buildPositiveLookAround(child: IRNode, body: IRNode => Unit = build): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")
    val scope = assertionThenScope(child)

    // With `(*THEN)` in it, a failing fallback is the barrier, which makes the assertion fail.
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

  /** A builder for a conditional. */
  def buildConditional(condition: IRCondition, yes: IRNode, no: IRNode): Unit =
    val yesLabel = allocateLabel("yes")
    val noLabel = allocateLabel("no")
    val cont = allocateLabel("cont")

    // A look-around condition is atomic, and it keeps the captures in it even if it is negative, unless they are
    // restored as in a negative look-around.
    // Its fallback is the barrier, so `(*COMMIT)`, `(*PRUNE)` and `(*SKIP)` in it make it false (or true for a negative
    // one).
    def buildTest(negative: Boolean, child: IRNode, body: IRNode => Unit): Unit =
      val main = allocateLabel("main")
      val scope = assertionThenScope(child)
      val restores = negative && pattern.captures.restoredByNegativeCondition
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
    * In a look-behind, it ends the look-behind before reaching the position.
    */
  def buildAccept(): Unit =
    for index <- openCaptures do emitInst(Inst.CapEnd(index))
    acceptScope match
      case AcceptScope.Top => emitOk()
      // PCRE and Perl match a look-behind forward.
      case AcceptScope.LookAround =>
        for _ <- 0 until atomicDepth do emitInst(Inst.TxDrop)
        emitTerminator(Inst.Rollback)
      case AcceptScope.Atomic =>
        for _ <- 0 until atomicDepth do emitInst(Inst.TxDrop)
        emitTerminator(Inst.Commit)
    // The rest of the sequence is not reachable.
    enterBlock(allocateLabel("accepted"))

  /** Sets the matching direction for a look-ahead assertion. */
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
      // The register holds the end of the look-behind, so memoization tells the look-behinds at different positions
      // apart.
      emitTerminator(Inst.StepBack(end, starts, start))
      enterBlock(start)
      build(node)
      emitAssert(AssertKind.LookBehindEnd(end))

    (starts, child.data) match
      case (Seq(s), _)                                                     => attempt(child, s)
      case (_, IRNodeData.Disjunction(nodes)) if nodes.size == starts.size =>
        val (alternatives, alternativeStarts) = (nodes.toIndexedSeq, starts.toIndexedSeq)
        buildDisjunction(alternatives, i => attempt(alternatives(i), alternativeStarts(i)))
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException("look-behind starts do not match the alternatives")
      // $COVERAGE-ON$

  /** Sets the matching direction for a look-behind assertion. */
  def wrapLookBehind(run: => Unit): Unit =
    val oldBack = back
    back = true
    run
    back = oldBack

  /** Adds an `assert` instruction. */
  def emitAssert(kind: AssertKind): Unit =
    emitInst(Inst.Assert(kind))

  /** Adds a `read` (or `read_back`) instruction. */
  def emitRead(kind: ReadKind, loc: Option[SourceLocation]): Unit =
    emitInst(if back then Inst.ReadBack(kind, loc) else Inst.Read(kind, loc))
