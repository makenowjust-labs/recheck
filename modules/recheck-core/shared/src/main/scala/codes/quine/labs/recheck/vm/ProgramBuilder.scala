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
import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.recheck.vm.Program.Meta
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRAssertKind
import codes.quine.labs.resyntax.ir.IRBuilder
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
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

private[vm] class ProgramBuilder(
    private val pattern: ParsedPattern,
    private val capturesSize: Int
)(using ctx: Context):
  import ctx._

  /** The unit mode of the pattern. */
  private val mode = pattern.ir.mode

  /** Whether captures in a repetition are reset on each iteration, which only JavaScript does. */
  private val resetsCaptures = pattern.dialect == Dialect.JavaScript

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
      case Inst.Read(_: ReadKind.Ref, _) => hasRef = true
      case _                             => // Nothing to do

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
    val meta = Meta(mode, hasRef, capturesSize, counters.size, canaries.size, predecessors)

    Program(blocks, meta)

  /** Builds a program from the RegExp pattern. */
  def build(): Program =
    val main = allocateLabel("main")
    enterBlock(main)

    // Adds `.*` part if the pattern does not start with `^`.
    if pattern.needsSigmaStarAtBegin then
      val loop = allocateLabel("loop")
      val cont = allocateLabel("cont")

      emitTerminator(Inst.Try(cont, loop))

      enterBlock(loop)
      emitInst(Inst.Read(ReadKind.Any, None))
      emitTerminator(Inst.Jmp(main))

      enterBlock(cont)

    emitInst(Inst.CapBegin(0))
    build(pattern.node)
    emitInst(Inst.CapEnd(0))
    emitTerminator(Inst.Ok)

    result()

  /** Builds a program from a node. */
  def build(node: IRNode): Unit = interrupt:
    node.data match
      case IRNodeData.Disjunction(children)    => buildDisjunction(children)
      case IRNodeData.Sequence(children)       => buildSequence(children)
      case IRNodeData.Empty                    => ()
      case IRNodeData.Capture(index, _, child) => buildCapture(index, child)
      case IRNodeData.Repeat(child, q)         => buildRepeat(q, child)
      case IRNodeData.Atomic(child)            => buildAtomic(child)
      case IRNodeData.Assert(kind)             => emitAssert(assertKind(kind))
      case IRNodeData.LookAhead(false, child)  => wrapLookAhead(buildPositiveLookAround(child))
      case IRNodeData.LookAhead(true, child)   => wrapLookAhead(buildNegativeLookAround(child))
      case IRNodeData.LookBehind(false, child) => wrapLookBehind(buildPositiveLookAround(child))
      case IRNodeData.LookBehind(true, child)  => wrapLookBehind(buildNegativeLookAround(child))
      case IRNodeData.Chars(set)               => emitRead(readKind(set), Location.from(node.loc))
      case IRNodeData.Strings(set)             =>
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
    case IRAssertKind.LineBegin(t, after)   => AssertKind.LineBegin(t, after)
    case IRAssertKind.LineEnd(t)            => AssertKind.LineEnd(t)
    case IRAssertKind.TextBegin             => AssertKind.InputBegin
    case IRAssertKind.TextEnd               => AssertKind.InputEnd
    case IRAssertKind.ChompTextEnd(t)       => AssertKind.ChompInputEnd(t)

  /** A builder for a disjunction node. */
  def buildDisjunction(children: Seq[IRNode]): Unit =
    buildAlternatives(children.map(n => () => build(n)))

  /** A builder for alternatives tried in the order. */
  def buildAlternatives(alternatives: Seq[() => Unit]): Unit =
    val labels = alternatives.init.map(_ => (allocateLabel("left"), allocateLabel("right")))
    val cont = allocateLabel("cont")

    for (run, (l, r)) <- alternatives.init.zip(labels) do
      emitTerminator(Inst.Try(l, r))

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
    build(children)
    emitInst(if back then Inst.CapBegin(index) else Inst.CapEnd(index))

  /** A builder for `x*` node. */
  def buildStar(isLazy: Boolean, child: IRNode): Unit =
    val begin = allocateEntrance("begin")
    val loop = allocateLabel("loop")
    val cont = allocateLabel("cont")

    enterLoopBlock(isLazy, loop, cont)
    wrapCanary(child)(wrapReset(child)(build(child)))
    emitTerminator(Inst.Jmp(begin))

    enterBlock(cont)

  /** A builder for `x+` node. */
  def buildPlus(isLazy: Boolean, child: IRNode): Unit =
    build(child)
    buildStar(isLazy, child)

  /** A builder for `x?` node. */
  def buildQuestion(isLazy: Boolean, child: IRNode): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(if isLazy then Inst.Try(cont, main) else Inst.Try(main, cont))

    enterBlock(main)
    build(child)
    emitTerminator(Inst.Jmp(cont))

    enterBlock(cont)

  /** A builder for `x{n,m}` node. */
  def buildRepeat(quantifier: IRQuantifier, child: IRNode): Unit =
    def isLazy(strategy: BacktrackStrategy): Boolean = strategy == BacktrackStrategy.Lazy
    quantifier match
      case IRQuantifier.Exact(0)               => // nothing to do
      case IRQuantifier.Exact(1)               => build(child)
      case IRQuantifier.Exact(n)               => buildRepeatN(n, child)
      case IRQuantifier.Unbounded(0, strategy) => buildStar(isLazy(strategy), child)
      case IRQuantifier.Unbounded(1, strategy) => buildPlus(isLazy(strategy), child)
      case IRQuantifier.Unbounded(n, strategy) =>
        buildRepeatN(n, child)
        buildStar(isLazy(strategy), child)
      case IRQuantifier.Bounded(0, 1, strategy) => buildQuestion(isLazy(strategy), child)
      case IRQuantifier.Bounded(0, n, strategy) => buildRepeatAtMostN(isLazy(strategy), n, child)
      case IRQuantifier.Bounded(1, 2, strategy) =>
        build(child)
        buildQuestion(isLazy(strategy), child)
      case IRQuantifier.Bounded(1, m, strategy) =>
        build(child)
        buildRepeatAtMostN(isLazy(strategy), m - 1, child)
      case IRQuantifier.Bounded(n, m, strategy) if m - n == 1 =>
        buildRepeatN(n, child)
        buildQuestion(isLazy(strategy), child)
      case IRQuantifier.Bounded(n, m, strategy) =>
        buildRepeatN(n, child)
        buildRepeatAtMostN(isLazy(strategy), m - n, child)

  /** A builder for `x{n}` node. */
  def buildRepeatN(n: Int, child: IRNode): Unit =
    val loop = allocateEntrance("loop")
    val cont = allocateLabel("cont")

    val reg = allocateCounter()

    // Note that `wrapCanary` is not needed.
    wrapReset(child)(build(child))
    closeLoopBlock(reg, n, loop, cont)

  /** A builder for `x{0,n}` node. */
  def buildRepeatAtMostN(isLazy: Boolean, n: Int, child: IRNode): Unit =
    val begin = allocateEntrance("begin")
    val loop = allocateLabel("loop")
    val cont = allocateLabel("cont")

    val reg = allocateCounter()

    enterLoopBlock(isLazy, loop, cont)
    wrapCanary(child)(wrapReset(child)(build(child)))
    closeLoopBlock(reg, n, begin, cont)

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

  /** A builder for an atomic group, which commits the first match of the child. */
  def buildAtomic(child: IRNode): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(Inst.Tx(main, Some(cont), None))

    enterBlock(main)
    build(child)
    emitTerminator(Inst.Commit)

    enterBlock(cont)

  /** A builder for positive look-around assertion. */
  def buildPositiveLookAround(child: IRNode): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(Inst.Tx(main, Some(cont), None))

    enterBlock(main)
    build(child)
    emitTerminator(Inst.Rollback)

    enterBlock(cont)

  /** A builder for negative look-around assertion. */
  def buildNegativeLookAround(child: IRNode): Unit =
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(Inst.Tx(main, None, Some(cont)))

    enterBlock(main)
    build(child)
    emitTerminator(Inst.Rollback)

    enterBlock(cont)

  /** Sets a matching direction fot look-ahead assertion. */
  def wrapLookAhead(run: => Unit): Unit =
    val oldBack = back
    back = false
    run
    back = oldBack

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
