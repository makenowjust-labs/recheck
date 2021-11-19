package codes.quine.labo.recheck.vm

import scala.collection.mutable
import scala.util.Try

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.Pattern._
import codes.quine.labo.recheck.regexp.PatternExtensions._
import codes.quine.labo.recheck.unicode.IChar
import codes.quine.labo.recheck.unicode.UChar
import codes.quine.labo.recheck.vm.Inst.AssertKind
import codes.quine.labo.recheck.vm.Inst.ReadKind
import codes.quine.labo.recheck.vm.Program.Meta

/** ProgramBuilder is a builder of a program from a RegExp pattern. */
object ProgramBuilder {

  /** Builds a corresponding program from the RegExp pattern. */
  def build(pattern: Pattern)(implicit ctx: Context): Try[Program] =
    for {
      _ <- Try(())
      capturesSize = pattern.capturesSize
      builder = new ProgramBuilder(pattern, capturesSize)
      program <- Try(builder.build())
    } yield program
}

private[vm] class ProgramBuilder(
    private[this] val pattern: Pattern,
    private[this] val capturesSize: Int
)(implicit ctx: Context) {
  import ctx._
  import pattern._
  import flagSet._

  /** A buffer of created counter registers. */
  private[this] val counters: mutable.Buffer[CounterReg] = mutable.Buffer.empty

  /** A pool of free counter registers. */
  private[this] val countersPool: mutable.Stack[CounterReg] = mutable.Stack.empty

  /** A buffer of created canary registers. */
  private[this] val canaries: mutable.Buffer[CanaryReg] = mutable.Buffer.empty

  /** A pool of free canary registers. */
  private[this] val canariesPool: mutable.Stack[CanaryReg] = mutable.Stack.empty

  /** A buffer collecting predecessors of a block. */
  private[this] val predecessorsBuffer: mutable.Buffer[mutable.Set[Label]] = mutable.Buffer.empty

  /** A next instruction ID. */
  private[this] var nextInstID: Int = 0

  /** A buffer of processed labels. */
  private[this] val labelsBuffer: mutable.Buffer[Label] = mutable.Buffer.empty

  /** Current processing block's label. It may be `null` when a block is not entered. */
  private[this] var currentLabel: Label = _

  /** A buffer of current processing block's instructions. */
  private[this] val instsBuffer: mutable.Buffer[Inst.NonTerminator] = mutable.Buffer.empty

  /** A program has a back reference information. */
  private[this] var hasRef: Boolean = false

  /** Whether a matching direction is backward or not. */
  private[this] var back: Boolean = false

  /** Allocates a new counter register, or reuses a free register. */
  def allocateCounter(): CounterReg =
    if (countersPool.nonEmpty) countersPool.pop()
    else {
      val index = counters.size
      val reg = CounterReg(index)
      counters.append(reg)
      reg
    }

  /** Marks the counter register is free. */
  def freeCounter(reg: CounterReg): Unit = {
    countersPool.push(reg)
  }

  /** Allocates a new canary register, or reuses a free register. */
  def allocateCanary(): CanaryReg =
    if (canariesPool.nonEmpty) canariesPool.pop()
    else {
      val index = canaries.size
      val reg = CanaryReg(index)
      canaries.append(reg)
      reg
    }

  /** Marks the canary register is free. */
  def freeCanary(reg: CanaryReg): Unit = {
    canariesPool.push(reg)
  }

  /** Allocates a new label with an unique ID. */
  def allocateLabel(name: String): Label = {
    val index = predecessorsBuffer.size
    predecessorsBuffer.append(mutable.Set.empty)
    Label(name, index)
  }

  /** Starts a block of the label. */
  def enterBlock(label: Label): Unit = {
    // $COVERAGE-OFF$
    if ((currentLabel ne null) || instsBuffer.nonEmpty) throw new IllegalStateException("unterminated block is found")
    if (label.block ne null) throw new IllegalStateException("a block cannot be re-entered")
    // $COVERAGE-ON$

    currentLabel = label
  }

  /** Adds an instruction to a block. */
  def emitInst(inst: Inst.NonTerminator): Unit = {
    // $COVERAGE-OFF$
    if (currentLabel eq null) throw new IllegalStateException("block is not entered")
    // $COVERAGE-ON$

    // Updates a meta information.
    inst match {
      case Inst.Read(ReadKind.Ref(_), _) => hasRef = true
      case _                             => // Nothing to do
    }

    instsBuffer.append(inst)

    // Assigns an unique ID to `inst`.
    val id = nextInstID
    nextInstID += 1
    inst.id = id
  }

  /** Adds a terminator instruction to a block, and finishes the block. */
  def emitTerminator(inst: Inst.Terminator): Unit = {
    // $COVERAGE-OFF$
    if (currentLabel eq null) throw new IllegalStateException("block is not entered")
    // $COVERAGE-ON$

    // Updates a predecessor information.
    for (successor <- inst.successors) predecessorsBuffer(successor.index).add(currentLabel)

    val block = Block(instsBuffer.toSeq, inst)
    currentLabel.block = block
    labelsBuffer.append(currentLabel)

    currentLabel = null
    instsBuffer.clear()

    // Assigns an unique ID to `inst`.
    val id = nextInstID
    nextInstID += 1
    inst.id = id
  }

  /** Allocates a new label, or reuses the current label if it is empty.
    *
    * It is an utility for an entrance label of a loop, with reducing a label allocation.
    */
  def allocateEntrance(name: String): Label =
    if (instsBuffer.isEmpty) currentLabel
    else {
      val label = allocateLabel(name)
      emitTerminator(Inst.Jmp(label))
      enterBlock(label)
      label
    }

  /** Returns a built program. */
  def result(): Program = {
    val blocks = labelsBuffer.iterator.map(label => (label, label.block)).toVector
    val predecessors = predecessorsBuffer.iterator.map(_.toSet).toVector
    val meta = Meta(ignoreCase, unicode, hasRef, capturesSize, counters.size, canaries.size, predecessors)
    Program(blocks, meta)
  }

  /** Builds a program from the RegExp pattern. */
  def build(): Program = {
    val main = allocateLabel("main")
    enterBlock(main)

    // Adds `.*` part if the pattern does not start with `^`.
    if (!pattern.hasLineBeginAtBegin && !pattern.flagSet.sticky) {
      val loop = allocateLabel("loop")
      val cont = allocateLabel("cont")

      emitTerminator(Inst.Try(cont, loop))

      enterBlock(loop)
      emitInst(Inst.Read(ReadKind.Any, None))
      emitTerminator(Inst.Jmp(main))

      enterBlock(cont)
    }

    emitInst(Inst.CapBegin(0))
    build(pattern.node)
    emitInst(Inst.CapEnd(0))
    emitTerminator(Inst.Ok)

    result()
  }

  /** Builds a program from a node. */
  def build(node: Node): Unit = interrupt(node match {
    case Disjunction(children)              => buildDisjunction(children)
    case Sequence(children)                 => buildSequence(children)
    case Capture(index, child)              => buildCapture(index, child)
    case NamedCapture(index, _, child)      => buildCapture(index, child)
    case Group(child)                       => build(child)
    case Star(nonGreedy, child)             => buildStar(nonGreedy, child)
    case Plus(nonGreedy, child)             => buildPlus(nonGreedy, child)
    case Question(nonGreedy, child)         => buildQuestion(nonGreedy, child)
    case Repeat(nonGreedy, min, max, child) => buildRepeat(nonGreedy, min, max, child)
    case WordBoundary(invert)     => emitAssert(if (invert) AssertKind.WordBoundaryNot else AssertKind.WordBoundary)
    case LineBegin()              => emitAssert(if (multiline) AssertKind.LineBegin else AssertKind.InputBegin)
    case LineEnd()                => emitAssert(if (multiline) AssertKind.LineEnd else AssertKind.InputEnd)
    case LookAhead(false, child)  => wrapLookAhead(buildPositiveLookAround(child))
    case LookAhead(true, child)   => wrapLookAhead(buildNegativeLookAround(child))
    case LookBehind(false, child) => wrapLookBehind(buildPositiveLookAround(child))
    case LookBehind(true, child)  => wrapLookBehind(buildNegativeLookAround(child))
    case Character(c0) =>
      val c = if (ignoreCase) UChar.canonicalize(c0, unicode) else c0
      emitRead(ReadKind.Char(c), node.loc)
    case node @ CharacterClass(invert, _) =>
      val ch0 = node.toIChar(unicode).get
      val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
      emitRead(if (invert) ReadKind.ClassNot(ch) else ReadKind.Class(ch), node.loc)
    case node: AtomNode =>
      val ch0 = node.toIChar(unicode).get
      val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
      emitRead(ReadKind.Class(ch), node.loc)
    case Dot() =>
      emitRead(if (dotAll) ReadKind.Any else ReadKind.Dot, node.loc)
    case BackReference(index)         => emitRead(ReadKind.Ref(index), node.loc)
    case NamedBackReference(index, _) => emitRead(ReadKind.Ref(index), node.loc)
  })

  /** A builder for a disjunction node. */
  def buildDisjunction(children: Seq[Node]): Unit = {
    val labels = children.init.map(_ => (allocateLabel("left"), allocateLabel("right")))
    val cont = allocateLabel("cont")

    for ((n, (l, r)) <- children.init.zip(labels)) {
      emitTerminator(Inst.Try(l, r))

      enterBlock(l)
      build(n)
      emitTerminator(Inst.Jmp(cont))

      enterBlock(r)
    }

    build(children.last)
    emitTerminator(Inst.Jmp(cont))

    enterBlock(cont)
  }

  /** A builder for a sequence of nodes. */
  def buildSequence(children: Seq[Node]): Unit = {
    for (n <- if (back) children.reverse else children) build(n)
  }

  /** A builder for a capture node. */
  def buildCapture(index: Int, children: Node): Unit = {
    emitInst(if (back) Inst.CapEnd(index) else Inst.CapBegin(index))
    build(children)
    emitInst(if (back) Inst.CapBegin(index) else Inst.CapEnd(index))
  }

  /** A builder for `x*` node. */
  def buildStar(nonGreedy: Boolean, child: Node): Unit = {
    val isEmpty = child.isEmpty
    val captureRange = child.captureRange

    val begin = allocateEntrance("begin")
    val loop = allocateLabel("loop")
    val cont = allocateLabel("cont")

    emitTerminator(if (nonGreedy) Inst.Try(cont, loop) else Inst.Try(loop, cont))

    enterBlock(loop)
    wrapCanary(isEmpty) {
      for ((min, max) <- captureRange.range) emitInst(Inst.CapReset(min, max))
      build(child)
    }
    emitTerminator(Inst.Jmp(begin))

    enterBlock(cont)
  }

  /** A builder for `x+` node. */
  def buildPlus(nonGreedy: Boolean, child: Node): Unit = {
    build(child)
    buildStar(nonGreedy, child)
  }

  /** A builder for `x?` node. */
  def buildQuestion(nonGreedy: Boolean, child: Node): Unit = {
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(if (nonGreedy) Inst.Try(cont, main) else Inst.Try(main, cont))

    enterBlock(main)
    build(child)
    emitTerminator(Inst.Jmp(cont))

    enterBlock(cont)
  }

  /** A builder for `x{n,m}` node. */
  def buildRepeat(nonGreedy: Boolean, min: Int, max: Option[Option[Int]], child: Node): Unit =
    (min, max) match {
      case (n, Some(Some(m))) if n > m => throw new InvalidRegExpException("out of order repetition quantifier")
      case (0, None)                   => // nothing to do
      case (1, None)                   => build(child)
      case (n, None)                   => buildRepeatN(n, child)
      case (0, Some(None))             => buildStar(nonGreedy, child)
      case (1, Some(None))             => buildPlus(nonGreedy, child)
      case (n, Some(None)) =>
        buildRepeatN(n, child)
        buildStar(nonGreedy, child)
      case (0, Some(Some(0))) => // nothing to do
      case (0, Some(Some(1))) => buildQuestion(nonGreedy, child)
      case (0, Some(Some(n))) => buildRepeatAtMostN(nonGreedy, n, child)
      case (1, Some(Some(1))) => build(child)
      case (1, Some(Some(2))) =>
        build(child)
        buildQuestion(nonGreedy, child)
      case (1, Some(Some(m))) =>
        build(child)
        buildRepeatAtMostN(nonGreedy, m - 1, child)
      case (n, Some(Some(m))) if n == m => buildRepeatN(n, child)
      case (n, Some(Some(m))) if m - n == 1 =>
        buildRepeatN(n, child)
        buildQuestion(nonGreedy, child)
      case (n, Some(Some(m))) =>
        buildRepeatN(n, child)
        buildRepeatAtMostN(nonGreedy, m - n, child)
    }

  /** A builder for `x{n}` node. */
  def buildRepeatN(n: Int, child: Node): Unit = {
    val captureRange = child.captureRange

    val loop = allocateEntrance("loop")
    val cont = allocateLabel("cont")

    val reg = allocateCounter()

    // Note that `wrapCanary` is not needed.
    for ((min, max) <- captureRange.range) emitInst(Inst.CapReset(min, max))
    build(child)
    emitInst(Inst.Inc(reg))
    emitTerminator(Inst.Cmp(reg, n, loop, cont))

    enterBlock(cont)
    emitInst(Inst.Reset(reg))

    freeCounter(reg)
  }

  /** A builder for `x{0,n}` node. */
  def buildRepeatAtMostN(nonGreedy: Boolean, n: Int, child: Node): Unit = {
    val isEmpty = child.isEmpty
    val captureRange = child.captureRange

    val begin = allocateEntrance("begin")
    val loop = allocateLabel("loop")
    val cont = allocateLabel("cont")

    val reg = allocateCounter()

    emitTerminator(if (nonGreedy) Inst.Try(cont, loop) else Inst.Try(loop, cont))

    enterBlock(loop)
    wrapCanary(isEmpty) {
      for ((min, max) <- captureRange.range) emitInst(Inst.CapReset(min, max))
      build(child)
    }
    emitInst(Inst.Inc(reg))
    emitTerminator(Inst.Cmp(reg, n, begin, cont))

    enterBlock(cont)
    emitInst(Inst.Reset(reg))

    freeCounter(reg)
  }

  /** Inserts a canary around a loop body if needed. */
  def wrapCanary(isEmpty: Boolean)(run: => Unit): Unit = {
    if (isEmpty) {
      val reg = allocateCanary()
      emitInst(Inst.SetCanary(reg))
      run
      emitInst(Inst.CheckCanary(reg))
      freeCanary(reg)
    } else run
  }

  /** A builder for positive look-around assertion. */
  def buildPositiveLookAround(child: Node): Unit = {
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(Inst.Tx(main, Some(cont), None))

    enterBlock(main)
    build(child)
    emitTerminator(Inst.Rollback)

    enterBlock(cont)
  }

  /** A builder for negative look-around assertion. */
  def buildNegativeLookAround(child: Node): Unit = {
    val main = allocateLabel("main")
    val cont = allocateLabel("cont")

    emitTerminator(Inst.Tx(main, None, Some(cont)))

    enterBlock(main)
    build(child)
    emitTerminator(Inst.Rollback)

    enterBlock(cont)
  }

  /** Sets a matching direction fot look-ahead assertion. */
  def wrapLookAhead(run: => Unit): Unit = {
    val oldBack = back
    back = false
    run
    back = oldBack
  }

  /** Sets a matching direction fot look-behind assertion. */
  def wrapLookBehind(run: => Unit): Unit = {
    val oldBack = back
    back = true
    run
    back = oldBack
  }

  /** Adds an `assert` instruction. */
  def emitAssert(kind: AssertKind): Unit = {
    emitInst(Inst.Assert(kind))
  }

  /** Adds a `read` (or `read_back`) instruction. */
  def emitRead(kind: ReadKind, loc: Option[(Int, Int)]): Unit = {
    emitInst(if (back) Inst.ReadBack(kind, loc) else Inst.Read(kind, loc))
  }
}
