package codes.quine.labs.recheck
package automaton

import scala.collection.mutable
import scala.util.Try

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRAssertKind
import codes.quine.labs.resyntax.ir.IRBacktrackControl
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.IRLineTerminators
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRNotEmpty
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.IRQuantifier
import codes.quine.labs.resyntax.ir.alphabet.CharKind
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** RegExp IR to ε-NFA Compiler. */
object EpsNFABuilder:

  /** Compiles RegExp IR into ε-NFA. */
  def build(pattern: IRPattern)(using ctx: Context): Try[EpsNFA[Int]] =
    ctx.interrupt:
      for
        _ <- Try(())
        alphabet = PatternAlphabet.of(pattern)
        builder = new EpsNFABuilder(pattern, alphabet)
        epsNFA <- Try(builder.build())
      yield
        ctx.log:
          s"""|automaton: EpsNFA construction
              |     state size: ${epsNFA.stateSet.size}
              |  alphabet size: ${epsNFA.alphabet.pairs.size}""".stripMargin
        epsNFA

private class EpsNFABuilder(
    private val pattern: IRPattern,
    private val alphabet: PatternAlphabet
)(using ctx: Context):

  /** A cached result of `IRPattern#needsInputTerminatorDistinction`. */
  private val needsInputTerminatorDistinction = pattern.needsInputTerminatorDistinction

  /** A next state counter. */
  private var counterQ = 0

  private val refineCache: mutable.Map[CharSet, Set[(CharSet, CharKind)]] = mutable.Map.empty

  /** Returns a next state number. */
  def nextQ(): Int =
    val q = counterQ
    counterQ += 1
    q

  /** Returns a state set from the current state counter. */
  def stateSet(): Set[Int] = (0 until counterQ).toSet

  /** A next loop counter. */
  private var counterLoop = 0

  /** Returns a next loop number. */
  def nextLoop(): Int =
    val loop = counterLoop
    counterLoop += 1
    loop

  /** A transition function. */
  private val tau: mutable.Map[Int, Transition[Int]] = mutable.Map.empty

  /** The states of `\K`, each of which has an ε-transition. */
  private val resetMatchStarts: mutable.Set[Int] = mutable.Set.empty

  /** Emits a transition. */
  def emit(t: (Int, Transition[Int])): Unit =
    tau.addOne(t)

  /** Builds EpsNFA for the pattern. */
  def build(): EpsNFA[Int] =
    val start = counterQ
    val (i0, a00) = buildNode(pattern.node)
    val (first, later, a0) = rejectEmpty(start, i0, a00)

    val init = if pattern.needsSigmaStarAtBegin then
      val loop = nextLoop()
      val i1 = nextQ()
      val i2 = nextQ()
      val i3 = nextQ()
      val i4 = nextQ()
      emit(i1 -> Eps(Vector(i4, i2)))
      emit(i2 -> LoopEnter(loop, i3))
      emit(i4 -> LoopExit(loop, first))
      if later == first then emit(i3 -> Consume(alphabet.any, i1))
      else
        // The search after the start enters the other copy of the pattern.
        val j1 = nextQ()
        val j4 = nextQ()
        emit(i3 -> Consume(alphabet.any, j1))
        emit(j1 -> Eps(Vector(j4, i2)))
        emit(j4 -> LoopExit(loop, later))
      i1
    else first

    val accept = if pattern.needsSigmaStarAtEnd then
      val loop = nextLoop()
      val a1 = nextQ()
      val a2 = nextQ()
      val a3 = nextQ()
      val a4 = nextQ()
      emit(a0 -> Eps(Vector(a3, a1)))
      emit(a1 -> LoopEnter(loop, a2))
      emit(a2 -> Consume(alphabet.any, a0))
      emit(a3 -> LoopExit(loop, a4))
      a4
    else a0

    EpsNFA(alphabet, stateSet(), init, accept, tau.toMap)

  /** Makes the pattern's states between `start` and the current counter reject an empty match, as PCRE's `(*NOTEMPTY)`
    * and `(*NOTEMPTY_ATSTART)` do, and returns the initial states of the search at the start and after it, and the
    * final state.
    *
    * The states are doubled into a copy before consuming a character and one after it (a product with a 2-state DFA),
    * and only the latter accepts. It keeps the order of the transitions, so the engine backtracks from an empty match
    * as the ε-NFA does from a dead state. `\K` returns to the first copy for `(*NOTEMPTY)`, which sees the reported
    * match. For `(*NOTEMPTY_ATSTART)`, the search after the start begins in the second copy.
    */
  private def rejectEmpty(start: Int, i0: Int, a0: Int): (Int, Int, Int) =
    pattern.notEmpty match
      case IRNotEmpty.Never => (i0, i0, a0)
      case notEmpty         =>
        val end = counterQ
        val shift = end - start
        counterQ += shift
        def moved(q: Int): Int = q + shift

        def retarget(t: Transition[Int], f: Int => Int): Transition[Int] = t match
          case Eps(qs)              => Eps(qs.map(f))
          case Assert(kind, q)      => Assert(kind, f(q))
          case Consume(set, q, loc) => Consume(set, f(q), loc)
          case LoopEnter(loop, q)   => LoopEnter(loop, f(q))
          case LoopExit(loop, q)    => LoopExit(loop, f(q))

        for q <- start until end; t <- tau.get(q) do
          val reset = notEmpty == IRNotEmpty.Anywhere && resetMatchStarts.contains(q)
          emit(moved(q) -> (if reset then t else retarget(t, moved)))
          t match
            case c: Consume[Int] => emit(q -> retarget(c, moved))
            case _               => ()
        emit(a0 -> Eps(Vector.empty))
        (i0, if notEmpty == IRNotEmpty.AtStart then moved(i0) else i0, moved(a0))

  private def refine(set: CharSet): Set[(CharSet, CharKind)] =
    refineCache.getOrElseUpdate(set, alphabet.refine(set))

  /** Builds a transition consuming one of the pairs. */
  private def consume(chs: Set[(CharSet, CharKind)], loc: Option[SourceLocation]): (Int, Int) =
    val i = nextQ()
    val a = nextQ()
    emit(i -> Consume(chs, a, loc))
    (i, a)

  /** Builds an ε-NFA for a choice of the alternatives in the order. */
  private def buildChoice(alternatives: Seq[() => (Int, Int)]): (Int, Int) =
    val ss = alternatives.map(_())
    val i = nextQ()
    emit(i -> Eps(ss.map(_._1)))
    val a = nextQ()
    for (_, a0) <- ss do emit(a0 -> Eps(Vector(a)))
    (i, a)

  private def buildSequence(builders: Seq[() => (Int, Int)]): (Int, Int) =
    val ss = builders.map(_())
    ss
      .reduceLeftOption[(Int, Int)]:
        case ((i1, a1), (i2, a2)) =>
          emit(a1 -> Eps(Vector(i2)))
          (i1, a2)
      .getOrElse(epsilon())

  /** Builds EpsNFA transitions for the node. The result pair is, the first is initial state, and the second is final
    * state of the node.
    */
  def buildNode(node: IRNode): (Int, Int) = ctx.interrupt:
    node.data match
      case IRNodeData.Disjunction(ns)  => buildChoice(ns.map(n => () => buildNode(n)))
      case IRNodeData.Sequence(ns)     => buildSequence(ns.map(n => () => buildNode(n)))
      case IRNodeData.Empty            => epsilon()
      case IRNodeData.Capture(_, _, n) => buildNode(n)
      case IRNodeData.Repeat(n, q)     => buildRepeat(n, q)
      case IRNodeData.Assert(kind)     => buildAssert(node, kind)
      case IRNodeData.Chars(set)       => consume(refine(set), node.loc.toOption)
      case IRNodeData.Strings(set)     =>
        val loc = node.loc.toOption
        // The strings are tried from the longest, and then the characters.
        val strings = set.strings.toSeq
          .sortBy(-_.length)
          .map: s =>
            () => buildSequence(s.iterator.toSeq.map(c => () => consume(refine(CharSet.single(c)), loc)))
        val chars = Option.when(set.chars.nonEmpty)(() => consume(refine(set.chars), loc))
        buildChoice(strings ++ chars)
      case IRNodeData.ResetMatchStart =>
        val (i, a) = (nextQ(), nextQ())
        emit(i -> Eps(Vector(a)))
        resetMatchStarts.add(i)
        (i, a)
      // A mark has no effect on matching, and `(*SKIP:NAME)` is unsupported.
      case IRNodeData.BacktrackControl(_: IRBacktrackControl.Mark) => epsilon()
      case IRNodeData.BacktrackControl(_)  => throw new UnsupportedException("backtracking control verb")
      case IRNodeData.Conditional(_, _, _) => throw new UnsupportedException("conditional")
      case IRNodeData.LookAhead(_, _) | IRNodeData.NonAtomicLookAhead(_) =>
        throw new UnsupportedException("look-ahead assertion")
      case IRNodeData.LookBehind(_, _, _) | IRNodeData.NonAtomicLookBehind(_, _) =>
        throw new UnsupportedException("look-behind assertion")
      case IRNodeData.Atomic(_)           => throw new UnsupportedException("atomic group")
      case IRNodeData.BackReference(_, _) => throw new UnsupportedException("back-reference")
      case IRNodeData.Unsupported(_)      => throw new UnsupportedException(node.unsupportedMessage)

  /** Builds EpsNFA transitions for a repetition.
    *
    * `x{n,}` is built as `n - 1` copies of `x` and `x+`, and `x{n,m}` as `n` copies of `x` and nested `(x(x)?)?`.
    */
  private def buildRepeat(n: IRNode, q: IRQuantifier): (Int, Int) =
    q match
      case IRQuantifier.Unbounded(0, strategy)   => buildStar(n, strategy)
      case IRQuantifier.Unbounded(min, strategy) =>
        buildSequence(Vector.fill(min - 1)(() => buildNode(n)) :+ (() => buildPlus(n, strategy)))
      case IRQuantifier.Exact(m) =>
        buildSequence(Vector.fill(m)(() => buildNode(n)))
      case IRQuantifier.Bounded(min, max, strategy) =>
        def question(n: IRNode): IRNode = IRNode(IRNodeData.Repeat(n, IRQuantifier.Bounded(0, 1, strategy)))

        val maxN = Option.when(max > min):
          Vector
            .fill(max - min)(n)
            .reduceRight((l, r) => IRNode(IRNodeData.Sequence(Vector(l, question(r)))))
        buildSequence(Vector.fill(min)(() => buildNode(n)) ++ maxN.map(m => () => buildQuestion(m, strategy)))

  /** Builds EpsNFA transitions for `x*`. */
  private def buildStar(n: IRNode, strategy: BacktrackStrategy): (Int, Int) =
    //      +---------------------+
    //      |                     |
    //  (i)-+->(i2)->[i0-->a0]-+  +->(a1)->(a)
    //   ^                     |
    //   +---------------------+
    val (i0, a0) = buildNode(n)
    val loop = nextLoop()
    val i = nextQ()
    val i2 = nextQ()
    val a1 = nextQ()
    val a = nextQ()
    val t = if strategy == BacktrackStrategy.Lazy then Vector(a1, i2) else Vector(i2, a1)
    emit(i -> Eps(t))
    emit(i2 -> LoopEnter(loop, i0))
    emit(a0 -> Eps(Vector(i)))
    emit(a1 -> LoopExit(loop, a))
    (i, a)

  /** Builds EpsNFA transitions for `x+`. */
  private def buildPlus(n: IRNode, strategy: BacktrackStrategy): (Int, Int) =
    // +----(i1)---+
    // |           |
    // +->[i-->a0]-+->(a1)->(a)
    val (i, a0) = buildNode(n)
    val loop = nextLoop()
    val i1 = nextQ()
    val a1 = nextQ()
    val a = nextQ()
    val t = if strategy == BacktrackStrategy.Lazy then Vector(a1, i1) else Vector(i1, a1)
    emit(a0 -> Eps(t))
    emit(i1 -> LoopEnter(loop, i))
    emit(a1 -> LoopExit(loop, a))
    (i, a)

  /** Builds EpsNFA transitions for `x?`. */
  private def buildQuestion(n: IRNode, strategy: BacktrackStrategy): (Int, Int) =
    //     +--------+
    //     |        v
    // (i)-+->[i0-->a]
    val (i0, a) = buildNode(n)
    val i = nextQ()
    val t = if strategy == BacktrackStrategy.Lazy then Vector(a, i0) else Vector(i0, a)
    emit(i -> Eps(t))
    (i, a)

  /** The line terminator sets that the character kinds tell, in the order of the bits. */
  private val kindLineTerminators = PatternAlphabet.kindLineTerminators(pattern)

  /** The word sets that the character kinds tell, in the order of the bits. */
  private val wordSets = PatternAlphabet.wordSets(pattern)

  /** The word marks that the character kinds tell, in the order of the bits. */
  private val wordMarks = PatternAlphabet.wordMarks(pattern)

  /** The text end assertions before a final line terminator that are not at an end point. */
  private val chompTextEndsInMiddle = pattern.chompTextEndsInMiddle.toSet

  private def buildAssert(node: IRNode, kind: IRAssertKind): (Int, Int) =
    def atomic(t: IRLineTerminators): Boolean = t.crlf == IRLineTerminators.CRLF.Atomic

    def plain(t: IRLineTerminators): Boolean = t.crlf == IRLineTerminators.CRLF.Ignored

    def boundary(kind: AssertKind): (Int, Int) = if needsInputTerminatorDistinction then assert(kind) else epsilon()

    kind match
      case IRAssertKind.WordBoundary(w) =>
        val i = wordSets.indexOf(w)
        assert(if i == 0 then AssertKind.WordBoundary else AssertKind.WordBoundaryOf(i))
      case IRAssertKind.NotWordBoundary(w) =>
        val i = wordSets.indexOf(w)
        assert(if i == 0 then AssertKind.WordBoundaryNot else AssertKind.WordBoundaryNotOf(i))
      case IRAssertKind.MarkWordBoundary(w, m) =>
        assert(AssertKind.MarkWordBoundaryOf(wordSets.indexOf(w), wordMarks.indexOf(m)))
      case IRAssertKind.NotMarkWordBoundary(w, m) =>
        assert(AssertKind.MarkWordBoundaryNotOf(wordSets.indexOf(w), wordMarks.indexOf(m)))
      case IRAssertKind.LineBegin(t, atEnd) =>
        val i = kindLineTerminators.indexOf(t)
        if i == 0 && plain(t) && atEnd == IRLineBeginAtEnd.AfterTerminator then assert(AssertKind.LineBegin)
        else assert(AssertKind.LineBeginOf(i, atEnd, !plain(t) && !t.chars(0x0a), atomic(t)))
      case IRAssertKind.LineEnd(t) =>
        val i = kindLineTerminators.indexOf(t)
        if i == 0 && plain(t) then assert(AssertKind.LineEnd)
        else assert(AssertKind.LineEndOf(i, !plain(t) && !t.chars(0x0d), atomic(t)))
      case IRAssertKind.NotBetweenCRLF                                          => assert(AssertKind.NotBetweenCRLF)
      case IRAssertKind.TextBegin                                               => boundary(AssertKind.InputBegin)
      case IRAssertKind.TextEnd                                                 => boundary(AssertKind.InputEnd)
      case IRAssertKind.ChompTextEnd(t) if chompTextEndsInMiddle.contains(node) =>
        assert(AssertKind.ChompEnd(kindLineTerminators.indexOf(t), !plain(t), atomic(t)))
      case IRAssertKind.ChompTextEnd(t) =>
        // At an end point, it is the end after an optional line terminator.
        val chars = CharSet.of(t.chars.toSeq*)
        val single = Option.when(chars.nonEmpty)(() => consume(refine(chars), None))
        val crlf = Option.when(!plain(t)): () =>
          buildSequence(
            Seq(() => consume(refine(CharSet.single(0x0d)), None), () => consume(refine(CharSet.single(0x0a)), None))
          )
        val terminator = buildChoice(Seq(() => epsilon()) ++ single ++ crlf)
        val end = boundary(AssertKind.InputEnd)
        emit(terminator._2 -> Eps(Vector(end._1)))
        if atomic(t) then
          val guard = assert(AssertKind.NotBetweenCRLF)
          emit(guard._2 -> Eps(Vector(terminator._1)))
          (guard._1, end._2)
        else (terminator._1, end._2)

  private def assert(kind: AssertKind): (Int, Int) =
    val i = nextQ()
    val a = nextQ()
    emit(i -> Assert(kind, a))
    (i, a)

  private def epsilon(): (Int, Int) =
    val i = nextQ()
    (i, i)
