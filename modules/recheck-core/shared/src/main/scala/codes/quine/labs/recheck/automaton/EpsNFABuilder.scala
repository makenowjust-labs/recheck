package codes.quine.labs.recheck
package automaton

import scala.collection.mutable

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.ClassSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.dialect.LineTerminatorSet
import codes.quine.labs.resyntax.ir.IRAssertKind
import codes.quine.labs.resyntax.ir.IRBacktrackControl
import codes.quine.labs.resyntax.ir.IRCaseEquivalence
import codes.quine.labs.resyntax.ir.IRCondition
import codes.quine.labs.resyntax.ir.IRLookBehindMatch
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
  def build(pattern: IRPattern)(using ctx: Context): EpsNFA[Int] =
    build(pattern, Int.MaxValue, false, false)._1

  /** A simplified ε-NFA, the ordered NFA converted from it, and whether their cost bounds the cost of the pattern from
    * above.
    */
  final case class Simplified(epsNFA: EpsNFA[Int], orderedNFA: OrderedNFA[CharSet, Int], isUpperBound: Boolean)

  /** Compiles RegExp IR into a simplified ε-NFA, which approximates the constructs that `build` does not support, and
    * converts it into an ordered NFA without telling whether a look-around succeeds (see [[EpsNFA.toOrderedNFA]]).
    *
    * With `lookBehindsAsLookAheads`, a look-behind is a look-ahead of its inner expression. It throws
    * [[UnsupportedException]] when the automata would be larger than `maxNFASize`.
    */
  def buildSimplified(pattern: IRPattern, maxNFASize: Int, lookBehindsAsLookAheads: Boolean)(using
      ctx: Context
  ): Simplified =
    val (epsNFA, isUpperBound) = build(pattern, maxNFASize, true, lookBehindsAsLookAheads)
    // The DFA of the look-behinds of the simplified ε-NFA has the single state.
    Simplified(epsNFA, epsNFA.toOrderedNFA(maxNFASize, Int.MaxValue, true).rename, isUpperBound)

  private def build(pattern: IRPattern, maxNFASize: Int, isSimplified: Boolean, lookBehindsAsLookAheads: Boolean)(using
      ctx: Context
  ): (EpsNFA[Int], Boolean) =
    ctx.interrupt:
      val alphabet = PatternAlphabet.of(pattern)
      val builder = new EpsNFABuilder(pattern, alphabet, isSimplified, lookBehindsAsLookAheads, Set.empty, maxNFASize)
      val epsNFA = builder.build()
      ctx.log:
        s"""|automaton: EpsNFA construction
            |     state size: ${epsNFA.stateSet.size}
            |  alphabet size: ${epsNFA.alphabet.pairs.size}""".stripMargin
      (epsNFA, builder.isUpperBound)

private class EpsNFABuilder(
    private val pattern: IRPattern,
    private val alphabet: PatternAlphabet,
    private val isSimplified: Boolean,
    private val lookBehindsAsLookAheads: Boolean,
    /** The indices of the captures that the ε-NFA marks (see [[CaptureMark]]), where the 0th is the whole match. A
      * marked capture or `\K` in the inner expression of a look-around is unsupported.
      */
    private val marks: Set[Int],
    private val maxNFASize: Int
)(using ctx: Context):

  /** Whether the cost of the simplified ε-NFA bounds the cost of the pattern from above. */
  var isUpperBound = true

  /** A cached result of `IRPattern#needsInputTerminatorDistinction`. */
  private val needsInputTerminatorDistinction = pattern.needsInputTerminatorDistinction

  /** A next state counter. */
  private var counterQ = 0

  private val refineCache: mutable.Map[CharSet, Set[(CharSet, CharKind)]] = mutable.Map.empty

  /** Returns a next state number, and throws [[UnsupportedException]] if there are more than `maxNFASize` states. */
  def nextQ(): Int =
    val q = counterQ
    counterQ += 1
    if counterQ > maxNFASize then
      ctx.log("auto: exceed maxNFASize on EpsNFA construction")
      throw new UnsupportedException("EpsNFA size is too large")
    if isBackward then innerStates.add(q)
    path.addOne(q)
    q

  /** The states made on the paths of the node being built, except the inner expressions of the look-arounds and the
    * tests of the atomic groups.
    */
  private var path: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty

  /** Builds with `f`, and returns the states that it makes on the paths too, which are not on the current paths. */
  private def withPath[A](f: => A): (A, Seq[Int]) =
    val saved = path
    path = mutable.ArrayBuffer.empty
    val result = f
    val made = path.toVector
    path = saved
    (result, made)

  /** The states of the inner expressions of the look-behinds. */
  private val innerStates: mutable.Set[Int] = mutable.Set.empty

  /** The states that the copies for `(*NOTEMPTY)` keep for the inner expressions of the look-behinds, which they share
    * instead.
    */
  private val unusedStates: mutable.Set[Int] = mutable.Set.empty

  /** A next loop counter. */
  private var counterLoop = 0

  /** Returns a next loop number. */
  def nextLoop(): Int =
    val loop = counterLoop
    counterLoop += 1
    loop

  /** A transition function. */
  private val tau: mutable.Map[Int, Transition[Int]] = mutable.Map.empty

  /** A transition function of the inner expressions of the look-behinds. */
  private val innerTau: mutable.Map[Int, Transition[Int]] = mutable.Map.empty

  /** The states of `\K`, each of which has an ε-transition. */
  private val resetMatchStarts: mutable.Set[Int] = mutable.Set.empty

  /** Emits a transition. */
  def emit(t: (Int, Transition[Int])): Unit =
    if isBackward then innerTau.addOne(t) else tau.addOne(t)

  /** Builds EpsNFA for the pattern. */
  def build(): EpsNFA[Int] =
    val start = counterQ
    val (i0, a00) = buildMarkedCapture(0)(buildNode(pattern.node))
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

    // A match ends at its first success, which no priority tells in the simplified ε-NFA.
    // So it does not read the rest of the input, where a path reaching the end of the pattern stops.
    if pattern.needsSigmaStarAtEnd && !isSimplified then
      val loop = nextLoop()
      val a1 = nextQ()
      val a2 = nextQ()
      val a3 = nextQ()
      val a4 = nextQ()
      emit(a0 -> Eps(Vector(a3, a1)))
      emit(a1 -> LoopEnter(loop, a2))
      emit(a2 -> Consume(alphabet.any, a0))
      emit(a3 -> LoopExit(loop, a4))

    val lookBehind = Option.when(innerStates.nonEmpty)(LookBehind(innerStates.toSet, innerTau.toMap))
    EpsNFA(alphabet, (0 until counterQ).toSet -- innerStates -- unusedStates, init, tau.toMap, lookBehind)

  /** Makes the pattern's states between `start` and the current counter reject an empty match, as PCRE's `(*NOTEMPTY)`
    * and `(*NOTEMPTY_ATSTART)` do, and returns the initial states of the search at the start and after it, and the
    * final state.
    */
  private def rejectEmpty(start: Int, i0: Int, a0: Int): (Int, Int, Int) =
    pattern.notEmpty match
      case IRNotEmpty.Never => (i0, i0, a0)
      case notEmpty         =>
        // The states are doubled into a copy before consuming a character and one after it (a product with a 2-state
        // DFA), and only the latter accepts.
        // It keeps the order of the transitions, so the engine backtracks from an empty match as the ε-NFA does from a
        // dead state.
        val end = counterQ
        val shift = end - start
        counterQ += shift
        def moved(q: Int): Int = q + shift
        // The copies share the inner expressions of the look-behinds, which reject nothing.
        unusedStates.addAll((start until end).filter(innerStates.contains).map(moved))

        def retarget(t: Transition[Int], f: Int => Int): Transition[Int] = t match
          case Eps(qs)                             => Eps(qs.map(f))
          case Assert(kind, q)                     => Assert(kind, f(q))
          case Consume(set, q, loc)                => Consume(set, f(q), loc)
          case LoopEnter(loop, q)                  => LoopEnter(loop, f(q))
          case LoopExit(loop, q)                   => LoopExit(loop, f(q))
          case LookAheadAssert(negative, init, q)  => LookAheadAssert(negative, f(init), f(q))
          case LookBehindAssert(negative, init, q) => LookBehindAssert(negative, init, f(q))
          case AtomicEps(alternatives, last)       =>
            AtomicEps(alternatives.map((test, q) => (f(test), f(q))), f(last))
          case LookAroundAccept()   => LookAroundAccept()
          case MarkCapture(mark, q) => MarkCapture(mark, f(q))

        for q <- start until end; t <- tau.get(q) do
          // `\K` returns to the first copy for `(*NOTEMPTY)`, which sees the reported match.
          val reset = notEmpty == IRNotEmpty.Anywhere && resetMatchStarts.contains(q)
          emit(moved(q) -> (if reset then t else retarget(t, moved)))
          t match
            case c: Consume[Int] => emit(q -> retarget(c, moved))
            case _               => ()
        emit(a0 -> Eps(Vector.empty))
        // For `(*NOTEMPTY_ATSTART)`, the search after the start begins in the second copy.
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
      case IRNodeData.Capture(i, _, n) => buildMarkedCapture(i)(buildNode(n))
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
      case IRNodeData.ResetMatchStart if ignoresAssertions                   => epsilon()
      case IRNodeData.ResetMatchStart if marks.contains(0) && isInLookAround =>
        throw new UnsupportedException("\\K in look-around assertion")
      case IRNodeData.ResetMatchStart =>
        val (i, a) = (nextQ(), nextQ())
        // `\K` begins the reported match again.
        emit(i -> (if marks.contains(0) then MarkCapture(CaptureMark.Begin(0), a) else Eps(Vector(a))))
        resetMatchStarts.add(i)
        (i, a)
      case IRNodeData.BacktrackControl(_: IRBacktrackControl.Mark) => epsilon()
      case IRNodeData.BacktrackControl(_) if isSimplified          => epsilon()
      case IRNodeData.BacktrackControl(_)             => throw new UnsupportedException("backtracking control verb")
      case IRNodeData.Conditional(condition, yes, no) => buildNode(conditionalChoice(condition, yes, no))
      case IRNodeData.LookAhead(negative, n)          =>
        val ((i0, a0), _) = withPath(buildInLookAround(buildNode(n)))
        val i = nextQ()
        val a = nextQ()
        emit(i -> LookAheadAssert(negative, i0, a))
        emit(a0 -> LookAroundAccept())
        (i, a)
      case IRNodeData.NonAtomicLookAhead(n) if isSimplified =>
        // Backtracking into it tries the continuation again for each match of the inner expression, which the cost of
        // the atomic one does not count.
        isUpperBound = false
        buildNode(node.copy(data = IRNodeData.LookAhead(false, n)))
      case IRNodeData.NonAtomicLookAhead(_) => throw new UnsupportedException("look-ahead assertion")
      case IRNodeData.LookBehind(negative, n, _) if lookBehindsAsLookAheads =>
        // It reads the input after the position instead of the one before.
        isUpperBound = false
        buildNode(node.copy(data = IRNodeData.LookAhead(negative, n)))
      case IRNodeData.LookBehind(negative, n, matching) =>
        validateLookBehindInner(n, matching)
        val wasBackward = isBackward
        isBackward = true
        val ((i0, a0), _) = withPath(buildInLookAround(buildNode(reverseLookBehindInner(n))))
        emit(a0 -> LookAroundAccept())
        isBackward = wasBackward
        val i = nextQ()
        val a = nextQ()
        emit(i -> LookBehindAssert(negative, i0, a))
        (i, a)
      case IRNodeData.NonAtomicLookBehind(n, matching) if isSimplified =>
        isUpperBound = false
        buildNode(node.copy(data = IRNodeData.LookBehind(false, n, matching)))
      case IRNodeData.NonAtomicLookBehind(_, _) => throw new UnsupportedException("look-behind assertion")
      case IRNodeData.Atomic(n)                 => buildAtomic(n)
      case IRNodeData.BackReference(indices, equivalence, _) if isSimplified =>
        // It matches the strings equivalent to what the groups matched, which their bodies need not match.
        if equivalence != IRCaseEquivalence.Identity then isUpperBound = false
        buildBackReference(indices)
      case IRNodeData.BackReference(_, _, _) => throw new UnsupportedException("back-reference")
      case IRNodeData.Unsupported(_)         => throw new UnsupportedException(node.unsupportedMessage)

  /** Returns the choice that a conditional is: the assertion followed by the yes-branch and its negation followed by
    * the no-branch, or the two branches for a conditional on a capture in the simplified ε-NFA.
    */
  private def conditionalChoice(condition: IRCondition, yes: IRNode, no: IRNode): IRNode =
    def choice(assertion: Boolean => IRNodeData): IRNode =
      def branch(negated: Boolean, n: IRNode): IRNode =
        IRNode(IRNodeData.Sequence(Vector(IRNode(assertion(negated)), n)))
      IRNode(IRNodeData.Disjunction(Vector(branch(false, yes), branch(true, no))))
    condition match
      case IRCondition.LookAhead(negative, n) => choice(negated => IRNodeData.LookAhead(negative != negated, n))
      case IRCondition.LookBehind(negative, n, matching) =>
        choice(negated => IRNodeData.LookBehind(negative != negated, n, matching))
      case IRCondition.Captured(_) if isSimplified => IRNode(IRNodeData.Disjunction(Vector(yes, no)))
      case IRCondition.Captured(_)                 => throw new UnsupportedException("conditional")

  /** The bodies of the capture groups by the indices. */
  private lazy val captureBodies: Map[Int, Seq[IRNode]] =
    pattern.node.collect { case IRNodeData.Capture(index, _, n) => (index, n) }.groupMap(_._1)(_._2)

  /** The indices of the groups whose bodies are being built for a back-reference. */
  private var referredIndices: Set[Int] = Set.empty

  /** Whether the builder builds the assertions and `\K` as ε, in a referred group for a back-reference. */
  private var ignoresAssertions = false

  /** Builds EpsNFA transitions for a back-reference to the indices in the simplified ε-NFA, which is a choice of the
    * groups' bodies without assertions and the empty string.
    */
  private def buildBackReference(indices: Seq[Int]): (Int, Int) =
    // A back-reference in the group it refers to would copy the group endlessly.
    if indices.exists(referredIndices.contains) then
      isUpperBound = false
      epsilon()
    else
      // A back-reference matches a string that one of the groups matched, or the empty string for an unset group.
      // The assertions in the group held where it matched, but they need not hold here.
      // The IR has no body for a group in a skipped branch (e.g. PCRE's `(?(DEFINE)(a))`), which is always unset.
      val bodies = for index <- indices; n <- captureBodies.getOrElse(index, Seq.empty) yield () =>
        val (oldReferred, oldIgnores) = (referredIndices, ignoresAssertions)
        referredIndices = referredIndices ++ indices
        ignoresAssertions = true
        val built = buildNode(if isBackward then reverseLookBehindInner(n) else n)
        referredIndices = oldReferred
        ignoresAssertions = oldIgnores
        built
      buildChoice(bodies :+ (() => epsilon()))

  /** Whether the builder builds the inner expression of a look-around. */
  private var isInLookAround = false

  /** Builds EpsNFA transitions marking the begin and the end of the `i`-th capture around `build`, if it is marked. */
  private def buildMarkedCapture(i: Int)(build: => (Int, Int)): (Int, Int) =
    if !marks.contains(i) then build
    else if isInLookAround then throw new UnsupportedException("capture in look-around assertion")
    else
      val begin = nextQ()
      val (i0, a0) = build
      val end = nextQ()
      emit(begin -> MarkCapture(CaptureMark.Begin(i), i0))
      emit(a0 -> MarkCapture(CaptureMark.End(i), end))
      (begin, end)

  /** Builds the inner expression of a look-around. */
  private def buildInLookAround(build: => (Int, Int)): (Int, Int) =
    val wasInLookAround = isInLookAround
    isInLookAround = true
    val built = build
    isInLookAround = wasInLookAround
    built

  /** Builds EpsNFA transitions for an atomic group.
    *
    * A choice of the group tries the alternatives in order, and commits to the first one that reaches the end of the
    * group ([[AtomicEps]]). The test of an alternative is its state in a copy of the paths of the group, which ends
    * with a [[LookAroundAccept]] at the end of the group, and which only the [[AtomicEps]] refer to. The last
    * alternative has no test, since it fails within the group anyway if it does not reach the end.
    */
  private def buildAtomic(n: IRNode): (Int, Int) =
    val ((i0, a0), groupPath) = withPath(buildNode(n))
    path.addAll(groupPath)
    val transitions = if isBackward then innerTau else tau
    val (test, _) = withPath(groupPath.map(q => q -> nextQ()).toMap)
    for q <- groupPath do
      // The copy of `\K` resets the reported match too (see `rejectEmpty`).
      if resetMatchStarts.contains(q) then resetMatchStarts.add(test(q))
      // The copy shares the inner expressions of the look-arounds and the tests of the nested atomic groups.
      val t = transitions.get(q) match
        case Some(Eps(qs))                              => Eps(qs.map(test))
        case Some(Assert(kind, q1))                     => Assert(kind, test(q1))
        case Some(Consume(set, q1, loc))                => Consume(set, test(q1), loc)
        case Some(LoopEnter(loop, q1))                  => LoopEnter(loop, test(q1))
        case Some(LoopExit(loop, q1))                   => LoopExit(loop, test(q1))
        case Some(LookAheadAssert(negative, init, q1))  => LookAheadAssert(negative, init, test(q1))
        case Some(LookBehindAssert(negative, init, q1)) => LookBehindAssert(negative, init, test(q1))
        case Some(AtomicEps(alternatives, last))        =>
          AtomicEps(alternatives.map((t, q1) => (t, test(q1))), test(last))
        // The tests set no captures, which the alternatives set.
        case Some(MarkCapture(_, q1)) => Eps(Vector(test(q1)))
        // `a0` is the end of the group, which has no transition yet.
        case Some(LookAroundAccept()) | None => LookAroundAccept()
      emit(test(q) -> t)
    for q <- groupPath do
      transitions.get(q) match
        case Some(Eps(qs)) if qs.size > 1 => emit(q -> AtomicEps(qs.init.map(q1 => (test(q1), q1)), qs.last))
        case _                            => ()
    (i0, a0)

  /** Builds EpsNFA transitions for a repetition.
    *
    * `x{n,}` is built as `n - 1` copies of `x` and `x+`, and `x{n,m}` as `n` copies of `x` and nested `(x(x)?)?`.
    */
  private def buildRepeat(n: IRNode, q: IRQuantifier): (Int, Int) =
    q match
      case IRQuantifier.Unbounded(0, strategy)   => buildStar(n, strategy)
      case IRQuantifier.Unbounded(min, strategy) =>
        buildSequence(Vector.fill(min - 1)(() => buildIteration(n)) :+ (() => buildPlus(n, strategy)))
      case IRQuantifier.Exact(m) =>
        buildSequence(Vector.fill(m)(() => buildIteration(n)))
      case IRQuantifier.Bounded(min, max, strategy) =>
        def question(n: IRNode): IRNode = IRNode(IRNodeData.Repeat(n, IRQuantifier.Bounded(0, 1, strategy)))

        val maxN = Option.when(max > min):
          Vector
            .fill(max - min)(n)
            .reduceRight((l, r) => IRNode(IRNodeData.Sequence(Vector(l, question(r)))))
        buildSequence(Vector.fill(min)(() => buildIteration(n)) ++ maxN.map(m => () => buildQuestion(m, strategy)))

  /** Builds EpsNFA transitions for an iteration of a repetition, which unsets the marked captures in it first if each
    * iteration resets them.
    */
  private def buildIteration(n: IRNode): (Int, Int) =
    val unsets =
      if !pattern.captures.resetsEachIteration then Seq.empty
      else n.captureRange.toSeq.flatMap((min, max) => (min to max).filter(marks.contains))
    if unsets.isEmpty then buildNode(n)
    else
      val i = nextQ()
      val (i0, a0) = buildNode(n)
      val last = unsets.init.foldLeft(i): (q, k) =>
        val q1 = nextQ()
        emit(q -> MarkCapture(CaptureMark.Unset(k), q1))
        q1
      emit(last -> MarkCapture(CaptureMark.Unset(unsets.last), i0))
      (i, a0)

  /** Builds EpsNFA transitions for `x*`. */
  private def buildStar(n: IRNode, strategy: BacktrackStrategy): (Int, Int) =
    //      +---------------------+
    //      |                     |
    //  (i)-+->(i2)->[i0-->a0]-+  +->(a1)->(a)
    //   ^                     |
    //   +---------------------+
    val (i0, a0) = buildIteration(n)
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
    val (i, a0) = buildIteration(n)
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
    val (i0, a) = buildIteration(n)
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
    def atomic(t: LineTerminatorSet): Boolean = t.crlf == LineTerminatorSet.CRLF.Atomic

    def plain(t: LineTerminatorSet): Boolean = t.crlf == LineTerminatorSet.CRLF.Ignored

    def boundary(kind: AssertKind): (Int, Int) = if needsInputTerminatorDistinction then assert(kind) else epsilon()

    kind match
      case IRAssertKind.LineBegin(t, atEnd) =>
        assert(AssertKind.LineBegin(kindLineTerminators.indexOf(t), atEnd, !plain(t) && !t.chars(0x0a), atomic(t)))
      case IRAssertKind.LineEnd(t) =>
        assert(AssertKind.LineEnd(kindLineTerminators.indexOf(t), !plain(t) && !t.chars(0x0d), atomic(t)))
      case IRAssertKind.TextBegin                                               => boundary(AssertKind.TextBegin)
      case IRAssertKind.TextEnd                                                 => boundary(AssertKind.TextEnd)
      case IRAssertKind.ChompTextEnd(t) if chompTextEndsInMiddle.contains(node) =>
        assert(AssertKind.ChompTextEnd(kindLineTerminators.indexOf(t), !plain(t), atomic(t)))
      case IRAssertKind.ChompTextEnd(t) =>
        // At an end point, it is the end after an optional line terminator.
        val chars = CharSet.of(t.chars.toSeq*)
        val single = Option.when(chars.nonEmpty)(() => consume(refine(chars), None))
        val crlf = Option.when(!plain(t)): () =>
          buildSequence(
            Seq(() => consume(refine(CharSet.single(0x0d)), None), () => consume(refine(CharSet.single(0x0a)), None))
          )
        val terminator = buildChoice(Seq(() => epsilon()) ++ single ++ crlf)
        val end = boundary(AssertKind.TextEnd)
        emit(terminator._2 -> Eps(Vector(end._1)))
        if atomic(t) then
          val guard = assert(AssertKind.NotBetweenCRLF)
          emit(guard._2 -> Eps(Vector(terminator._1)))
          (guard._1, end._2)
        else (terminator._1, end._2)
      case IRAssertKind.WordBoundary(w)        => assert(AssertKind.WordBoundary(wordSets.indexOf(w)))
      case IRAssertKind.NotWordBoundary(w)     => assert(AssertKind.NotWordBoundary(wordSets.indexOf(w)))
      case IRAssertKind.MarkWordBoundary(w, m) =>
        assert(AssertKind.MarkWordBoundary(wordSets.indexOf(w), wordMarks.indexOf(m)))
      case IRAssertKind.NotMarkWordBoundary(w, m) =>
        assert(AssertKind.NotMarkWordBoundary(wordSets.indexOf(w), wordMarks.indexOf(m)))
      case IRAssertKind.NotBetweenCRLF => assert(AssertKind.NotBetweenCRLF)

  private def assert(kind: AssertKind): (Int, Int) =
    val inLookBehind = isBackward && !kind.looksOnlyAtKinds
    if ignoresAssertions || (inLookBehind && isSimplified) then epsilon()
    else if inLookBehind then throw new UnsupportedException("assertion in look-behind assertion")
    else
      val i = nextQ()
      val a = nextQ()
      emit(i -> Assert(kind, a))
      (i, a)

  /** Whether the builder builds the inner expression of a look-behind, which reads the input backward. */
  private var isBackward = false

  /** Reverses the inner expression of a look-behind, so that its ε-NFA reads the input backward.
    *
    * A nested look-behind assertion reads the input in the same direction as the inner expression, so it is a
    * look-ahead assertion of the reversed one.
    *
    * It throws [[UnsupportedException]] on a look-ahead assertion in the inner expression, which is ε in the simplified
    * ε-NFA.
    */
  private def reverseLookBehindInner(node: IRNode): IRNode = ctx.interrupt:
    // An engine may match the inner expression forward from its starts (Java, PCRE, Perl and Python).
    // That succeeds exactly when matching it backward does, because the starts are those of its widths.
    node.data match
      case IRNodeData.Disjunction(ns) => node.copy(data = IRNodeData.Disjunction(ns.map(reverseLookBehindInner)))
      case IRNodeData.Sequence(ns)    => node.copy(data = IRNodeData.Sequence(ns.reverse.map(reverseLookBehindInner)))
      case IRNodeData.Capture(i, name, n) => node.copy(data = IRNodeData.Capture(i, name, reverseLookBehindInner(n)))
      case IRNodeData.Atomic(n)           => node.copy(data = IRNodeData.Atomic(reverseLookBehindInner(n)))
      case IRNodeData.Repeat(n, q)        => node.copy(data = IRNodeData.Repeat(reverseLookBehindInner(n), q))
      case IRNodeData.Strings(set)        =>
        val strings = set.strings.map(s => UString.from(s.iterator.toSeq.reverse))
        node.copy(data = IRNodeData.Strings(ClassSet(set.chars, strings)))
      case IRNodeData.LookAhead(_, _) | IRNodeData.NonAtomicLookAhead(_) if isSimplified =>
        // The cost of its inner expression, which reads the input forward, is lost.
        isUpperBound = false
        node.copy(data = IRNodeData.Empty)
      case IRNodeData.LookAhead(_, _) => throw new UnsupportedException("look-ahead assertion in look-behind assertion")
      case IRNodeData.LookBehind(negative, n, matching) =>
        validateLookBehindInner(n, matching)
        node.copy(data = IRNodeData.LookAhead(negative, reverseLookBehindInner(n)))
      case IRNodeData.NonAtomicLookBehind(n, _) if isSimplified =>
        isUpperBound = false
        node.copy(data = IRNodeData.LookAhead(false, reverseLookBehindInner(n)))
      case IRNodeData.Conditional(condition, yes, no) => reverseLookBehindInner(conditionalChoice(condition, yes, no))
      case _                                          => node

  /** Throws [[UnsupportedException]] unless the automaton checker supports the inner expression of a look-behind that
    * the engine matches as `matching`.
    *
    * An inner expression matched forward with an unbounded repetition (e.g. Java's `(?<=a*b)`) or an atomic group is
    * unsupported, except in the simplified ε-NFA.
    */
  private def validateLookBehindInner(n: IRNode, matching: IRLookBehindMatch): Unit =
    // The engine tries it from each start in turn, which no backward path of the reversed one counts.
    // Its cost at a position is bounded if the inner expression is.
    // An atomic group finds a different first success forward than backward.
    // For example, `(?<=(?>a|ab)c)` succeeds backward after `abc`, but not forward.
    val unsupported = n.exists:
      case IRNodeData.Repeat(_, _: IRQuantifier.Unbounded) => true
      case IRNodeData.Atomic(_)                            => true
      case _                                               => false
    if unsupported && matching != IRLookBehindMatch.Backward then
      if isSimplified then isUpperBound = false
      else throw new UnsupportedException("look-behind assertion matched forward")

  private def epsilon(): (Int, Int) =
    val i = nextQ()
    (i, i)
