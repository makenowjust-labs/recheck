package codes.quine.labs.recheck
package automaton

import scala.collection.mutable

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.util.GraphvizUtil.escape
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.alphabet.CharKind
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** EpsNFA is an ordered ε-NFA on the units of the pattern's unit mode, whose accepting states have no transitions.
  *
  * `lookBehind` is the ε-NFA of the inner expressions of its look-behind assertions.
  */
final case class EpsNFA[Q](
    alphabet: PatternAlphabet,
    stateSet: Set[Q],
    init: Q,
    tau: Map[Q, Transition[Q]],
    lookBehind: Option[LookBehind[Q]] = None
):

  /** Converts to Graphviz format text. */
  def toGraphviz: String =
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append(s"  ${escape("")} [shape=point];\n")
    sb.append(s"  ${escape("")} -> ${escape(init)};\n")
    val transitions = stateSet.iterator.map(q => (q, tau.get(q))) ++
      lookBehind.iterator.flatMap(lb => lb.stateSet.iterator.map(q => (q, lb.tau.get(q))))
    for (q0, t) <- transitions do
      t match
        case Some(Eps(Seq(q1))) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)};\n")
        case Some(Eps(qs)) =>
          sb.append(s"  ${escape(q0)} [shape=diamond];\n")
          for (q1, i) <- qs.zipWithIndex do sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=$i];\n")
        case Some(Assert(k, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(k)}];\n")
        case Some(Consume(chs, q1, _)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(
            s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(chs.map((c, k) => s"($c,$k)").mkString("{", ", ", "}"))}];\n"
          )
        case Some(LoopEnter(loop, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"Enter($loop)")}];\n")
        case Some(LoopExit(loop, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"Exit($loop)")}];\n")
        case Some(MarkCapture(mark, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"MarkCapture($mark)")}];\n")
        case Some(IterationEnd(loop, q1, empty)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"End($loop)")}];\n")
          for q2 <- empty do sb.append(s"  ${escape(q0)} -> ${escape(q2)} [label=${escape(s"Empty($loop)")}];\n")
        case Some(LookAheadAssert(negative, init, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=diamond];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(init)} [label=${escape(if negative then "?!" else "?=")}];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)};\n")
        case Some(LookBehindAssert(negative, init, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=diamond];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(init)} [label=${escape(if negative then "?<!" else "?<=")}];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)};\n")
        case Some(LookAroundAccept()) =>
          sb.append(s"  ${escape(q0)} [shape=doubleoctagon];\n")
        case Some(AtomicEps(alternatives, last)) =>
          sb.append(s"  ${escape(q0)} [shape=diamond];\n")
          for ((t, q1), i) <- alternatives.zipWithIndex do
            sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"$i if $t")}];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(last)} [label=${alternatives.size}];\n")
        case None =>
          sb.append(s"  ${escape(q0)} [shape=doublecircle];\n")
    sb.append("}")

    sb.result()

  /** Renames the states to integers. */
  def rename: EpsNFA[Int] = mapState((stateSet ++ lookBehind.iterator.flatMap(_.stateSet)).zipWithIndex.toMap)

  /** Maps the states by the function. */
  def mapState[R](f: Q => R): EpsNFA[R] =
    EpsNFA(alphabet, stateSet.map(f), f(init), tau.map((q, t) => f(q) -> t.mapState(f)), lookBehind.map(_.mapState(f)))

  /** Folds the marks into the states, each of which has the marks since the last character.
    *
    * The [[toOrderedNFA]] of the result with the marks of the states tells the marks before each character and at the
    * end of the input. The marks before a positive look-around go with it, so they come before what its inner
    * expression sets: the inner expression of a look-ahead takes them, the state of a look-behind keeps them, and the
    * continuation starts without them. The inner expressions of the other look-arounds start without marks.
    *
    * The marks keep the last ones on the begin and the end of each capture (see [[CaptureMark.append]]), which tell the
    * captures, so an ε-loop does not add marks forever.
    */
  def foldCaptureMarks: EpsNFA[(Q, Seq[CaptureMark])] =
    type R = (Q, Seq[CaptureMark])

    val innerStateSet = lookBehind.fold(Set.empty[Q])(_.stateSet)
    val innerTau = lookBehind.fold(Map.empty[Q, Transition[Q]])(_.tau)
    val newTau = mutable.Map.empty[R, Transition[R]]
    val newInit = (init, Seq.empty[CaptureMark])
    val queue = mutable.Queue(newInit)
    val visited = mutable.Set(newInit)

    def visit(r: R): R =
      if visited.add(r) then queue.enqueue(r)
      r

    while queue.nonEmpty do
      val r @ (q, since) = queue.dequeue()
      def same(q1: Q): R = visit((q1, since))

      def fresh(q1: Q): R = visit((q1, Seq.empty))

      (if innerStateSet.contains(q) then innerTau.get(q) else tau.get(q)) match
        case Some(Eps(qs))                       => newTau(r) = Eps(qs.map(same))
        case Some(Assert(kind, q1))              => newTau(r) = Assert(kind, same(q1))
        case Some(Consume(chs, q1, loc))         => newTau(r) = Consume(chs, fresh(q1), loc)
        case Some(LoopEnter(loop, q1))           => newTau(r) = LoopEnter(loop, same(q1))
        case Some(LoopExit(loop, q1))            => newTau(r) = LoopExit(loop, same(q1))
        case Some(IterationEnd(loop, q1, empty)) => newTau(r) = IterationEnd(loop, same(q1), empty.map(same))
        case Some(MarkCapture(mark, q1))         =>
          newTau(r) = Eps(Vector(visit((q1, CaptureMark.append(since, mark)))))
        case Some(LookAheadAssert(false, init, q1))     => newTau(r) = LookAheadAssert(false, same(init), fresh(q1))
        case Some(LookAheadAssert(true, init, q1))      => newTau(r) = LookAheadAssert(true, fresh(init), same(q1))
        case Some(LookBehindAssert(negative, init, q1)) =>
          newTau(r) = LookBehindAssert(negative, fresh(init), if negative then same(q1) else fresh(q1))
        case Some(LookAroundAccept())            => newTau(r) = LookAroundAccept()
        case Some(AtomicEps(alternatives, last)) =>
          newTau(r) = AtomicEps(alternatives.map((test, q1) => (fresh(test), same(q1))), same(last))
        case None => ()

    def isInner(r: R): Boolean = innerStateSet.contains(r._1)
    val newLookBehind = lookBehind.map: _ =>
      LookBehind(visited.toSet.filter(isInner), newTau.toMap.filter((r, _) => isInner(r)))
    EpsNFA(
      alphabet,
      visited.toSet.filterNot(isInner),
      newInit,
      newTau.toMap.filterNot((r, _) => isInner(r)),
      newLookBehind
    )

  /** Restricts the input of the NFA renamed from [[toOrderedNFA]] to the strings where no low surrogate follows a high
    * surrogate, if the kinds tell them (see [[CharKind.high]]).
    */
  def restrictInputs(nfa: OrderedNFA[CharSet, Int]): OrderedNFA[CharSet, Int] =
    val highs = alphabet.pairs.collect { case (c, k) if k.high => c }.toSet
    val lows = alphabet.pairs.collect { case (c, k) if k.low => c }.toSet
    if highs.isEmpty || lows.isEmpty then nfa
    else
      val free = nfa.stateSet.maxOption.getOrElse(-1) + 1
      nfa.restrictInputs(highs, lows, free, free + 1)

  /** Converts this ε-NFA to ordered NFA with ε-elimination.
    *
    * Its state is a [[CharKindState]], an ε-closure, whose leaves are the [[Stop]]s that the ε-transitions reach, and
    * the state of the DFA of the [[LookBehindAutomaton]] of [[lookBehind]], which has less than `maxDFASize`
    * transitions (see [[OrderedNFA.toLookAheadDFA]]).
    *
    * With `isSimplified`, it converts a simplified ε-NFA (see [[EpsNFABuilder.buildSimplified]]) without telling
    * whether a look-around succeeds: a look-around is a choice of its inner expression, whose end may end the match,
    * and the continuation, and a choice in an atomic group is a choice of all its alternatives.
    *
    * With `captureMarksOf`, which tells the marks of an ε-NFA state (see [[foldCaptureMarks]]), the result has
    * [[OrderedNFA.markedDelta]] too.
    */
  def toOrderedNFA(
      isSimplified: Boolean,
      maxNFASize: Int,
      maxDFASize: Int,
      captureMarksOf: Option[Q => Seq[CaptureMark]] = None
  )(using
      ctx: Context
  ): OrderedNFA[CharSet, (CharKindState, Branch[Stop[Q]], Set[Int])] =
    ctx.interrupt:
      val kinds = (tau.values ++ lookBehind.iterator.flatMap(_.tau.values)).collect { case Assert(k, _) => k }.toSet
      val afterCRLF = kinds.exists:
        case AssertKind.LineBegin(_, _, afterCRLF, _) => afterCRLF
        case _                                        => false
      val last = kinds.exists(_.isInstanceOf[AssertKind.ChompTextEnd])
      val follow = kinds.exists:
        case AssertKind.LineEnd(_, beforeCRLF, _) => beforeCRLF
        case AssertKind.ChompTextEnd(_, crlf, _)  => crlf
        case _                                    => false
      val marks = kinds.exists:
        case _: AssertKind.MarkWordBoundary | _: AssertKind.NotMarkWordBoundary => true
        case _                                                                  => false
      val baseKinds = alphabet.pairs.iterator.map(_._2).toSet

      // The begin and the end of the input are distinguished only for the assertions looking at them, and they are
      // line terminators if the assertions see no difference, which keeps the automaton smaller.
      val inputTerminatorKind =
        if afterCRLF || last || follow || marks then CharKind.Boundary
        else if !kinds.exists(_.seesBoundary) then CharKind.Normal
        else if kinds.forall(_.treatsBoundaryAsLineTerminator(baseKinds)) then CharKind.LineTerminator
        else CharKind.Boundary
      val inputTerminator = CharKindState(inputTerminatorKind)

      def marksOf(q: Q): Seq[CaptureMark] = captureMarksOf.fold(Seq.empty)(_(q))

      type R = (CharKindState, Branch[Stop[Q]], Set[Int])

      val lookBehindInits = tau.collect { case (q, LookBehindAssert(_, init, _)) => q -> init }
      val charKinds = alphabet.any.toMap
      val newAlphabet = charKinds.keySet
      var deltaSize = 0

      def fragment(
          transitions: Map[Q, Transition[Q]],
          isBackward: Boolean,
          lookBehind: Option[LookBehindAutomaton[Q]]
      ): Fragment[Q] =
        Fragment(
          transitions,
          isBackward,
          isSimplified,
          lookBehind,
          lookBehindInits,
          afterCRLF,
          last,
          follow,
          baseKinds,
          inputTerminator,
          charKinds,
          marksOf
        )

      /** Builds an ordered NFA of the fragment from the initial states.
        *
        * A tried look-behind assertion is recorded as a transition into a state without transitions (an attempt), or at
        * the end of the input, with the initial state of the NFA of the inner expression.
        */
      def build(fragment: Fragment[Q], inits: Seq[R]): (OrderedNFA[CharSet, R], Map[R, Int], Map[R, Seq[Int]]) =
        val queue = mutable.Queue.from(inits)
        val newStateSet = mutable.Set.from(inits)
        val newAcceptSet = Set.newBuilder[R]
        val newAttemptsAtEnd = Map.newBuilder[R, Seq[Int]]
        val newDelta = Map.newBuilder[(R, CharSet), Branch[R]]
        val newSourcemap = mutable.Map.empty[(R, CharSet, R), Seq[SourceLocation]].withDefaultValue(Vector.empty)
        val newMarkedDelta = Map.newBuilder[(R, CharSet), Branch[MarkedStep[R]]]
        val newMarkedDeltaAtEnd = Map.newBuilder[R, Branch[MarkedStep[R]]]

        // The leaf of a step without the marks, where a success regardless of the rest of the input is `Succeed`.
        def plainLeaf(step: MarkedStep[R]): Branch[R] = step match
          case MarkedStep.Next(_, r) => Branch.Leaf(r)
          case _                     => Branch.Succeed

        // Records the state that the character leads to, if any, and returns the step.
        def visit(r0: R, ch: CharSet)(next: (MarkedStep[R], Option[SourceLocation])): MarkedStep[R] = next match
          case (step @ MarkedStep.Next(_, r1), loc) =>
            newSourcemap((r0, ch, r1)) ++= loc
            if newStateSet.add(r1) then queue.enqueue(r1)
            step
          case (step, _) => step

        while queue.nonEmpty do
          ctx.interrupt:
            val r0 @ (s0, stops, d) = queue.dequeue()
            val end = stops.flatMap(fragment.atEnd(d))
            if end.succeeds(_.isRight) then newAcceptSet.addOne(r0)
            val attemptsAtEnd = end.explore(_.isRight).collect { case Left(b) => b }
            if attemptsAtEnd.nonEmpty then newAttemptsAtEnd.addOne(r0 -> attemptsAtEnd)
            val markedAtEnd = end.flatMap(_.fold(_ => Branch.Fail, Branch.Leaf(_)))
            if captureMarksOf.nonEmpty && markedAtEnd != Branch.Fail then newMarkedDeltaAtEnd.addOne(r0 -> markedAtEnd)
            for ch <- stops.allLeaves.flatMap(fragment.charsOf).distinct do
              val marked = stops.flatMap(fragment.step(s0, d, ch)).map(visit(r0, ch))
              if captureMarksOf.nonEmpty && marked != Branch.Fail then newMarkedDelta.addOne((r0, ch) -> marked)
              val b = marked.flatMap(plainLeaf)
              if b != Branch.Fail then
                newDelta.addOne((r0, ch) -> b)
                deltaSize += 1
            if deltaSize >= maxNFASize then
              ctx.log("auto: exceed maxNFASize on OrderedNFA construction")
              throw new UnsupportedException("OrderedNFA size is too large")

        val nfa = OrderedNFA(
          newAlphabet,
          newStateSet.toSet,
          inits,
          newAcceptSet.result(),
          newDelta.result(),
          newSourcemap.toMap.filter(_._2.nonEmpty),
          markedDelta = newMarkedDelta.result(),
          markedDeltaAtEnd = newMarkedDeltaAtEnd.result()
        )
        val attempts = newStateSet.iterator.flatMap(r => fragment.attemptInit(r).map(r -> _)).toMap
        (nfa, attempts, newAttemptsAtEnd.result())

      val lookBehindAutomaton = lookBehind.map: lb =>
        val inner = fragment(lb.tau, true, None)
        // An inner expression starts at the state of the character after the position, which it reads before.
        val inits =
          for
            q <- lookBehindInits.values.toVector.distinct
            k <- (baseKinds + inputTerminatorKind).toVector
          yield
            val s = CharKindState(k).forgetMarks
            (q, k) -> (s, inner.closure(s, q), Set.empty[Int])
        // A look-behind nested in the inner expression is built as a look-ahead.
        // So the inner expression has no attempts, and they are ignored.
        val (nfa, _, _) = build(inner, inits.map(_._2).distinct)
        val index = nfa.stateSet.zipWithIndex.toMap
        ctx.log:
          s"""|automaton: look-behind NFA construction
              |     state size: ${nfa.stateSet.size}""".stripMargin
        val indexed = nfa.mapState(index)
        // The simplified ε-NFA does not tell whether a look-behind succeeds.
        val dfa =
          if isSimplified then DFA.universal(newAlphabet, Set.empty[Int]) else indexed.toLookAheadDFA(maxDFASize)
        LookBehindAutomaton(indexed, dfa, inits.map((init, r) => init -> index(r)).toMap)

      val pattern = fragment(tau, false, lookBehindAutomaton)
      val (patternNFA, attempts, attemptsAtEnd) = build(
        pattern,
        Vector((inputTerminator, pattern.closure(inputTerminator, init), lookBehindAutomaton.fold(Set.empty)(_.init)))
      )
      val orderedNFA = lookBehindAutomaton.fold(patternNFA): lb =>
        val lookBehind = OrderedNFA.LookBehind(
          lb.nfa,
          patternNFA.stateSet.iterator.map(r => r -> r._3).toMap,
          attempts,
          attemptsAtEnd
        )
        patternNFA.copy(lookBehind = Some(lookBehind))
      ctx.log:
        s"""|automaton: OrderedNFA construction
            |     state size: ${orderedNFA.stateSet.size}
            |  alphabet size: ${orderedNFA.alphabet.size}""".stripMargin
      orderedNFA

/** EpsNFA types and utilities. */
object EpsNFA:

  /** Transition is a transition of ε-NFA. */
  sealed abstract class Transition[Q] extends Serializable with Product:

    /** Maps the states of this transition by the function. */
    def mapState[R](f: Q => R): Transition[R] = this match
      case Eps(qs)                             => Eps(qs.map(f))
      case Assert(kind, q)                     => Assert(kind, f(q))
      case Consume(set, q, loc)                => Consume(set, f(q), loc)
      case LoopEnter(loop, q)                  => LoopEnter(loop, f(q))
      case LoopExit(loop, q)                   => LoopExit(loop, f(q))
      case MarkCapture(mark, q)                => MarkCapture(mark, f(q))
      case IterationEnd(loop, q, empty)        => IterationEnd(loop, f(q), empty.map(f))
      case LookAheadAssert(negative, init, q)  => LookAheadAssert(negative, f(init), f(q))
      case LookBehindAssert(negative, init, q) => LookBehindAssert(negative, f(init), f(q))
      case LookAroundAccept()                  => LookAroundAccept()
      case AtomicEps(alternatives, last)       => AtomicEps(alternatives.map((test, q) => (f(test), f(q))), f(last))

  /** Eps is an ε-NFA transition without consuming a character. */
  final case class Eps[Q](to: Seq[Q]) extends Transition[Q]

  /** Assert is an ε-NFA transition with consuming no character and assertion. */
  final case class Assert[Q](kind: AssertKind, to: Q) extends Transition[Q]

  /** Consume is an ε-NFA transition with consuming a character. */
  final case class Consume[Q](set: Set[(CharSet, CharKind)], to: Q, loc: Option[SourceLocation] = None)
      extends Transition[Q]

  /** LoopEnter is an ε-NFA transition with consuming no character and marking the entry to the loop. */
  final case class LoopEnter[Q](loop: Int, to: Q) extends Transition[Q]

  /** LoopExit is an ε-NFA transition with consuming no character and marking the exit from the loop. */
  final case class LoopExit[Q](loop: Int, to: Q) extends Transition[Q]

  /** MarkCapture is an ε-NFA transition with consuming no character and marking a capture. */
  final case class MarkCapture[Q](mark: CaptureMark, to: Q) extends Transition[Q]

  /** IterationEnd is an ε-NFA transition with consuming no character and ending an iteration of the loop.
    *
    * It goes to `to` if the iteration consumed a character, or to `empty` otherwise, failing without it.
    */
  final case class IterationEnd[Q](loop: Int, to: Q, empty: Option[Q]) extends Transition[Q]

  /** LookAheadAssert is an ε-NFA transition with a look-ahead assertion whose inner expression starts from `init`. */
  final case class LookAheadAssert[Q](negative: Boolean, init: Q, to: Q) extends Transition[Q]

  /** LookBehindAssert is an ε-NFA transition with a look-behind assertion.
    *
    * The inner expression starts from `init`, a state of the [[LookBehind]] of the ε-NFA.
    */
  final case class LookBehindAssert[Q](negative: Boolean, init: Q, to: Q) extends Transition[Q]

  /** LookAroundAccept is an end of the inner expression of a look-around. */
  final case class LookAroundAccept[Q]() extends Transition[Q]

  /** LookBehind is the ε-NFA of the inner expressions of look-behinds, which reads the input backward.
    *
    * An inner expression starts from the `init` of its [[LookBehindAssert]], and ends at a [[LookAroundAccept]]. Its
    * assertions see the character before the position as the next one.
    *
    * It does not nest: its transitions have no [[LookBehindAssert]], since a look-behind in an inner expression reads
    * the input in the same direction, and [[EpsNFABuilder]] builds it as a look-ahead of the reversed inner expression.
    */
  final case class LookBehind[Q](stateSet: Set[Q], tau: Map[Q, Transition[Q]]):

    /** Maps the states by the function. */
    def mapState[R](f: Q => R): LookBehind[R] = LookBehind(stateSet.map(f), tau.map((q, t) => f(q) -> t.mapState(f)))

  /** AtomicEps is an ε-NFA transition of a choice in an atomic group, which tries the alternatives `(test, to)` in
    * order, and commits to the first one whose `test` reaches a [[LookAroundAccept]], or else to `last`.
    *
    * `test` is the state of `to` in a copy of the group, which ends with the [[LookAroundAccept]] instead of going on
    * after the group.
    */
  final case class AtomicEps[Q](alternatives: Seq[(Q, Q)], last: Q) extends Transition[Q]

  /** Fragment is a part of an ε-NFA, which [[EpsNFA#toOrderedNFA]] uses. */
  private[automaton] final class Fragment[Q](
      transitions: Map[Q, Transition[Q]],
      isBackward: Boolean,
      isSimplified: Boolean,
      lookBehind: Option[LookBehindAutomaton[Q]],
      lookBehindInits: Map[Q, Q],
      afterCRLF: Boolean,
      last: Boolean,
      follow: Boolean,
      baseKinds: Set[CharKind],
      inputTerminator: CharKindState,
      charKinds: Map[CharSet, CharKind],
      marksOf: Q => Seq[CaptureMark]
  )(using ctx: Context):

    private type R = (CharKindState, Branch[Stop[Q]], Set[Int])

    /** A step with the locations read there. */
    private type Next = (MarkedStep[R], Option[SourceLocation])

    private val consumeMap: Map[Q, Consume[Q]] = transitions.collect { case (q, c @ Consume(_, _, _)) => q -> c }

    /** Returns the leaf at the end of the input, at the state `d` of the DFA of `lookBehind`, as a branch whose leaves
      * are the initial states of the NFA of the inner expressions of the look-behinds tried there (`Left`) and the
      * successes (`Right`).
      */
    def atEnd(d: Set[Int])(stop: Stop[Q]): Branch[Either[Int, MarkedStep[R]]] = stop match
      case Stop.Read(_, _)          => Branch.Fail
      case Stop.InnerEnd(ss, marks) =>
        if ss.contains(inputTerminator) then Branch.Leaf(Right(MarkedStep.End(marks))) else Branch.Fail
      case Stop.LookBehind(q, ss) =>
        if !ss.contains(inputTerminator) then Branch.Fail
        else
          val (b, ok) = tryLookBehind(d, q, inputTerminator.kind)
          val succeeded = if ok then Branch.Leaf(Right(MarkedStep.LookBehind(marksOf(q), b))) else Branch.Fail
          Branch.choice(Vector(Branch.Leaf(Left(b)), succeeded))
      case Stop.End(q) => Branch.Leaf(Right(MarkedStep.End(marksOf(q))))

    /** Returns the initial state of the NFA of the inner expression that the state tries, if the state is an attempt of
      * a look-behind assertion, which `step` makes.
      */
    def attemptInit(r: R): Option[Int] = r match
      case (s, Branch.Leaf(Stop.LookBehind(q, ss)), _) if ss.isEmpty => Some(tryLookBehind(Set.empty, q, s.kind)._1)
      case _                                                         => None

    /** Returns the characters on which the leaf may have a transition. */
    def charsOf(stop: Stop[Q]): Set[CharSet] = stop match
      case Stop.Read(q, _) => consumeMap(q).set.map(_._1)
      // An end of an inner expression and a look-behind succeed without consuming a character.
      case Stop.InnerEnd(_, _) | Stop.LookBehind(_, _) => charKinds.keySet
      case Stop.End(_)                                 => Set.empty

    /** Returns the transition of the leaf on the character at the state `d` of the DFA of `lookBehind`, as a branch of
      * the steps.
      */
    def step(s0: CharKindState, d: Set[Int], ch: CharSet)(stop: Stop[Q]): Branch[Next] =
      val d1 = lookBehind.fold(d)(_.next(d, ch))
      stop match
        case Stop.Read(q, ss) =>
          val Consume(chs, q1, loc) = consumeMap(q)
          Branch.choice:
            for (c, k) <- chs.toSeq if c == ch; s <- advance(s0, k) if ss.contains(s)
            yield
              val s1 = s.forgetMarks
              Branch.Leaf((MarkedStep.Next(marksOf(q), (s1, closure(s1, q1), d1)), loc))
        case Stop.InnerEnd(ss, marks) => successBefore(s0, ss, charKinds(ch), d1, marks)
        case Stop.LookBehind(q, ss)   =>
          val k = charKinds(ch)
          val next = advance(s0, k)
          val allowed = next.filter(ss.contains)
          if allowed.isEmpty then Branch.Fail
          else
            val (b, ok) = tryLookBehind(d, q, k)
            // An attempt allows no next state, so it has no transitions.
            val attempt = (CharKindState(k), Branch.Leaf(Stop.LookBehind(q, Set.empty)), d)
            val attempted = Branch.Leaf((MarkedStep.Next(Seq.empty, attempt), None))
            val tried =
              if isSimplified then attempted
              else
                val succeeded = if ok then Branch.Leaf((MarkedStep.LookBehind(marksOf(q), b), None)) else Branch.Fail
                Branch.choice(Vector(attempted, succeeded))
            // The inner expression is tried only when the rest of the input confirms a guess that the assertions allow.
            // A look-ahead into the pending success of the guess tells it.
            if isSimplified || allowed.size == next.size then tried
            else Branch.choice(allowed.map(s => Branch.lookAhead(false, pending(s, d1, Seq.empty), tried)))
        case Stop.End(_) => Branch.Fail

    /** Returns the next state pending the guess `s`, which is an end of an inner expression that the rest of the input
      * confirms, where `marks` are the marks of the success.
      */
    private def pending(s: CharKindState, d1: Set[Int], marks: Seq[CaptureMark]): Branch[Next] =
      val s1 = s.forgetMarks
      val r1 = (s1, Branch.Leaf(Stop.InnerEnd(nextStates(s1), Seq.empty)), d1)
      Branch.Leaf((MarkedStep.Next(marks, r1), None))

    /** Returns a success before a character of the kind, where `ss` are the next states that the assertions allow, and
      * `marks` are the marks of the success.
      *
      * It is pending if the states of the character guess differently and the assertions allow only some of them.
      */
    private def successBefore(
        s0: CharKindState,
        ss: Set[CharKindState],
        k: CharKind,
        d1: Set[Int],
        marks: Seq[CaptureMark]
    ): Branch[Next] =
      val next = advance(s0, k)
      val allowed = next.filter(ss.contains)
      if allowed.isEmpty then Branch.Fail
      else if allowed.size == next.size then Branch.Leaf((MarkedStep.End(marks), None))
      else Branch.choice(allowed.map(pending(_, d1, marks)))

    /** Returns the initial state of the NFA of the inner expression of the look-behind at the state `q`, when the next
      * character kind is `k`, and whether it succeeds at the state `d` of the DFA.
      *
      * Only the pattern has look-behind assertions, since `EpsNFABuilder` turns the nested ones into look-ahead ones.
      */
    private def tryLookBehind(d: Set[Int], q: Q, k: CharKind): (Int, Boolean) =
      val init = lookBehindInits(q)
      (lookBehind.get.inits((init, k)), lookBehind.get.succeeds(d, init, k))

    /** Returns the states after reading a character of the kind.
      *
      * The inner expression of a look-behind reads the input backward, where a state has only the character's kind. It
      * is enough, because the assertions in it look only at the kinds (see [[AssertKind#looksOnlyAtKinds]]).
      */
    private def advance(s: CharKindState, k: CharKind): Seq[CharKindState] =
      if !isBackward then s.advance(k, afterCRLF, last, follow)
      // The input has a high surrogate followed by a low surrogate as one character instead.
      else if k.high && s.kind.low then Seq.empty
      else Seq(CharKindState(k))

    private val nextStatesCache = mutable.Map.empty[CharKindState, Set[CharKindState]]

    /** Returns the states that can follow the state. */
    private def nextStates(s: CharKindState): Set[CharKindState] =
      nextStatesCache.getOrElseUpdate(s, (baseKinds.flatMap(advance(s, _)) + inputTerminator).filter(s.admits))

    /** Tests the assertion at a position, where `s0` is the state of the character read and `s1` is the next one.
      *
      * The inner expression of a look-behind reads the character before the position next.
      */
    private def accepts(k: AssertKind, s0: CharKindState, s1: CharKindState): Boolean =
      if isBackward then k.accepts(s1, s0) else k.accepts(s0, s1)

    /** Skips ε-transitions from the state and collects not ε-transition or terminal state. */
    private def buildClosure(s0: CharKindState, ss: Set[CharKindState], q: Q, loops: Set[Int]): Branch[Stop[Q]] =
      ctx.interrupt:
        if ss.isEmpty then Branch.Fail
        else
          transitions.get(q) match
            case Some(Eps(qs))             => Branch.choice(qs.map(buildClosure(s0, ss, _, loops)))
            case Some(Assert(k, q1))       => buildClosure(s0, ss.filter(accepts(k, s0, _)), q1, loops)
            case Some(LoopEnter(loop, q1)) =>
              if loops.contains(loop) then Branch.Fail // An ε-loop is detected.
              else buildClosure(s0, ss, q1, loops ++ Set(loop))
            case Some(LoopExit(loop, q1)) =>
              if loops.contains(loop) then Branch.Fail // An ε-loop is detected.
              else buildClosure(s0, ss, q1, loops)
            case Some(MarkCapture(_, q1))            => buildClosure(s0, ss, q1, loops)
            case Some(IterationEnd(loop, q1, empty)) =>
              // The iteration is empty if it entered the loop without consuming a character.
              if !loops.contains(loop) then buildClosure(s0, ss, q1, loops)
              else empty.fold(Branch.Fail)(buildClosure(s0, ss, _, loops))
            case Some(LookAheadAssert(_, init, q1)) if isSimplified =>
              Branch.choice(Seq(buildClosure(s0, ss, init, loops), buildClosure(s0, ss, q1, loops)))
            case Some(LookBehindAssert(_, _, q1)) if isSimplified =>
              Branch.choice(Seq(Branch.Leaf(Stop.LookBehind(q, ss)), buildClosure(s0, ss, q1, loops)))
            case Some(LookAroundAccept()) if isSimplified            => Branch.Leaf(Stop.End(q))
            case Some(AtomicEps(alternatives, last)) if isSimplified =>
              Branch.choice((alternatives.map(_._2) :+ last).map(buildClosure(s0, ss, _, loops)))
            case Some(LookAheadAssert(negative, init, q1)) =>
              Branch.lookAhead(negative, buildClosure(s0, ss, init, loops), buildClosure(s0, ss, q1, loops))
            case Some(LookBehindAssert(negative, _, q1)) =>
              Branch.lookAhead(negative, Branch.Leaf(Stop.LookBehind(q, ss)), buildClosure(s0, ss, q1, loops))
            case Some(LookAroundAccept())            => Branch.Leaf(Stop.InnerEnd(ss, marksOf(q)))
            case Some(AtomicEps(alternatives, last)) =>
              Branch.atomicChoice(
                alternatives.map((t, q1) => (buildClosure(s0, ss, t, loops), buildClosure(s0, ss, q1, loops))),
                buildClosure(s0, ss, last, loops)
              )
            case Some(Consume(chs, _, _)) =>
              if chs.exists((_, k) => advance(s0, k).exists(ss.contains)) then Branch.Leaf(Stop.Read(q, ss))
              else Branch.Fail
            case None =>
              if ss.contains(inputTerminator) then Branch.Leaf(Stop.End(q))
              else Branch.Fail

    private val closureCache = mutable.Map.empty[(CharKindState, Q), Branch[Stop[Q]]]

    /** Returns the ε-closure of the state after the character of the state `s0`. */
    def closure(s0: CharKindState, q: Q): Branch[Stop[Q]] =
      closureCache.getOrElseUpdate((s0, q), buildClosure(s0, nextStates(s0), q, Set.empty))

  /** Stop is a leaf of an ε-closure, where the matching reads a character, ends an inner expression, tries a
    * look-behind, or ends.
    */
  enum Stop[+Q]:

    /** Read is a state reading a character, with the next states that the assertions on the way allow. */
    case Read(q: Q, allowed: Set[CharKindState])

    /** InnerEnd is an end of the inner expression of a look-around, with the next states that the assertions allow and
      * the marks of the success.
      */
    case InnerEnd(allowed: Set[CharKindState], marks: Seq[CaptureMark])

    /** LookBehind is a state trying a look-behind assertion, with the next states that the assertions allow. */
    case LookBehind(q: Q, allowed: Set[CharKindState])

    /** End is an accepting state, which has no transitions. */
    case End(q: Q)

  /** AssertKind is assertion kind of this ε-NFA transition. */
  sealed abstract class AssertKind extends Serializable with Product:

    /** Tests the assertion at a position between the states before and after the character there.
      *
      * The state after is that of the end of the input at the end.
      */
    def accepts(prev: CharKindState, next: CharKindState): Boolean

    /** Whether the assertion distinguishes the begin and the end of the input from the characters. */
    def seesBoundary: Boolean = true

    /** Whether the assertion looks only at the kinds of the characters around the position, which the inner expression
      * of a look-behind reading the input backward knows.
      */
    def looksOnlyAtKinds: Boolean = true

    /** Whether the assertion tests the begin and the end of the input as a line terminator among the kinds. */
    def treatsBoundaryAsLineTerminator(kinds: Iterable[CharKind]): Boolean =
      val (b, t) = (CharKindState(CharKind.Boundary), CharKindState(CharKind.LineTerminator))
      (kinds.map(CharKindState(_)) ++ Seq(b, t)).forall: s =>
        accepts(b, s) == accepts(t, s) && accepts(s, b) == accepts(s, t)

  /** AssertKind values. */
  object AssertKind:

    private def betweenCRLF(prev: CharKindState, next: CharKindState): Boolean = prev.kind.cr && next.kind.lf

    /** LineBegin is `^` assertion with the `set`-th line terminator set, which `atEnd` tells where it matches at the
      * end of the input.
      *
      * With `afterCRLF`, it matches after `\r\n` too, where `\n` is not a line terminator by itself (PCRE's `(*CRLF)`).
      * With `atomic`, it does not match between `\r` and `\n` (Java).
      */
    final case class LineBegin(set: Int, atEnd: IRLineBeginAtEnd, afterCRLF: Boolean, atomic: Boolean)
        extends AssertKind:

      override def looksOnlyAtKinds: Boolean = !afterCRLF

      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        val afterTerminator = prev.kind.isLineTerminatorOf(set) || (afterCRLF && prev.afterCR)
        (prev.kind.boundary || afterTerminator) && !(atomic && betweenCRLF(prev, next)) &&
        (!next.kind.boundary || (atEnd match
          case IRLineBeginAtEnd.AfterTerminator => true
          case IRLineBeginAtEnd.InputBegin      => prev.kind.boundary
          case IRLineBeginAtEnd.Never           => false))

    /** LineEnd is `$` assertion with the `set`-th line terminator set.
      *
      * With `beforeCRLF`, it matches before `\r\n` too, where `\r` is not a line terminator by itself (PCRE's
      * `(*CRLF)`). With `atomic`, it does not match between `\r` and `\n` (Java).
      */
    final case class LineEnd(set: Int, beforeCRLF: Boolean, atomic: Boolean) extends AssertKind:

      override def looksOnlyAtKinds: Boolean = !beforeCRLF

      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        val beforeTerminator =
          next.kind.isLineTerminatorOf(set) || (beforeCRLF && next.kind.cr && !next.follow.contains(CRFollow.Other))
        (next.kind.boundary || beforeTerminator) && !(atomic && betweenCRLF(prev, next))

    /** TextBegin is `\A` assertion. */
    case object TextBegin extends AssertKind:

      def accepts(prev: CharKindState, next: CharKindState): Boolean = prev.kind.boundary

    /** TextEnd is `\z` assertion. */
    case object TextEnd extends AssertKind:

      def accepts(prev: CharKindState, next: CharKindState): Boolean = next.kind.boundary

    /** ChompTextEnd is `\Z` assertion with the `set`-th line terminator set, which matches at the end or before a line
      * terminator at the end.
      *
      * With `crlf`, it matches before `\r\n` at the end too. With `atomic`, it does not match between `\r` and `\n`
      * (Java).
      */
    final case class ChompTextEnd(set: Int, crlf: Boolean, atomic: Boolean) extends AssertKind:

      override def looksOnlyAtKinds: Boolean = false

      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        val beforeFinal = (next.kind.isLineTerminatorOf(set) && next.last.contains(true)) ||
          (crlf && next.kind.cr && next.follow.contains(CRFollow.FinalLF))
        (next.kind.boundary || beforeFinal) && !(atomic && betweenCRLF(prev, next))

    /** WordBoundary is `\b` assertion of the `set`-th word set. */
    final case class WordBoundary(set: Int) extends AssertKind:

      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        prev.kind.isWordOf(set) != next.kind.isWordOf(set)

      override def seesBoundary: Boolean = false

    /** NotWordBoundary is `\B` assertion of the `set`-th word set. */
    final case class NotWordBoundary(set: Int) extends AssertKind:

      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        prev.kind.isWordOf(set) == next.kind.isWordOf(set)

      override def seesBoundary: Boolean = false

    /** Tests whether the character before a position is a word character of the `word`-th word set, or an extender of
      * the `marks`-th word marks after a base.
      */
    private def isMarkWordBefore(prev: CharKindState, word: Int, marks: Int): Boolean =
      prev.kind.isWordOf(word) || prev.kind.isExtenderOf(marks) && prev.isBasedOf(marks)

    /** Tests whether the character after a position is a word character of the `word`-th word set, or a mark of the
      * `marks`-th word marks after a base.
      */
    private def isMarkWordAfter(prev: CharKindState, next: CharKindState, word: Int, marks: Int): Boolean =
      next.kind.isWordOf(word) || next.kind.isMarkOf(marks) && prev.isBasedOf(marks)

    /** MarkWordBoundary is `\b` assertion of the `word`-th word set and the `marks`-th word marks. */
    final case class MarkWordBoundary(word: Int, marks: Int) extends AssertKind:

      override def looksOnlyAtKinds: Boolean = false

      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        isMarkWordBefore(prev, word, marks) != isMarkWordAfter(prev, next, word, marks)

    /** NotMarkWordBoundary is `\B` assertion of the `word`-th word set and the `marks`-th word marks. */
    final case class NotMarkWordBoundary(word: Int, marks: Int) extends AssertKind:

      override def looksOnlyAtKinds: Boolean = false

      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        isMarkWordBefore(prev, word, marks) == isMarkWordAfter(prev, next, word, marks)

    /** NotBetweenCRLF is an assertion that the position is not between `\r` and `\n`. */
    case object NotBetweenCRLF extends AssertKind:

      def accepts(prev: CharKindState, next: CharKindState): Boolean = !betweenCRLF(prev, next)

      override def seesBoundary: Boolean = false
