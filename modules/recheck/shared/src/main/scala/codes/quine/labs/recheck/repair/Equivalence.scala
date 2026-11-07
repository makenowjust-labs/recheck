package codes.quine.labs.recheck
package repair

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

import codes.quine.labs.recheck.automaton.Branch
import codes.quine.labs.recheck.automaton.CaptureMark
import codes.quine.labs.recheck.automaton.DFA
import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.automaton.MarkedStep
import codes.quine.labs.recheck.automaton.OrderedNFA
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** Equivalence compares what two patterns extract from an input. */
object Equivalence:

  /** Finds the shortest input on which the patterns differ, if any.
    *
    * They differ when one of them matches and the other does not, or when a pair of `captures` differs in the match,
    * where a pair has the capture indices of `a` and `b`, and `(0, 0)` is the whole match. A capture is the last one
    * set on the path of the match, or unset.
    */
  def findCounterexample(a: IRPattern, b: IRPattern, captures: Seq[(Int, Int)], maxNFASize: Int, maxDFASize: Int)(using
      ctx: Context
  ): Option[UString] =
    ctx.interrupt:
      val sideA = ComparedPattern(a, captures.map(_._1), maxNFASize, maxDFASize)
      val sideB = ComparedPattern(b, captures.map(_._2), maxNFASize, maxDFASize)
      new Equivalence(sideA, sideB, maxNFASize).findCounterexample()

  /** The value of a begin or an end of a capture that is unset. */
  private[repair] val Unset = -1

  /** The value of a begin or an end of a capture that a path does not set. */
  private[repair] val NotSet = -2

  /** ComparedPattern is a pattern compared, as an ordered NFA marking the captures and its look-ahead DFA.
    *
    * `marking` and `lookBehindMarking` are the states of the NFA and of [[OrderedNFA.LookBehind.inner]] from which a
    * path may set a capture.
    */
  private[repair] final case class ComparedPattern(
      alphabet: PatternAlphabet,
      nfa: OrderedNFA[CharSet, Int],
      lookAhead: DFA[CharSet, Set[Int]],
      captures: Seq[Int],
      marking: Set[Int],
      lookBehindMarking: Set[Int]
  ):

    /** The begins of the captures by the indices, as the indices of the values (see [[SideCaptures]]). */
    val beginsOf: Map[Int, Seq[Int]] = captures.zipWithIndex.groupMap(_._1)(2 * _._2)

    /** The ends of the captures by the indices, as the indices of the values. */
    val endsOf: Map[Int, Seq[Int]] = captures.zipWithIndex.groupMap(_._1)(2 * _._2 + 1)

    /** Returns the values that the marks set at the position of the label, from the ones before them. */
    def setMarks(values: Vector[Int], marks: Seq[CaptureMark], label: Int): Vector[Int] =
      marks.foldLeft(values):
        case (vs, CaptureMark.Begin(i)) => beginsOf(i).foldLeft(vs)(_.updated(_, label))
        case (vs, CaptureMark.End(i))   => endsOf(i).foldLeft(vs)(_.updated(_, label))
        case (vs, CaptureMark.Unset(i)) => (beginsOf(i) ++ endsOf(i)).foldLeft(vs)(_.updated(_, Unset))

  private[repair] object ComparedPattern:

    def apply(pattern: IRPattern, captures: Seq[Int], maxNFASize: Int, maxDFASize: Int)(using
        ctx: Context
    ): ComparedPattern =
      val epsNFA =
        EpsNFABuilder.build(pattern, captures.toSet, maxNFASize, Parameters.DefaultMaxBackReferenceStrings)
      val nfa = epsNFA.foldCaptureMarks.toOrderedNFA(false, maxNFASize, maxDFASize, Some(_._2)).rename
      val lookBehindMarking = nfa.lookBehind.fold(Set.empty)(lb => markingStates(lb.inner, Set.empty))
      val marking = markingStates(nfa, lookBehindMarking)
      ComparedPattern(epsNFA.alphabet, nfa, nfa.toLookAheadDFA(maxDFASize), captures, marking, lookBehindMarking)

    /** Returns the states from which a path may set a capture, where `setsByLookBehind` tells whether the inner
      * expression of a look-behind from the initial state may set one.
      */
    private def markingStates(nfa: OrderedNFA[CharSet, Int], setsByLookBehind: Int => Boolean): Set[Int] =
      val branches = nfa.markedDelta.toSeq.map((key, b) => key._1 -> b) ++ nfa.markedDeltaAtEnd.toSeq
      val sources = mutable.Map.empty[Int, Seq[Int]].withDefaultValue(Vector.empty)
      val marking = mutable.Set.empty[Int]
      for (q, b) <- branches; step <- b.allLeaves do
        if step.marks.nonEmpty then marking.add(q)
        step match
          case MarkedStep.Next(_, q1)                                     => sources(q1) :+= q
          case MarkedStep.LookBehind(_, inner) if setsByLookBehind(inner) => marking.add(q)
          case _                                                          => ()
      val queue = mutable.Queue.from(marking)
      while queue.nonEmpty do for q <- sources(queue.dequeue()) if marking.add(q) do queue.enqueue(q)
      marking.toSet

  /** SharedAtom is a set of characters that both patterns read alike, with their atoms containing it. */
  private[repair] final case class SharedAtom(set: CharSet, a: CharSet, b: CharSet, high: Boolean, low: Boolean)

  /** RestClass is the class of the rest of an input that the look-ahead DFAs tell, and whether it starts with a low
    * surrogate.
    */
  private[repair] final case class RestClass(a: Set[Int], b: Set[Int], startsWithLow: Boolean)

  /** SideState is where a pattern is: the state of the match, and the states of the inner expressions of the positive
    * look-aheads that still read the input, from the earliest tried.
    */
  private[repair] final case class SideState(state: Int, lookAheads: Vector[Int])

  /** SideCaptures is what a pattern sets on the begins and the ends of the compared captures.
    *
    * A value is [[Unset]] or the label of a position, which is common to the patterns. A positive look-ahead sets its
    * captures when it is tried, but the search sees them at their positions later. So `laterFrom` tells the paths that
    * set each value later than it was set: the paths in [[SideState]], and then the match, from that index.
    *
    * `lookBehinds` are the values that the inner expression of a look-behind tried here sets, as [[NotSet]] unless it
    * does, by the initial states of [[OrderedNFA.LookBehind.inner]].
    */
  private[repair] final case class SideCaptures(
      values: Vector[Int],
      laterFrom: Vector[Int],
      lookBehinds: Map[Int, Vector[Int]]
  )

  /** CaptureState is what the patterns set on the compared captures, and the label of the position if a value has it.
    */
  private[repair] final case class CaptureState(a: SideCaptures, b: SideCaptures, position: Option[Int]):

    /** The number of the labels, which are from zero. */
    def labelCount: Int =
      val all = Iterator(a, b).flatMap(side => side.values.iterator ++ side.lookBehinds.valuesIterator.flatten)
      all.filter(_ >= 0).maxOption.fold(0)(_ + 1)

    /** Renames the labels in the order of their appearance, so equal states are equal. */
    def canonical: CaptureState =
      val renamed = mutable.Map.empty[Int, Int]
      def rename(v: Int): Int = if v < 0 then v else renamed.getOrElseUpdate(v, renamed.size)
      def renameLookBehinds(lookBehinds: Map[Int, Vector[Int]]): Map[Int, Vector[Int]] =
        lookBehinds.toSeq.sortBy(_._1).map((q, vs) => q -> vs.map(rename)).toMap
      val (valuesA, valuesB) = (a.values.map(rename), b.values.map(rename))
      val (lookBehindsA, lookBehindsB) = (renameLookBehinds(a.lookBehinds), renameLookBehinds(b.lookBehinds))
      CaptureState(
        SideCaptures(valuesA, a.laterFrom, lookBehindsA),
        SideCaptures(valuesB, b.laterFrom, lookBehindsB),
        position.flatMap(renamed.get)
      )

  /** SearchNode is a position on the paths of the matches, with the class of the rest of the input, whether the prefix
    * ends with a high surrogate, and the captures.
    */
  private[repair] final case class SearchNode(
      a: SideState,
      b: SideState,
      rest: RestClass,
      endsWithHigh: Boolean,
      captures: CaptureState
  )

  /** Path is the path of a branch that a backtracking matcher succeeds by: the paths of the inner expressions of the
    * positive look-aheads on the way, and the step at its end.
    */
  private[repair] final case class Path(inners: Seq[Path], step: MarkedStep[Int])

  /** Finds the path of the branch, when exactly the states satisfying `p` succeed. */
  private[repair] def findPath(b: Branch[MarkedStep[Int]], p: Int => Boolean): Option[Path] = b match
    case Branch.Leaf(step)                       => Option.when(step.succeeds(p))(Path(Vector.empty, step))
    case Branch.Choice(bs)                       => bs.iterator.flatMap(findPath(_, p)).nextOption()
    case Branch.LookAhead(negative, inner, cont) =>
      if negative then (if inner.succeeds(_.succeeds(p)) then None else findPath(cont, p))
      else for i <- findPath(inner, p); c <- findPath(cont, p) yield c.copy(inners = i +: c.inners)
    case Branch.AtomicChoice(alternatives, last) =>
      findPath(alternatives.find(_._1.succeeds(_.succeeds(p))).fold(last)(_._2), p)
    // The marked transitions have leaves for the successes.
    // $COVERAGE-OFF$
    case Branch.Succeed => throw new IllegalStateException("Succeed in a marked transition")
    // $COVERAGE-ON$

private final class Equivalence(
    sideA: Equivalence.ComparedPattern,
    sideB: Equivalence.ComparedPattern,
    maxNFASize: Int
)(using
    ctx: Context
):

  import Equivalence.*

  /** The atoms on which both patterns read the characters alike. */
  private val atoms: Seq[SharedAtom] =
    for
      (a, kindA) <- sideA.alphabet.pairs
      (b, kindB) <- sideB.alphabet.pairs
      set = a.intersect(b)
      if set.nonEmpty
    yield SharedAtom(set, a, b, kindA.high || kindB.high, kindA.low || kindB.low)

  /** The class of the empty rest. */
  private val end = RestClass(sideA.lookAhead.init, sideB.lookAhead.init, false)

  /** The length of the shortest rest of each class, and its first atom and the class after it. */
  private val shortest = mutable.Map[RestClass, (Int, Option[(SharedAtom, RestClass)])](end -> (0, None))

  /** The first atoms of the rests of each class, each with the class after it. */
  private val splits = mutable.Map.empty[RestClass, Vector[(SharedAtom, RestClass)]].withDefaultValue(Vector.empty)

  locally:
    val queue = mutable.Queue(end)
    while queue.nonEmpty do
      ctx.interrupt:
        val rest = queue.dequeue()
        val (n, _) = shortest(rest)
        // A high surrogate followed by a low one is a character by itself.
        for atom <- atoms if !(atom.high && rest.startsWithLow) do
          val rest1 =
            RestClass(sideA.lookAhead.delta((rest.a, atom.a)), sideB.lookAhead.delta((rest.b, atom.b)), atom.low)
          splits(rest1) = splits(rest1) :+ (atom, rest)
          if !shortest.contains(rest1) then
            shortest(rest1) = (n + 1, Some((atom, rest)))
            queue.enqueue(rest1)

  /** Returns the shortest rest of the class. */
  private def restOf(rest: RestClass): UString =
    UString.from(Iterator.unfold(rest)(r => shortest(r)._2.map((atom, r1) => (atom.set.representative, r1))))

  /** Writers runs the paths of a pattern at a position, which set the captures: the inner expressions of the positive
    * look-aheads tried before, and the match. They are in the order of the times when they set the captures, where a
    * path tried by another comes just before it.
    *
    * `here` is the label of the position.
    */
  private final class Writers(side: ComparedPattern, at: SideState, captures: SideCaptures, here: Int):

    private final class Writer(var state: Int)

    private val writers = mutable.ArrayBuffer.from(at.lookAheads.map(Writer(_)) :+ Writer(at.state))

    private val values = captures.values.toArray

    private val laterFrom = captures.laterFrom.toArray

    /** Runs the transitions on the character set, where exactly the states satisfying `p` succeed on the rest. */
    def advance(c: CharSet, p: Int => Boolean): (SideState, Vector[Int], Vector[Int]) =
      val main = writers.last
      for w <- writers.reverseIterator.toList do follow(w, side.nfa.markedDelta.getOrElse((w.state, c), Branch.Fail), p)
      // A look-ahead in the state of a later one sets the same values at the same positions before it, and one in a
      // state not marking sets none.
      val seen = mutable.Set.empty[Int]
      for w <- writers.init.reverseIterator.toList if !side.marking.contains(w.state) || !seen.add(w.state) do remove(w)
      (SideState(main.state, writers.init.map(_.state).toVector), values.toVector, laterFrom.toVector)

    /** Runs the paths at the end of the input, and returns the values. */
    def finish(): Vector[Int] =
      for w <- writers.reverseIterator.toList do
        follow(w, side.nfa.markedDeltaAtEnd.getOrElse(w.state, Branch.Fail), Set.empty)
      values.toVector

    private def follow(w: Writer, b: Branch[MarkedStep[Int]], p: Int => Boolean): Unit =
      follow(w, findSuccessPath(b, p))

    private def follow(w: Writer, path: Path): Unit =
      for inner <- path.inners do
        val tried = Writer(-1)
        insert(writers.indexOf(w), tried)
        follow(tried, inner)
      setAll(w, side.setMarks(Vector.fill(values.length)(NotSet), path.step.marks, here))
      path.step match
        case MarkedStep.Next(_, q)           => w.state = q
        case MarkedStep.End(_)               => remove(w)
        case MarkedStep.LookBehind(_, inner) =>
          captures.lookBehinds.get(inner).foreach(setAll(w, _))
          remove(w)

    /** Sets the values other than [[NotSet]] by the path, if it sets them later than they were set. */
    private def setAll(w: Writer, vs: Vector[Int]): Unit =
      val k = writers.indexOf(w)
      for (v, s) <- vs.zipWithIndex if v != NotSet && k >= laterFrom(s) do
        values(s) = v
        laterFrom(s) = k

    private def insert(k: Int, w: Writer): Unit =
      writers.insert(k, w)
      for s <- laterFrom.indices if k < laterFrom(s) do laterFrom(s) += 1

    private def remove(w: Writer): Unit =
      val k = writers.indexOf(w)
      writers.remove(k)
      for s <- laterFrom.indices if k < laterFrom(s) do laterFrom(s) -= 1

  /** Returns the values that the inner expressions of look-behinds tried after the character set `c` set, from the ones
    * before it.
    *
    * `before` and `after` are the states of the pattern before and after it, and `label` is the label of the position
    * after it.
    */
  private def advanceLookBehinds(
      side: ComparedPattern,
      lookBehinds: Map[Int, Vector[Int]],
      before: Int,
      after: Int,
      c: CharSet,
      label: Int
  ): Map[Int, Vector[Int]] =
    side.nfa.lookBehind.fold(Map.empty): lb =>
      val succeeded = lb.successSets(before)
      lb.successSets(after)
        .intersect(side.lookBehindMarking)
        .iterator
        .flatMap: q =>
          val path = findSuccessPath(lb.inner.markedDelta.getOrElse((q, c), Branch.Fail), succeeded.contains)
          val stepValues = lookBehindValues(side, path, label)
          // The inner expression reads the input backward, so it sets the values before these later.
          val earlier = path.step match
            case MarkedStep.Next(_, q0) => lookBehinds.get(q0)
            case _                      => None
          val values = earlier.fold(stepValues)(_.zip(stepValues).map((v0, v) => if v0 != NotSet then v0 else v))
          Option.when(values.exists(_ != NotSet))(q -> values)
        .toMap

  /** Returns the values that the inner expressions of look-behinds tried at the begin of the input set. */
  private def initialLookBehinds(side: ComparedPattern, init: Int, label: Int): Map[Int, Vector[Int]] =
    side.nfa.lookBehind.fold(Map.empty): lb =>
      lb.successSets(init)
        .intersect(side.lookBehindMarking)
        .iterator
        .flatMap: q =>
          val path = findSuccessPath(lb.inner.markedDeltaAtEnd.getOrElse(q, Branch.Fail), Set.empty)
          val values = lookBehindValues(side, path, label)
          Option.when(values.exists(_ != NotSet))(q -> values)
        .toMap

  /** Returns the values that a step of the inner expression of a look-behind sets at the position of the label.
    *
    * It reads the input backward, so the nested look-behinds on the way, which set no captures, take the marks before
    * them.
    */
  private def lookBehindValues(side: ComparedPattern, path: Path, label: Int): Vector[Int] =
    def marksOf(path: Path): Seq[CaptureMark] = path.inners.flatMap(marksOf) ++ path.step.marks
    side.setMarks(Vector.fill(2 * side.captures.size)(NotSet), marksOf(path), label)

  /** Finds the path of a branch from a state that succeeds. */
  private def findSuccessPath(b: Branch[MarkedStep[Int]], p: Int => Boolean): Path =
    findPath(b, p) match
      case Some(path) => path
      // The look-ahead DFA tells a success from the state only when the branch of the transition succeeds.
      // $COVERAGE-OFF$
      case None => throw new IllegalStateException("a success without the path")
      // $COVERAGE-ON$

  /** Returns the node after the atom, where the rest after it is of the class `rest1`. */
  private def next(node: SearchNode, atom: SharedAtom, rest1: RestClass): SearchNode =
    val captures = node.captures
    val here = captures.position.getOrElse(captures.labelCount)
    val label = captures.labelCount + 1
    def advance(side: ComparedPattern, at: SideState, sideCaptures: SideCaptures, c: CharSet, p: Set[Int]) =
      val (at1, values, laterFrom) = Writers(side, at, sideCaptures, here).advance(c, p.contains)
      val lookBehinds = advanceLookBehinds(side, sideCaptures.lookBehinds, at.state, at1.state, c, label)
      (at1, SideCaptures(values, laterFrom, lookBehinds))
    val (a, capturesA) = advance(sideA, node.a, captures.a, atom.a, rest1.a)
    val (b, capturesB) = advance(sideB, node.b, captures.b, atom.b, rest1.b)
    SearchNode(a, b, rest1, atom.high, CaptureState(capturesA, capturesB, Some(label)).canonical)

  /** Tests whether the captures differ at the end of the input. */
  private def differsAtEnd(node: SearchNode): Boolean =
    val here = node.captures.position.getOrElse(node.captures.labelCount)
    Writers(sideA, node.a, node.captures.a, here).finish() != Writers(sideB, node.b, node.captures.b, here).finish()

  /** Finds the shortest input on which the patterns differ, by A* search over the nodes whose estimate is the shortest
    * rest of the class.
    */
  def findCounterexample(): Option[UString] =
    // `EpsNFA#toOrderedNFA` builds the ordered NFA of a pattern from one initial state.
    // $COVERAGE-OFF$
    val initA = sideA.nfa.inits match
      case Seq(q) => q
      case qs     => throw new IllegalStateException(s"ordered NFA with ${qs.size} initial states")
    val initB = sideB.nfa.inits match
      case Seq(q) => q
      case qs     => throw new IllegalStateException(s"ordered NFA with ${qs.size} initial states")
    // $COVERAGE-ON$
    def initialSide(side: ComparedPattern, init: Int): SideCaptures =
      val size = 2 * side.captures.size
      SideCaptures(Vector.fill(size)(Unset), Vector.fill(size)(0), initialLookBehinds(side, init, 0))
    val initial = CaptureState(initialSide(sideA, initA), initialSide(sideB, initB), Some(0)).canonical

    val distance = mutable.Map.empty[SearchNode, Int]
    val parent = mutable.Map.empty[SearchNode, (SearchNode, SharedAtom)]
    var order = 0L
    val queue = mutable.PriorityQueue.empty[(Int, Long, SearchNode)](using Ordering.by((f, o, _) => (-f, -o)))

    def push(node: SearchNode, g: Int, from: Option[(SearchNode, SharedAtom)]): Unit =
      if distance.get(node).forall(g < _) then
        if distance.size >= maxNFASize then throw new UnsupportedException("equivalence search is too large")
        distance(node) = g
        from.foreach(parent(node) = _)
        queue.enqueue((g + shortest(node.rest)._1, order, node))
        order += 1

    def prefixOf(node: SearchNode): UString =
      UString.from(
        Iterator.unfold(node)(n => parent.get(n).map((n1, atom) => (atom.set.representative, n1))).toSeq.reverse
      )

    for rest <- shortest.keys do
      push(SearchNode(SideState(initA, Vector.empty), SideState(initB, Vector.empty), rest, false, initial), 0, None)

    boundary:
      while queue.nonEmpty do
        ctx.interrupt:
          val (f, _, node) = queue.dequeue()
          val SearchNode(a, b, rest, endsWithHigh, _) = node
          val g = distance(node)
          // The node may be pushed again by a shorter prefix.
          if f == g + shortest(rest)._1 then
            val (succeedsA, succeedsB) = (rest.a.contains(a.state), rest.b.contains(b.state))
            if succeedsA != succeedsB then break(Some(prefixOf(node) ++ restOf(rest)))
            if succeedsA then
              if rest == end && differsAtEnd(node) then break(Some(prefixOf(node)))
              for (atom, rest1) <- splits(rest) if !(endsWithHigh && atom.low) do
                push(next(node, atom, rest1), g + 1, Some((node, atom)))
      None
