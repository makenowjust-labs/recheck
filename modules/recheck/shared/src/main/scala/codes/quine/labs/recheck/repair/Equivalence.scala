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
    * where a pair has the capture indices of `a` and `b`, and `(0, 0)` is the whole match. The match is the first one
    * in the order of the backtracking, or the first of the longest ones for a longest-match pattern, as the interpreter
    * finds. A capture is the last one set on the path of the match, or unset. The back-references are expanded as
    * [[EpsNFABuilder.build]] does with `maxBackReferenceStrings`.
    */
  def findCounterexample(
      a: IRPattern,
      b: IRPattern,
      captures: Seq[(Int, Int)],
      maxNFASize: Int,
      maxDFASize: Int,
      maxBackReferenceStrings: Int = Parameters.DefaultMaxBackReferenceStrings
  )(using ctx: Context): Option[UString] =
    ctx.interrupt:
      val sideA = ComparedPattern(a, captures.map(_._1), maxNFASize, maxDFASize, maxBackReferenceStrings)
      val sideB = ComparedPattern(b, captures.map(_._2), maxNFASize, maxDFASize, maxBackReferenceStrings)
      new Equivalence(sideA, sideB, maxNFASize).findCounterexample()

  /** The value of a begin or an end of a capture that is unset. */
  private[repair] val Unset = -1

  /** The value of a begin or an end of a capture that a path does not set. */
  private[repair] val NotSet = -2

  /** The state of a match that has ended before the end of the input. */
  private[repair] val Ended = -1

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

    /** The begins of the captures by the indices, as the indices of the values (see [[Track]]). */
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

    def apply(pattern: IRPattern, captures: Seq[Int], maxNFASize: Int, maxDFASize: Int, maxBackReferenceStrings: Int)(
        using ctx: Context
    ): ComparedPattern =
      val epsNFA = EpsNFABuilder.build(pattern, captures.toSet, maxNFASize, maxBackReferenceStrings)
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

  /** Track is a path of the match: its state, or [[Ended]], the states of the inner expressions of the positive
    * look-aheads tried on it that still read the input, from the earliest tried, and what they set on the begins and
    * the ends of the compared captures.
    *
    * A value is [[Unset]] or the label of a position, which is common to the patterns. A positive look-ahead sets its
    * captures when it is tried, but the search sees them at their positions later. So `laterFrom` tells the paths that
    * set each value later than it was set: the look-aheads, and then the match, from that index.
    */
  private[repair] final case class Track(
      state: Int,
      lookAheads: Vector[Int],
      values: Vector[Int],
      laterFrom: Vector[Int]
  )

  /** SideState is where a pattern is: the tracks of the match that still read the input in the order of the
    * backtracking, and the match that has ended, if any.
    *
    * A pattern takes the first path that succeeds, so it has one track. A longest-match pattern tries every one, so a
    * track ending later makes the match then.
    *
    * `lookBehinds` are the values that the inner expression of a look-behind tried here sets, as [[NotSet]] unless it
    * does, by the initial states of [[OrderedNFA.LookBehind.inner]].
    */
  private[repair] final case class SideState(
      tracks: Vector[Track],
      matched: Option[Track],
      lookBehinds: Map[Int, Vector[Int]]
  ):

    /** A state that still reads the input, if any. All of them have the same look-behinds succeeding. */
    def reading: Option[Int] =
      (tracks.iterator.flatMap(t => t.state +: t.lookAheads) ++ matched.iterator.flatMap(_.lookAheads)).nextOption()

    /** Tests whether the pattern matches on a rest, from whose class `succeeding` are the states that succeed. */
    def succeeds(succeeding: Set[Int]): Boolean = matched.nonEmpty || tracks.exists(t => succeeding.contains(t.state))

  /** SearchNode is a position on the paths of the matches, with the class of the rest of the input, whether the prefix
    * ends with a high surrogate, and the label of the position if a value has it.
    */
  private[repair] final case class SearchNode(
      a: SideState,
      b: SideState,
      rest: RestClass,
      endsWithHigh: Boolean,
      position: Option[Int]
  ):

    /** The number of the labels, which are from zero. */
    def labelCount: Int =
      def values(side: SideState): Iterator[Int] =
        (side.tracks.iterator ++ side.matched).flatMap(_.values) ++ side.lookBehinds.valuesIterator.flatten
      (values(a) ++ values(b)).filter(_ >= 0).maxOption.fold(0)(_ + 1)

    /** Renames the labels in the order of their appearance, so equal nodes are equal. */
    def canonical: SearchNode =
      val renamed = mutable.Map.empty[Int, Int]
      def rename(v: Int): Int = if v < 0 then v else renamed.getOrElseUpdate(v, renamed.size)
      def renameTrack(t: Track): Track = t.copy(values = t.values.map(rename))
      def renameSide(side: SideState): SideState =
        val tracks = side.tracks.map(renameTrack)
        val matched = side.matched.map(renameTrack)
        val lookBehinds = side.lookBehinds.toSeq.sortBy(_._1).map((q, vs) => q -> vs.map(rename)).toMap
        SideState(tracks, matched, lookBehinds)
      val a1 = renameSide(a)
      val b1 = renameSide(b)
      SearchNode(a1, b1, rest, endsWithHigh, position.flatMap(renamed.get))

  /** Path is the path of a branch that a backtracking matcher succeeds by: the paths of the inner expressions of the
    * positive look-aheads on the way, and the step at its end.
    */
  private[repair] final case class Path(inners: Seq[Path], step: MarkedStep[Int])

  /** Finds the paths of the branch that succeed in the order of the backtracking, when exactly the states satisfying
    * `p` succeed.
    *
    * A choice takes the first branch that succeeds, but a longest choice takes every one, as a longest match tries them
    * all.
    */
  private[repair] def findPaths(b: Branch[MarkedStep[Int]], p: Int => Boolean): Seq[Path] = b match
    case Branch.Leaf(step)        => if step.succeeds(p) then Seq(Path(Vector.empty, step)) else Seq.empty
    case Branch.Choice(bs)        => bs.iterator.map(findPaths(_, p)).find(_.nonEmpty).getOrElse(Seq.empty)
    case Branch.LongestChoice(bs) => bs.flatMap(findPaths(_, p))
    case Branch.LookAhead(negative, inner, cont) =>
      if negative then (if inner.succeeds(_.succeeds(p)) then Seq.empty else findPaths(cont, p))
      else
        findPaths(inner, p).headOption.fold(Seq.empty): i =>
          findPaths(cont, p).map(c => c.copy(inners = i +: c.inners))
    case Branch.AtomicChoice(alternatives, last) =>
      findPaths(alternatives.find(_._1.succeeds(_.succeeds(p))).fold(last)(_._2), p)
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

  /** Writers runs the paths of a track at a position, which set the captures: the inner expressions of the positive
    * look-aheads tried before, and the match. They are in the order of the times when they set the captures, where a
    * path tried by another comes just before it.
    *
    * `here` is the label of the position.
    */
  private final class Writers(side: ComparedPattern, track: Track, lookBehinds: Map[Int, Vector[Int]], here: Int):

    private final class Writer(var state: Int)

    private val writers = mutable.ArrayBuffer.from(track.lookAheads.map(Writer(_)) :+ Writer(track.state))

    private val values = track.values.toArray

    private val laterFrom = track.laterFrom.toArray

    private val main = writers.last

    /** Runs the transitions on the character set, where the match takes `path` unless it has ended, and exactly the
      * states satisfying `p` succeed on the rest.
      */
    def advance(path: Option[Path], c: CharSet, p: Int => Boolean): Track =
      val lookAheads = writers.init.toList
      path.foreach(follow(main, _))
      for w <- lookAheads.reverse do
        follow(w, findSuccessPath(side.nfa.markedDelta.getOrElse((w.state, c), Branch.Fail), p))
      // A look-ahead in the state of a later one sets the same values at the same positions before it, and one in a
      // state not marking sets none.
      val seen = mutable.Set.empty[Int]
      for w <- writers.init.reverseIterator.toList if !side.marking.contains(w.state) || !seen.add(w.state) do remove(w)
      Track(main.state, writers.init.map(_.state).toVector, values.toVector, laterFrom.toVector)

    /** Runs the paths at the end of the input, where the match takes `path` unless it has ended, and returns the
      * values.
      */
    def finish(path: Option[Path]): Vector[Int] =
      val lookAheads = writers.init.toList
      path.foreach(follow(main, _))
      for w <- lookAheads.reverse do
        follow(w, findSuccessPath(side.nfa.markedDeltaAtEnd.getOrElse(w.state, Branch.Fail), Set.empty))
      values.toVector

    private def follow(w: Writer, path: Path): Unit =
      for inner <- path.inners do
        val tried = Writer(-1)
        insert(writers.indexOf(w), tried)
        follow(tried, inner)
      setAll(w, side.setMarks(Vector.fill(values.length)(NotSet), path.step.marks, here))
      path.step match
        case MarkedStep.Next(_, q) => w.state = q
        // The look-aheads tried on the match still read the input after it ends.
        case MarkedStep.End(_)               => if w eq main then w.state = Ended else remove(w)
        case MarkedStep.LookBehind(_, inner) =>
          lookBehinds.get(inner).foreach(setAll(w, _))
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

  /** Returns where the pattern is after the character set `c`, where exactly the states in `p` succeed on the rest, and
    * `here` and `label` are the labels of the positions before and after it.
    */
  private def advance(side: ComparedPattern, at: SideState, c: CharSet, p: Set[Int], here: Int, label: Int): SideState =
    val tracks = Vector.newBuilder[Track]
    val seen = mutable.Set.empty[Int]
    var endedHere = Option.empty[Track]
    for
      track <- at.tracks
      path <- findPaths(side.nfa.markedDelta.getOrElse((track.state, c), Branch.Fail), p.contains)
    do
      val next = Writers(side, track, at.lookBehinds, here).advance(Some(path), c, p.contains)
      // The first track ending here is longer than the match before.
      if next.state == Ended then endedHere = endedHere.orElse(Some(next))
      // A later track in the same state never makes the match, since an earlier one does first on the same rest.
      else if seen.add(next.state) then tracks += next
    val matched =
      endedHere.orElse(at.matched.map(m => Writers(side, m, at.lookBehinds, here).advance(None, c, p.contains)))
    val at1 = SideState(tracks.result(), matched, Map.empty)
    at1.copy(lookBehinds = advanceLookBehinds(side, at.lookBehinds, at, at1, c, label))

  /** Returns the values that the inner expressions of look-behinds tried after the character set `c` set, from the ones
    * before it.
    *
    * `before` and `after` are where the pattern is before and after it, and `label` is the label of the position after
    * it. They are needed only while a path reads the input.
    */
  private def advanceLookBehinds(
      side: ComparedPattern,
      lookBehinds: Map[Int, Vector[Int]],
      before: SideState,
      after: SideState,
      c: CharSet,
      label: Int
  ): Map[Int, Vector[Int]] =
    (side.nfa.lookBehind, before.reading, after.reading) match
      case (Some(lb), Some(state0), Some(state1)) =>
        val succeeded = lb.successSets(state0)
        lb.successSets(state1)
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
      case _ => Map.empty

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

  /** Finds the first path of a branch from a state that succeeds. */
  private def findSuccessPath(b: Branch[MarkedStep[Int]], p: Int => Boolean): Path =
    findPaths(b, p).headOption match
      case Some(path) => path
      // The look-ahead DFA tells a success from the state only when the branch of the transition succeeds.
      // $COVERAGE-OFF$
      case None => throw new IllegalStateException("a success without the path")
      // $COVERAGE-ON$

  /** Returns the node after the atom, where the rest after it is of the class `rest1`. */
  private def next(node: SearchNode, atom: SharedAtom, rest1: RestClass): SearchNode =
    val here = node.position.getOrElse(node.labelCount)
    val label = node.labelCount + 1
    val a = advance(sideA, node.a, atom.a, rest1.a, here, label)
    val b = advance(sideB, node.b, atom.b, rest1.b, here, label)
    SearchNode(a, b, rest1, atom.high, Some(label)).canonical

  /** Returns the values of the match at the end of the input, if it matches: of the first track ending there, or else
    * of the match that has ended before.
    */
  private def valuesAtEnd(side: ComparedPattern, at: SideState, here: Int): Option[Vector[Int]] =
    at.tracks.iterator
      .flatMap: track =>
        findPaths(side.nfa.markedDeltaAtEnd.getOrElse(track.state, Branch.Fail), Set.empty).headOption.map: path =>
          Writers(side, track, at.lookBehinds, here).finish(Some(path))
      .nextOption()
      .orElse(at.matched.map(Writers(side, _, at.lookBehinds, here).finish(None)))

  /** Tests whether the captures differ at the end of the input. */
  private def differsAtEnd(node: SearchNode): Boolean =
    val here = node.position.getOrElse(node.labelCount)
    valuesAtEnd(sideA, node.a, here) != valuesAtEnd(sideB, node.b, here)

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
    def initialSide(side: ComparedPattern, init: Int): SideState =
      val size = 2 * side.captures.size
      val track = Track(init, Vector.empty, Vector.fill(size)(Unset), Vector.fill(size)(0))
      SideState(Vector(track), None, initialLookBehinds(side, init, 0))

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

    val (a0, b0) = (initialSide(sideA, initA), initialSide(sideB, initB))
    for rest <- shortest.keys do push(SearchNode(a0, b0, rest, false, Some(0)).canonical, 0, None)

    boundary:
      while queue.nonEmpty do
        ctx.interrupt:
          val (f, _, node) = queue.dequeue()
          val SearchNode(a, b, rest, endsWithHigh, _) = node
          val g = distance(node)
          // The node may be pushed again by a shorter prefix.
          if f == g + shortest(rest)._1 then
            val (succeedsA, succeedsB) = (a.succeeds(rest.a), b.succeeds(rest.b))
            if succeedsA != succeedsB then break(Some(prefixOf(node) ++ restOf(rest)))
            if succeedsA then
              if rest == end && differsAtEnd(node) then break(Some(prefixOf(node)))
              for (atom, rest1) <- splits(rest) if !(endsWithHigh && atom.low) do
                push(next(node, atom, rest1), g + 1, Some((node, atom)))
      None
