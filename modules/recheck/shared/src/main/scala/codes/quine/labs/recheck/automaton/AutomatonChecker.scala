package codes.quine.labs.recheck
package automaton

import scala.collection.immutable.BitSet
import scala.collection.mutable

import codes.quine.labs.recheck.automaton.AutomatonChecker.*
import codes.quine.labs.recheck.automaton.Complexity.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.data.Graph
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.resyntax.ast.SourceLocation

/** ReDoS vulnerable RegExp checker based on automata theory. */
object AutomatonChecker:

  /** Checks a match time complexity of the NFA.
    *
    * With `isUpperBound`, it checks an upper bound of the complexity, where every path is explored (see
    * [[OrderedNFA.toNFAwLA]]).
    */
  def check[A, Q](nfa: OrderedNFA[A, Q], isUpperBound: Boolean, maxNFASize: Int, maxDFASize: Int)(using
      ctx: Context
  ): Iterator[Complexity[A]] =
    ctx.interrupt:
      val lookAheadDFA =
        if isUpperBound then DFA.universal(nfa.alphabet, Set.empty[Q]) else nfa.toLookAheadDFA(maxDFASize, maxNFASize)
      // The look-ahead DFA states are numbered, since hashing their sets takes most of the time on a large NFA.
      val sets = lookAheadDFA.stateSet.toIndexedSeq
      val nfaWLA = nfa.toNFAwLA(lookAheadDFA.mapState(sets.zipWithIndex.toMap), sets, isUpperBound, maxNFASize)
      val complexities = new AutomatonChecker(nfaWLA, false, isUpperBound).check()
      nfa.lookBehind match
        case None => complexities
        // A look-behind whose inner expression has no cycle costs at most a constant on each attempt, which is less
        // than the cost of the transition that tries it.
        case Some(lookBehind) if !hasCycle(lookBehind.inner) => complexities
        case Some(lookBehind)                                =>
          val product = LookBehindProduct.build(nfaWLA, lookBehind, maxNFASize)
          val lookBehindComplexities = new AutomatonChecker(product, true, isUpperBound).check().map(dropInputEnds)
          merge(complexities, lookBehindComplexities).distinct

  /** The bits of a number of a state in the products of the searches, which pack three of them into a `Long`. */
  private[automaton] val StateBits = 21

  /** Throws [[UnsupportedException]] if the states are too many to pack their numbers into the products. */
  private[automaton] def checkStateCount(count: Int)(using ctx: Context): Unit =
    if count > (1 << StateBits) then
      ctx.log("auto: exceed the states that the automaton checker packs")
      throw new UnsupportedException("NFAwLA has too many states")

  private val StateMask = (1L << StateBits) - 1

  /** A number of a state, which the products of the searches pack. */
  private type StateNumber = Int

  /** A pair of numbers of states, a vertex of the product of the EDA search. */
  private type StatePair = Long

  private def statePair(q1: StateNumber, q2: StateNumber): StatePair = (q1.toLong << 32) | q2

  private def first(p: StatePair): StateNumber = (p >>> 32).toInt

  private def second(p: StatePair): StateNumber = p.toInt

  /** A triple of numbers of states, a vertex of the product of the IDA search. */
  private type StateTriple = Long

  private def stateTriple(q1: StateNumber, q2: StateNumber, q3: StateNumber): StateTriple =
    (q1.toLong << (2 * StateBits)) | (q2.toLong << StateBits) | q3

  private def third1(t: StateTriple): StateNumber = (t >>> (2 * StateBits)).toInt

  private def third2(t: StateTriple): StateNumber = ((t >>> StateBits) & StateMask).toInt

  private def third3(t: StateTriple): StateNumber = (t & StateMask).toInt

  /** A number of a character, which the keys of the transitions in the IDA search pack. */
  private type CharNumber = Int

  /** The number of no character, which the edges back in the IDA search have. */
  private val NoChar: CharNumber = -1

  /** A pair of the numbers of a character and a state, a key of the transitions in the IDA search. */
  private type CharState = Long

  private def charState(c: CharNumber, q: StateNumber): CharState = (c.toLong << 32) | q

  /** Merges the complexities, each ordered from the worst as [[AutomatonChecker#check]] gives them, into one ordered
    * from the worst, where `xs` comes first on a tie.
    */
  private def merge[A](xs: Iterator[Complexity[A]], ys: Iterator[Complexity[A]]): Iterator[Complexity[A]] =
    val bx = xs.buffered
    val by = ys.buffered
    Iterator
      .continually(())
      .takeWhile(_ => bx.hasNext || by.hasNext)
      .map: _ =>
        if !by.hasNext || bx.hasNext && complexityOrder(bx.head) <= complexityOrder(by.head) then bx.next()
        else by.next()

  /** Tests whether the NFA has a cycle of transitions. */
  private def hasCycle[A](nfa: OrderedNFA[A, Int])(using ctx: Context): Boolean =
    val graph = Graph.from(nfa.delta.toIndexedSeq.flatMap { case ((s1, a), b) => b.allLeaves.map((s1, a, _)) })
    graph.scc.exists(sc => sc.size > 1 || graph.neighbors(sc.head).exists(_._2 == sc.head))

  /** Orders complexities from the worst. */
  private def complexityOrder(c: Complexity[?]): Int = c match
    case _: Exponential[?]   => Int.MinValue
    case p: Polynomial[?]    => -p.degree
    case Complexity.Linear   => -1
    case Complexity.Constant => 0

  /** Drops the end-of-input markers from the witness of the complexity. */
  private def dropInputEnds[A](c: Complexity[Option[A]]): Complexity[A] =
    def dropFromWitness[B <: Option[A]](w: Witness[B]): Witness[A] =
      Witness(w.pumps.map((pre, pump) => (pre.flatten[A], pump.flatten[A])), w.suffix.flatten[A])

    c match
      case Exponential(witness, hotspot)        => Exponential(dropFromWitness(witness), hotspot)
      case Polynomial(degree, witness, hotspot) => Polynomial(degree, dropFromWitness(witness), hotspot)
      case safe: Safe                           => safe

/** AutomatonChecker is a ReDoS vulnerable RegExp checker based on automata theory.
  *
  * Without `countsAcceptedPaths`, it counts the paths over all prefixes, which is the backtracking cost. With it, it
  * counts the paths into the accepting states, where every state of the NFA must reach them.
  *
  * With `isUpperBound`, the look-ahead DFA of [[nfaWLA]] tells no failure, so the suffix of a witness is a character
  * that the last state cannot read, if any.
  */
private final class AutomatonChecker[A, Q, P](
    private val nfaWLA: NFAwLA[A, Q, P],
    private val countsAcceptedPaths: Boolean,
    private val isUpperBound: Boolean
)(using private val ctx: Context):

  // Introduces `ctx` methods into the scope.
  import ctx.*

  /** A [[nfaWLA]] transition graph. */
  private val graph = interrupt(nfaWLA.toGraph.reachable(nfaWLA.initSet.toSet))

  /** The number of the components of an IDA chain minus the degree that it gives.
    *
    * An IDA chain of `n` components gives `n` degrees on counting the paths over all prefixes, and `n - 1` on counting
    * the paths into the accepting states.
    */
  private val degreeOffset = if countsAcceptedPaths then 1 else 0

  /** The states where a witness stops following the transitions, after which it follows the look-ahead DFA. */
  private val witnessEnds = if countsAcceptedPaths then nfaWLA.acceptSet else nfaWLA.stateSet

  /** A [[nfaWLA]] transition graph's SCCs, numbered by their indices, where an SCC comes after the ones it reaches. */
  private val scc = graph.scc.toIndexedSeq

  /** A map from a state of [[nfaWLA]] to the index of its SCC. */
  private val sccIndex = interrupt((for (sc, i) <- scc.zipWithIndex; q <- sc yield q -> i).toMap)

  /** The transitions from the states of each SCC, with the indices of the SCCs that they go to. */
  private val sccOutEdges = interrupt:
    val edges = Array.fill(scc.size)(Vector.newBuilder[(Int, B, R, R)])
    for (q1, a, q2) <- graph.edges do interrupt(edges(sccIndex(q1)).addOne((sccIndex(q2), a, q1, q2)))
    edges.map(_.result()).toIndexedSeq

  /** The indices of the other SCCs that each SCC has transitions to, in the order of the transitions. */
  private val sccNeighbors = interrupt(scc.indices.map(i => sccOutEdges(i).map(_._1).filter(_ != i).distinct))

  /** The indices of the other SCCs that have transitions to each SCC. */
  private lazy val sccPredecessors = interrupt:
    val predecessors = Array.fill(scc.size)(Vector.newBuilder[Int])
    for i <- scc.indices; j <- sccNeighbors(i) do predecessors(j).addOne(i)
    predecessors.map(_.result()).toIndexedSeq

  /** The SCCs that the SCC reaches through `neighbors`, including itself. */
  private def reach(i: Int, neighbors: IndexedSeq[IndexedSeq[Int]]): BitSet = interrupt:
    val seen = mutable.BitSet(i)
    val stack = mutable.Stack(i)
    while stack.nonEmpty do interrupt(for k <- neighbors(stack.pop()) if seen.add(k) do stack.push(k))
    seen.toImmutable

  /** The SCCs that each SCC reaches, including itself, computed when needed. */
  private val sccReachable = mutable.Map.empty[Int, BitSet]

  private def reachable(i: Int): BitSet = sccReachable.getOrElseUpdate(i, reach(i, sccNeighbors))

  /** The SCCs that reach each SCC, including itself, computed when needed. */
  private val sccCoreachable = mutable.Map.empty[Int, BitSet]

  private def coreachable(i: Int): BitSet = sccCoreachable.getOrElseUpdate(i, reach(i, sccPredecessors))

  /** The first number of the states of each SCC, where the states are numbered in the order of the SCCs. */
  private lazy val sccStateStarts = scc.scanLeft(0)(_ + _.size)

  /** The SCC of each numbered state. */
  private lazy val stateSccs = scc.indices.flatMap(i => Iterator.fill(scc(i).size)(i))

  /** The states by their numbers, in the order of the SCCs. */
  private lazy val numberedStates: IndexedSeq[R] = scc.flatten

  /** The numbers of the states, which the products of the searches pack (see [[AutomatonChecker.checkStateCount]]). */
  private lazy val stateNumbers: Map[R, StateNumber] = interrupt:
    AutomatonChecker.checkStateCount(numberedStates.size)
    numberedStates.zipWithIndex.toMap

  /** The characters by their numbers. */
  private lazy val chars: IndexedSeq[B] = interrupt(graph.edges.map(_._2).distinct)

  /** The numbers of the characters. */
  private lazy val charNumbers: Map[B, CharNumber] = chars.zipWithIndex.toMap

  /** The transitions from the states of each SCC by the numbers, with the indices of the SCCs that they go to. */
  private lazy val sccNumberedOutEdges: IndexedSeq[IndexedSeq[(Int, CharNumber, StateNumber, StateNumber)]] =
    interrupt:
      sccOutEdges.map(_.map((j, a, q1, q2) => (j, charNumbers(a), stateNumbers(q1), stateNumbers(q2))))

  /** The transitions in each SCC, from the numbers of the characters to the pairs of the numbers of the states. */
  private lazy val sccLoops: IndexedSeq[Map[CharNumber, IndexedSeq[(StateNumber, StateNumber)]]] = interrupt:
    sccNumberedOutEdges.indices.map: i =>
      sccNumberedOutEdges(i).collect { case (j, c, q1, q2) if j == i => (c, (q1, q2)) }.groupMap(_._1)(_._2)

  /** The numbers of the states that each numbered state has transitions to. */
  private lazy val stateSuccessors = interrupt:
    val successors = Array.fill(numberedStates.size)(Vector.newBuilder[StateNumber])
    for edges <- sccNumberedOutEdges; (_, _, q1, q2) <- edges do successors(q1).addOne(q2)
    successors.map(_.result().distinct).toIndexedSeq

  /** The other SCCs that each SCC reaches, nearest first by the length of the input to them, computed when needed. */
  private val sccNearest = mutable.Map.empty[Int, IndexedSeq[Int]]

  private def nearest(i: Int): IndexedSeq[Int] =
    sccNearest.getOrElseUpdate(
      i,
      interrupt:
        val starts = sccStateStarts(i) until sccStateStarts(i + 1)
        val seen = mutable.BitSet()
        seen ++= starts
        val queue = mutable.Queue.from(starts)
        val order = mutable.LinkedHashSet.empty[Int]
        while queue.nonEmpty do
          interrupt:
            for q <- stateSuccessors(queue.dequeue()) if seen.add(q) do
              order.add(stateSccs(q))
              queue.enqueue(q)
        order.toIndexedSeq
    )

  /** A map from a pair of the indices of SCCs to the transitions between them by the characters. */
  private val sccPairEdges = interrupt:
    scc.indices
      .flatMap(i => sccOutEdges(i).map((j, a, q1, q2) => ((i, j), a, (q1, q2))))
      .groupMap(_._1)(e => (e._2, e._3))
      .view
      .mapValues(_.groupMap(_._1)(_._2).withDefaultValue(Vector.empty))
      .toMap
      .withDefaultValue(Map.empty.withDefaultValue(Vector.empty))

  /** The lookahead DFA states in each SCC. */
  private val sccLookaheads = scc.map(_.map(_._2).toSet)

  /** Whether each SCC is an atom, which is a singleton and does not have a self-loop. */
  private val sccIsAtom = scc.indices.map(i => scc(i).size == 1 && !sccOutEdges(i).exists(_._1 == i))

  /** A type of [[nfaWLA]] state. */
  private type R = (Q, P)

  /** A type of [[nfaWLA]] alphabet. */
  private type B = (A, P)

  /** Pump is a pump of a witness: the path of its characters from `r1` to `r2`, and the source locations on its loops
    * and on its path between the two loops, which only an IDA structure has.
    */
  private final case class Pump(r1: R, path: Seq[B], r2: R, loops: Seq[SourceLocation], between: Seq[SourceLocation])

  /** Runs a checker. */
  def check(): Iterator[Complexity[A]] = interrupt:
    ctx.log:
      s"""|automaton: start
          |  scc size: ${scc.size}""".stripMargin
    (checkExponential() ++ checkPolynomial()).distinct

  /** Finds an EDA structure in the graph. */
  private def checkExponential(): Iterator[Complexity[A]] =
    log("automaton: find EDA")
    interrupt:
      scc.indices.iterator
        .filterNot(sccIsAtom)
        .flatMap(findEDA)
        .map: pump =>
          val (w, hotspot) = witness(Vector(pump))
          Exponential(w, hotspot)

  /** Finds an EDA structure in the SCC of the index. */
  private def findEDA(sc: Int): Option[Pump] =
    interrupt:
      val edges = interrupt(sccPairEdges((sc, sc)))

      val found = interrupt(edges.find { case (_, es) => es.size != es.distinct.size })
      found match
        // The SCC has multi-transitions. In this case, we can find an EDA easily by using it.
        case Some((a, es)) =>
          for
            (q1, q2) <- es.diff(es.distinct).headOption
            (back, last) <- graph.path(Set(q2), q1)
          yield Pump(q1, a +: back.map(_._2), q1, spots((q1, a) +: back, last), Vector.empty)
        // In other cases, we need to use SCC of a pair graph.
        case None =>
          // A state pair graph with co-transition, whose vertices pack the numbers of the states.
          val g2 = interrupt(Graph.from(for
            (c, es) <- sccLoops(sc).toIndexedSeq
            (q11, q12) <- es
            (q21, q22) <- es
          yield (statePair(q11, q21), c, statePair(q12, q22))))
          val result = interrupt:
            g2.scc.view
              .flatMap: sc =>
                val eda = interrupt(for
                  // If there is an SCC of g2 that contains `(q1, q1)` and `(q2, q3)` (s.t. `q2 != q3`),
                  // then the SCC contains an EDA structure.
                  p1 <- sc.find(p => first(p) == second(p))
                  p2 <- sc.find(p => first(p) != second(p))
                  (path1, _) <- g2.path(Set(p1), p2)
                  (path2, _) <- g2.path(Set(p2), p1)
                yield
                  val q1 = numberedStates(first(p1))
                  val path = (path1 ++ path2).map: (p, c) =>
                    ((numberedStates(first(p)), numberedStates(second(p))), chars(c))
                  Pump(q1, path.map(_._2), q1, spotsG2(path, (q1, q1)), Vector.empty))
                eda
              .headOption
          result

  /** Finds an IDA structure chain in the graph. */
  private def checkPolynomial(): Iterator[Complexity[A]] =
    log("automaton: find IDA")
    interrupt:
      scc.indices
        .map(findIDAChain)
        .sortBy(-_._1)
        .iterator
        .map((degree, pumps) => (degree - degreeOffset, pumps))
        .map:
          case (degree, _) if degree <= 0 => Constant
          case (1, _)                     => Linear
          case (degree, pumps)            =>
            val (w, hotspot) = witness(pumps)
            Polynomial(degree, w, hotspot)

  /** An internal cache of [[findIDAChain]] method's result. */
  private val idaChainCache = mutable.Map.empty[Int, (Int, Seq[Pump])]

  /** Finds an IDA structure chain from the SCC of the index. */
  private def findIDAChain(sc: Int): (Int, Seq[Pump]) =
    idaChainCache.getOrElseUpdate(
      sc,
      interrupt:
        // Computes the maximum IDA structure chain from neighbors.
        val (maxDegree, maxPumps) = interrupt:
          sccNeighbors(sc)
            .map(findIDAChain)
            .maxByOption(_._1)
            .getOrElse((0, Vector.empty))
        if maxDegree == 0 then (if sccIsAtom(sc) then 0 else 1, Vector.empty)
        else if sccIsAtom(sc) then (maxDegree, maxPumps)
        else
          // Appends an IDA structure between the SCC and the maximum chain's source into the chain. The nearest target
          // comes first, since a witness through less of the pattern is likelier to run as it is (e.g. not through
          // the copy of a capture that an upper bound makes for a back-reference).
          val result = interrupt:
            nearest(sc).iterator
              .filter: target =>
                sc != target && !sccIsAtom(target) && findIDAChain(target)._1 == maxDegree &&
                  // If the intersection of lookahead states of `sc` and `target` is empty,
                  // then there is no IDA structure between them.
                  (sccLookaheads(sc) & sccLookaheads(target)).nonEmpty
              .flatMap(target => findIDABetween(sc, target).map((target, _)))
              .map { case (target, pump) => (maxDegree + 1, pump +: findIDAChain(target)._2) }
              .nextOption()
              .getOrElse((maxDegree, maxPumps))
          result
    )

  /** Finds an IDA structure between the source and the target SCCs of the indices. */
  private def findIDABetween(source: Int, target: Int): Option[Pump] =
    val between = interrupt(reachable(source) & coreachable(target))
    // The transitions from the states in the SCCs between them, and in `target`, by the characters and the states.
    def nextOf(
        edges: Iterator[(CharNumber, StateNumber, StateNumber)]
    ): mutable.LongMap[mutable.ArrayBuffer[StateNumber]] =
      val next = mutable.LongMap.empty[mutable.ArrayBuffer[StateNumber]]
      for (c, q1, q2) <- edges do next.getOrElseUpdate(charState(c, q1), mutable.ArrayBuffer.empty).addOne(q2)
      next
    val betweenNext = interrupt:
      nextOf(
        between.iterator.flatMap(i =>
          sccNumberedOutEdges(i).collect { case (j, c, q1, q2) if between(j) => (c, q1, q2) }
        )
      )
    val targetNext = interrupt(nextOf(sccLoops(target).iterator.flatMap((c, es) => es.map((q1, q2) => (c, q1, q2)))))
    val sourceOut = interrupt:
      sccLoops(source).toSeq.flatMap((c, es) => es.map((q1, q2) => (q1, (c, q2)))).groupMap(_._1)(_._2)
    val none = mutable.ArrayBuffer.empty[StateNumber]

    // The product of the transitions in `source`, between them, and in `target`, which is built only from the
    // triples `(q1, q1, q2)` for `q1` in `source` and `q2` in `target`, since only them matter. Its vertices pack the
    // numbers of the states.
    // A transition goes to a state of the look-ahead DFA state of its character, so the loops of `q1` and `q2` on the
    // same characters end at the same look-ahead DFA state.
    val sourceStates = sccStateStarts(source) until sccStateStarts(source + 1)
    val targetStates = (sccStateStarts(target) until sccStateStarts(target + 1)).groupBy(numberedStates(_)._2)
    val pairs = interrupt:
      sourceStates.flatMap(q1 => targetStates.getOrElse(numberedStates(q1)._2, Vector.empty).map((q1, _)))
    val starts = pairs.map((q1, q2) => stateTriple(q1, q1, q2))
    val g3 = interrupt:
      val seen = mutable.LongMap.empty[Unit]
      for v <- starts do seen(v) = ()
      val queue = mutable.Queue.from(starts)
      val edges = Vector.newBuilder[(StateTriple, CharNumber, StateTriple)]
      while queue.nonEmpty do
        interrupt:
          val v = queue.dequeue()
          for
            (c, p1) <- sourceOut.getOrElse(third1(v), Vector.empty)
            p2 <- betweenNext.getOrElse(charState(c, third2(v)), none)
            p3 <- targetNext.getOrElse(charState(c, third3(v)), none)
          do
            val w = stateTriple(p1, p2, p3)
            edges.addOne((v, c, w))
            if !seen.contains(w) then
              seen(w) = ()
              queue.enqueue(w)
      Graph.from(edges.result())
    // The edges back from `(q1, q2, q2)` to `(q1, q1, q2)` have no character.
    val g3back = interrupt:
      Graph.from(pairs.map((q1, q2) => (stateTriple(q1, q2, q2), NoChar, stateTriple(q1, q1, q2))) ++ g3.edges)

    val result = interrupt:
      g3back.scc.iterator
        .flatMap: sc =>
          val members = interrupt(sc.toSet)
          val ida = interrupt:
            // If there is an SCC of `g3back` that contains `(q1, q1, q2)` and `(q1, q2, q2)`,
            // then an IDA structure exists between `source` and `target`.
            sc
              .collect:
                case v
                    if third1(v) == third2(v) && third2(v) != third3(v) &&
                      members.contains(stateTriple(third1(v), third3(v), third3(v))) =>
                  (third1(v), third3(v))
              .flatMap: (q1, q2) =>
                g3.path(Set(stateTriple(q1, q1, q2)), stateTriple(q1, q2, q2))
                  .map: (path, _) =>
                    val (r1, r2) = (numberedStates(q1), numberedStates(q2))
                    val decoded = path.map: (v, c) =>
                      ((numberedStates(third1(v)), numberedStates(third2(v)), numberedStates(third3(v))), chars(c))
                    val between = spots(decoded.map { case ((_, q2, _), a) => (q2, a) }, r2)
                    Pump(r1, decoded.map(_._2), r2, spotsG3(decoded, (r1, r2, r2)), between)
          ida
        .nextOption()
    result

  /** Builds a witness object from pump strings and states, with its hotspots.
    *
    * The parts on the loops of the pumps are hot, and the ones between the two loops of an IDA structure are warm. The
    * other parts that the matching goes through on the witness are cold.
    */
  private def witness(pumps: Seq[Pump]): (Witness[A], Hotspot) =
    log("automaton: either EDA or IDA is found")

    // The pumps with their prefixes, the states after them, their spots, and the graph of the witness up to the vertex
    // `v`.
    final case class PartialWitness(
        pumpPaths: Vector[(Seq[A], Seq[A])],
        last: Set[R],
        spots: Vector[Hotspot.Spot],
        witnessGraph: Graph[Int, B],
        v: Int
    )

    val init = PartialWitness(Vector.empty, nfaWLA.initSet.toSet, Vector.empty, Graph.from(IndexedSeq.empty), 0)
    val partial = pumps.foldLeft(init): (partial, pump) =>
      val (prefix, _) = graph.path(partial.last, pump.r1).get
      val spots = pump.loops.map { case SourceLocation(s, e) => Hotspot.Spot(s, e, Hotspot.Hot) } ++
        pump.between.map { case SourceLocation(s, e) => Hotspot.Spot(s, e, Hotspot.Warm) }
      // The first character of the pump enters its cycle, so that the witness goes on from the end of the pump.
      val v = partial.v + prefix.size
      val witnessGraph = Graph.line(partial.v, prefix.map(_._2) :+ pump.path.head) ++
        Graph.cycle(v + 1, pump.path.tail :+ pump.path.head)
      PartialWitness(
        partial.pumpPaths :+ (prefix.map(_._2._1), pump.path.map(_._1)),
        Set(pump.r2),
        partial.spots ++ spots,
        partial.witnessGraph ++ witnessGraph,
        v + pump.path.size
      )

    val (tail, last) = graph.path(partial.last, witnessEnds).get
    val suffix: Seq[B] =
      if isUpperBound then
        val read = graph.neighbors.getOrElse(last, Vector.empty).map(_._1._1).toSet
        // The look-ahead DFA has every character, while the NFA has only the characters that it reads.
        nfaWLA.lookAheadDFA.alphabet.find(a => !read.contains(a)).map((_, nfaWLA.lookAheadDFA.init)).toVector
      else nfaWLA.lookAheadDFA.toGraph.path(Set(nfaWLA.lookAheadDFA.init), last._2).get._1.map(_.swap).reverse

    // The parts that the matching goes through on the witness are cold, unless they are hot or warm.
    val attackGraph = partial.witnessGraph ++ Graph.line(partial.v, tail.map(_._2) ++ suffix)
    val coldSpots = graph
      .product(attackGraph, nfaWLA.initSet.toSet.map((_, 0)))
      .edges
      .flatMap:
        case ((r1, _), b, (r2, _)) => nfaWLA.sourcemap.getOrElse((r1, b, r2), Vector.empty)
      .map { case SourceLocation(s, e) => Hotspot.Spot(s, e, Hotspot.Cold) }

    (Witness(partial.pumpPaths, (tail.map(_._2) ++ suffix).map(_._1)), Hotspot(partial.spots ++ coldSpots))

  /** Gets hotspots positions from a path. */
  private def spots(path: Seq[(R, B)], last: R): Seq[SourceLocation] =
    path
      .foldRight((last, Vector.empty[SourceLocation])):
        case ((q1, a), (q2, spots)) =>
          (q1, spots ++ nfaWLA.sourcemap.getOrElse((q1, a, q2), Vector.empty))
      ._2

  /** Gets hotspots positions from a G2 path. */
  private def spotsG2(path: Seq[((R, R), B)], last: (R, R)): Seq[SourceLocation] =
    val spots1 = spots(path.map { case ((q1, _), a) => (q1, a) }, last._1)
    val spots2 = spots(path.map { case ((_, q2), a) => (q2, a) }, last._2)
    spots1 ++ spots2

  /** Gets hotspots positions from the loops of a G3 path, which its first and third states go around. */
  private def spotsG3(path: Seq[((R, R, R), B)], last: (R, R, R)): Seq[SourceLocation] =
    val spots1 = spots(path.map { case ((q1, _, _), a) => (q1, a) }, last._1)
    val spots3 = spots(path.map { case ((_, _, q3), a) => (q3, a) }, last._3)
    spots1 ++ spots3
