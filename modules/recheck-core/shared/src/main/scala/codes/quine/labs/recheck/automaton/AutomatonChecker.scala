package codes.quine.labs.recheck
package automaton

import scala.collection.mutable

import codes.quine.labs.recheck.automaton.Complexity.*
import codes.quine.labs.recheck.common.Context
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
  def check[A, Q](nfa: OrderedNFA[A, Q], maxNFASize: Int, maxDFASize: Int, isUpperBound: Boolean)(using
      ctx: Context
  ): Iterator[Complexity[A]] =
    ctx.interrupt:
      val nfaWLA = nfa.toNFAwLA(maxNFASize, maxDFASize, isUpperBound)
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

  /** A [[nfaWLA]] transition graph's SCCs. */
  private val scc = graph.scc

  /** A map from a state of [[nfaWLA]] to SCC. */
  private val sccMap = interrupt((for sc <- scc; q <- sc yield q -> sc).toMap)

  /** A graph with transitions between SCCs. */
  private val sccGraph = interrupt:
    val edges = graph.edges.iterator
      .map { case (q1, _, q2) => (sccMap(q1), (), sccMap(q2)) }
      .filter { case (sc1, _, sc2) => sc1 != sc2 }
    val edgeSet = mutable.LinkedHashSet.empty[(IndexedSeq[(Q, P)], Unit, IndexedSeq[(Q, P)])]
    for edge <- edges do interrupt(edgeSet.addOne(edge))
    Graph.from(edgeSet.toIndexedSeq)

  /** A map from SCC to reachable SCCs. */
  private val sccReachableMap = sccGraph.reachableMap

  /** A map from SCC to reachable SCCs on reversed [[sccGraph]]. */
  private val sccReverseReachableMap = sccGraph.reverse.reachableMap

  /** A map from a pair of SCCs to edges (transitions) between two SCCs. */
  private val sccPairEdges = interrupt:
    graph.edges
      .groupMap { case (q1, _, q2) => (sccMap(q1), sccMap(q2)) } { case (q1, a, q2) => a -> (q1, q2) }
      .view
      .mapValues(_.groupMap(_._1)(_._2).withDefaultValue(Vector.empty))
      .toMap
      .withDefaultValue(Map.empty.withDefaultValue(Vector.empty))

  /** A map from an SCC to the lookahead DFA states in this. */
  private val sccLookaheadMap = scc.map { sc => sc -> sc.map(_._2).toSet }.toMap

  /** A type of [[nfaWLA]] state. */
  private type R = (Q, P)

  /** A type of [[nfaWLA]] alphabet. */
  private type B = (A, P)

  /** A type of a pair of pumps of witness and hotspots. */
  private type Pump = (R, Seq[B], R, Seq[SourceLocation])

  /** Tests whether the SCC is an atom, which is a singleton and does not have a self-loop. */
  private def isAtom(sc: Seq[R]): Boolean =
    sc.size == 1 && !graph.neighbors(sc.head).exists(_._2 == sc.head)

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
      scc.iterator
        .filterNot(isAtom)
        .flatMap(findEDA)
        .map: pump =>
          val (w, hotspot) = witness(Vector(pump))
          Exponential(w, hotspot)

  /** Finds an EDA structure in the SCC. */
  private def findEDA(sc: IndexedSeq[R]): Option[Pump] =
    interrupt:
      val edges = interrupt(sccPairEdges((sc, sc)))

      val found = interrupt(edges.find { case (_, es) => es.size != es.distinct.size })
      found match
        // The SCC has multi-transitions. In this case, we can find an EDA easily by using it.
        case Some((a, es)) =>
          for
            (q1, q2) <- es.diff(es.distinct).headOption
            (back, last) <- graph.path(Set(q2), q1)
          yield (q1, a +: back.map(_._2), q1, spots((q1, a) +: back, last))
        // In other cases, we need to use SCC of a pair graph.
        case None =>
          // A state pair graph with co-transition.
          val g2 = interrupt(Graph.from(for
            a1 -> es <- edges.toIndexedSeq
            (q11, q12) <- es
            (q21, q22) <- es
          yield ((q11, q21), a1, (q12, q22))))
          val result = interrupt:
            g2.scc.view
              .flatMap: sc =>
                val eda = interrupt(for
                  // If there is an SCC of g2 that contains `(q1, q1)` and `(q2, q3)` (s.t. `q2 != q3`),
                  // then the SCC contains an EDA structure.
                  p1 <- sc.find { case (q1, q2) => q1 == q2 }
                  p2 <- sc.find { case (q1, q2) => q1 != q2 }
                  (path1, _) <- g2.path(Set(p1), p2)
                  (path2, _) <- g2.path(Set(p2), p1)
                yield (p1._1, path1.map(_._2) ++ path2.map(_._2), p1._1, spotsG2(path1 ++ path2, p1)))
                eda
              .headOption
          result

  /** Finds an IDA structure chain in the graph. */
  private def checkPolynomial(): Iterator[Complexity[A]] =
    log("automaton: find IDA")
    interrupt:
      scc
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
  private val idaChainCache = mutable.Map.empty[Seq[R], (Int, Seq[Pump])]

  /** Finds an IDA structure chain from the SCC. */
  private def findIDAChain(sc: IndexedSeq[R]): (Int, Seq[Pump]) =
    idaChainCache.getOrElseUpdate(
      sc,
      interrupt:
        // Computes the maximum IDA structure chain from neighbors.
        val (maxDegree, maxPumps) = interrupt:
          sccGraph
            .neighbors(sc)
            .map { case ((), sc) => findIDAChain(sc) }
            .maxByOption(_._1)
            .getOrElse((0, Vector.empty))
        if maxDegree == 0 then (if isAtom(sc) then 0 else 1, Vector.empty)
        else if isAtom(sc) then (maxDegree, maxPumps)
        else
          // Appends an IDA structure between the SCC and the maximum chain's source into the chain.
          val result = interrupt:
            sccReachableMap(sc).iterator
              .filter: target =>
                sc != target && !isAtom(target) && findIDAChain(target)._1 == maxDegree &&
                  // If the intersection of lookahead states of `sc` and `target` is empty,
                  // then there is no IDA structure between them.
                  (sccLookaheadMap(sc) & sccLookaheadMap(target)).nonEmpty
              .flatMap(target => findIDABetween(sc, target).map((target, _)))
              .map { case (target, pump) => (maxDegree + 1, pump +: findIDAChain(target)._2) }
              .nextOption()
              .getOrElse((maxDegree, maxPumps))
          result
    )

  /** Finds an IDA structure between source and target SCCs. */
  private def findIDABetween(source: IndexedSeq[R], target: IndexedSeq[R]): Option[Pump] =
    val sourceEdges = interrupt(sccPairEdges((source, source)))
    val between = interrupt(sccReachableMap(source) & sccReverseReachableMap(target))
    val betweenEdges = interrupt(for sc1 <- between; sc2 <- between yield sccPairEdges((sc1, sc2)))
    val targetEdges = interrupt(sccPairEdges((target, target)))

    val g3 = interrupt:
      Graph.from(
        (for
          a <- nfaWLA.alphabet.iterator
          (q11, q12) <- sourceEdges(a)
          (q21, q22) <- betweenEdges.flatMap(_(a))
          (q31, q32) <- targetEdges(a)
        yield
          val edge = interrupt(((q11, q21, q31), a, (q12, q22, q32)))
          edge
        ).toIndexedSeq
      )
    val g3back = interrupt:
      Graph.from:
        source.flatMap(q1 => target.map { q2 => ((q1, q2, q2), None, (q1, q1, q2)) }) ++
          g3.edges.map { case (qqq1, a, qqq2) => (qqq1, Some(a), qqq2) }

    val result = interrupt:
      g3back.scc.iterator
        .flatMap: sc =>
          val ida = interrupt:
            // If there is an SCC of `g3back` that contains `(q1, q1, q2)` and `(q1, q2, q2)`,
            // then an IDA structure exists between `source` and `target`.
            sc
              .collect:
                case (q1, q2, q3) if q1 == q2 && q2 != q3 && sc.contains((q1, q3, q3)) => (q1, q3)
              .flatMap:
                case (q1, q2) =>
                  g3.path(Set((q1, q1, q2)), (q1, q2, q2))
                    .map:
                      case (path, _) => (q1, path.map(_._2), q2, spotsG3(path, (q1, q2, q2)))
          ida
        .nextOption()
    result

  /** Builds a witness object from pump strings and states. */
  private def witness(pumps: Seq[Pump]): (Witness[A], Hotspot) =
    log("automaton: either EDA or IDA is found")
    val (pumpPaths, qs, spots) =
      pumps.foldLeft((Vector.empty[(Seq[A], Seq[A])], nfaWLA.initSet.toSet, Vector.empty[Hotspot.Spot])):
        case ((pumpPaths, last, spots), (q1, path, q2, pos)) =>
          val (prefix, _) = graph.path(last, q1).get
          val newSpots = spots ++ pos.map { case SourceLocation(s, e) => Hotspot.Spot(s, e, Hotspot.Hot) } ++
            this.spots(prefix, q1).map { case SourceLocation(s, e) => Hotspot.Spot(s, e, Hotspot.Warm) }
          (pumpPaths :+ (prefix.map(_._2._1), path.map(_._1)), Set(q2), newSpots)
    val (tail, last) = graph.path(qs, witnessEnds).get
    val tailSpots = this.spots(tail, last).map { case SourceLocation(s, e) => Hotspot.Spot(s, e, Hotspot.Warm) }
    val suffix =
      if isUpperBound then
        val read = graph.neighbors.getOrElse(last, Vector.empty).map(_._1._1).toSet
        // The look-ahead DFA has every character, while the NFA has only the characters that it reads.
        nfaWLA.lookAheadDFA.alphabet.find(a => !read.contains(a)).toVector
      else nfaWLA.lookAheadDFA.toGraph.path(Set(nfaWLA.lookAheadDFA.init), last._2).get._1.map(_._2).reverse
    (Witness(pumpPaths, tail.map(_._2._1) ++ suffix), Hotspot(spots ++ tailSpots))

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

  /** Gets hotspots positions from a G3 path. */
  private def spotsG3(path: Seq[((R, R, R), B)], last: (R, R, R)): Seq[SourceLocation] =
    val spots1 = spots(path.map { case ((q1, _, _), a) => (q1, a) }, last._1)
    val spots2 = spots(path.map { case ((_, q2, _), a) => (q2, a) }, last._2)
    val spots3 = spots(path.map { case ((_, _, q3), a) => (q3, a) }, last._3)
    spots1 ++ spots2 ++ spots3
