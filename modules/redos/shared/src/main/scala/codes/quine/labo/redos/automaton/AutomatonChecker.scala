package codes.quine.labo.redos
package automaton

import scala.collection.mutable

import Complexity._
import automaton._
import data.Graph
import util.Timeout

/** ReDoS vulnerable RegExp checker based on automata theory. */
object AutomatonChecker {

  /** Checks a match time complexity of the NFA. */
  def check[A, Q](nfa: OrderedNFA[A, Q], maxNFASize: Int = Int.MaxValue)(implicit
      timeout: Timeout = Timeout.NoTimeout
  ): Complexity[A] =
    timeout.checkTimeout("automaton.AutomatonChecker.check")(new AutomatonChecker(nfa, maxNFASize, timeout).check())
}

/** AutomatonChecker is a ReDoS vulnerable RegExp checker based on automata theory. */
private final class AutomatonChecker[A, Q](
    private[this] val nfa: OrderedNFA[A, Q],
    maxNFASize: Int,
    private[this] implicit val timeout: Timeout
) {

  // Introduces `timeout` methods into the scope.
  import timeout._

  /** A reversed DFA constructed from [[nfa]],
    * and a NFA with multi-transitions constructed from [[nfa]].
    */
  private[this] val (reverseDFA, multiNFA) = OrderedNFA.prune(nfa, maxNFASize)

  /** A [[multiNFA]] transition graph. */
  private[this] val graph = multiNFA.toGraph.reachable(multiNFA.initSet.toSet)

  /** A [[multiNFA]] transition graph's SCCs. */
  private[this] val scc = graph.scc

  /** A map from a state of [[multiNFA]] to SCC. */
  private[this] val sccMap =
    checkTimeout("automaton.AutomatonChecker#sccMap")((for (sc <- scc; q <- sc) yield q -> sc).toMap)

  /** A graph with transitions between SCCs. */
  private[this] val sccGraph = checkTimeout("automaton.AutomatonChecker#sccGraph") {
    val edges = graph.edges.iterator
      .map { case (q1, _, q2) => (sccMap(q1), (), sccMap(q2)) }
      .filter { case (sc1, _, sc2) => sc1 != sc2 }
    val edgeSet = mutable.LinkedHashSet.empty[(IndexedSeq[(Q, Set[Q])], (), IndexedSeq[(Q, Set[Q])])]
    for (edge <- edges) checkTimeout("automaton.AutomatonChecker#sccGraph:loop") {
      edgeSet.addOne(edge)
    }
    Graph.from(edgeSet.toIndexedSeq)
  }

  /** A map from SCC to reachable SCCs. */
  private[this] val sccReachableMap = sccGraph.reachableMap

  /** A map from SCC to reachable SCCs on reversed [[sccGraph]]. */
  private[this] val sccReverseReachableMap = sccGraph.reverse.reachableMap

  /** A map from a pair of SCCs to edges (transitions) between two SCCs. */
  private[this] val sccPairEdges = checkTimeout("automaton.AutomatonChecker#sccPairGraph") {
    graph.edges
      .groupMap { case (q1, _, q2) => (sccMap(q1), sccMap(q2)) } { case (q1, a, q2) => a -> (q1, q2) }
      .view
      .mapValues(_.groupMap(_._1)(_._2).withDefaultValue(Vector.empty))
      .toMap
      .withDefaultValue(Map.empty.withDefaultValue(Vector.empty))
  }

  /** A map from a SCC to the lookahead DFA's states in this. */
  private[this] val sccLookaheadMap = scc.map { sc => sc -> sc.map(_._2).toSet }.toMap

  /** A type of [[multiNFA]] state. */
  private type R = (Q, Set[Q])

  /** A type of pumps of witness. */
  private type Pump = (R, Seq[(A, Set[Q])], R)

  /** Tests whether the SCC is an atom, which is a singleton and does not have a self-loop. */
  private[this] def isAtom(sc: Seq[R]): Boolean =
    sc.size == 1 && !graph.neighbors(sc.head).exists(_._2 == sc.head)

  /** Runs a checker. */
  def check(): Complexity[A] =
    checkTimeout("automaton.AutomatonChecker#check")(checkExponential() match {
      case Some(pump) => Exponential(witness(Vector(pump)))
      case None =>
        checkPolynomial() match {
          case (0, _)          => Constant
          case (1, _)          => Linear
          case (degree, pumps) => Polynomial(degree, witness(pumps))
        }
    })

  /** Finds an EDA structure in the graph. */
  private[this] def checkExponential(): Option[Pump] =
    checkTimeout("automaton.AutomatonChecker#checkExponential") {
      scc.iterator.filterNot(isAtom(_)).flatMap(checkExponentialComponent).nextOption()
    }

  /** Finds an EDA structure in the SCC. */
  private[this] def checkExponentialComponent(sc: IndexedSeq[R]): Option[Pump] =
    checkTimeout("automaton.AutomatonChecker#checkExponentialComponent") {
      val edges = checkTimeout("automaton.AutomatonChecker#checkExponentialComponent:edges")(sccPairEdges((sc, sc)))

      checkTimeout("automaton.AutomatonChecker#checkExponentialComponent:edges.find") {
        edges.find { case (_, es) => es.size != es.distinct.size }
      } match {
        // The SCC has multi-transitions. In this case, we can find an EDA easily by using it.
        case Some((a, es)) =>
          for {
            (q1, q2) <- es.diff(es.distinct).headOption
            back <- graph.path(Set(q2), q1)
          } yield (q1, a +: back, q1)
        // In other cases, we need to use SCC of a pair graph.
        case None =>
          // A state pair graph with co-transition.
          val g2 = checkTimeout("automaton.AutomatonChecker#checkExponentialComponent:g2")(Graph.from(for {
            a1 -> es <- edges.toIndexedSeq
            (q11, q12) <- es
            (q21, q22) <- es
          } yield ((q11, q21), a1, (q12, q22))))
          checkTimeout("automaton.AutomatonChecker#checkExponentialComponent:result") {
            g2.scc.iterator
              .flatMap { sc =>
                checkTimeout("automaton.AutomatonChecker#checkExponentialComponent:EDA")(for {
                  // If there is a SCC of g2 contains `(q1, q1)` and `(q2, q3)` (s.t. `q2 != q3`),
                  // then the SCC contains an EDA structure.
                  p1 <- sc.find { case (q1, q2) => q1 == q2 }
                  p2 <- sc.find { case (q1, q2) => q1 != q2 }
                  path1 <- g2.path(Set(p1), p2)
                  path2 <- g2.path(Set(p2), p1)
                } yield (p1._1, path1 ++ path2, p1._1))
              }
              .nextOption()
          }
      }
    }

  /** Finds an IDA structure chain in the graph. */
  private[this] def checkPolynomial(): (Int, Seq[Pump]) =
    checkTimeout("automaton.AutomatonChecker#checkPolynomial") {
      scc.map(checkPolynomialComponent).maxByOption(_._1).getOrElse((0, Seq.empty))
    }

  /** An internal cache of [[checkPolynomialComponent]] method's result. */
  private[this] val checkPolynomialComponentCache = mutable.Map.empty[Seq[R], (Int, Seq[Pump])]

  /** Finds an IDA structure chain from the SCC. */
  private[this] def checkPolynomialComponent(sc: IndexedSeq[R]): (Int, Seq[Pump]) =
    checkPolynomialComponentCache.getOrElseUpdate(
      sc,
      checkTimeout("automaton.AutomatonChecker#checkPolynomialComponent") {
        // Computes the maximum IDA structure chain from neighbors.
        val (maxDegree, maxPumps) =
          checkTimeout("automaton.AutomatonChecker#checkPolynomialComponent:(maxDegree,maxPumps)") {
            sccGraph
              .neighbors(sc)
              .map { case ((), sc) => checkPolynomialComponent(sc) }
              .maxByOption(_._1)
              .getOrElse((0, Vector.empty))
          }
        if (maxDegree == 0) (if (isAtom(sc)) 0 else 1, Vector.empty)
        else if (isAtom(sc)) (maxDegree, maxPumps)
        else {
          // Appends an IDA structure between the SCC and the maximum chain's source into the chain.
          checkTimeout("automaton.AutomatonChecker#checkPolynomialComponent:result") {
            sccReachableMap(sc).iterator
              .filter { target =>
                sc != target && !isAtom(target) && checkPolynomialComponent(target)._1 == maxDegree &&
                // If the intersection of lookahead states of `sc` and `target` is empty,
                // then there is no IDA structure between them.
                (sccLookaheadMap(sc) & sccLookaheadMap(target)).nonEmpty
              }
              .flatMap(target => checkPolynomialComponentBetween(sc, target).map((target, _)))
              .map { case (target, pump) => (maxDegree + 1, pump +: checkPolynomialComponent(target)._2) }
              .nextOption()
              .getOrElse((maxDegree, maxPumps))
          }
        }
      }
    )

  /** Finds an IDA structure between source and target SCCs. */
  private[this] def checkPolynomialComponentBetween(source: IndexedSeq[R], target: IndexedSeq[R]): Option[Pump] = {
    val sourceEdges = checkTimeout("automaton.AutomatonChecker#checkPolynomialComponentBetween:sourceEdges") {
      sccPairEdges((source, source))
    }
    val between = checkTimeout("automaton.AutomatonChecker#checkPolynomialComponentBetween:between") {
      sccReachableMap(source) & sccReverseReachableMap(target)
    }
    val betweenEdges = checkTimeout("automaton.AutomatonChecker#checkPolynomialComponentBetween:betweenEdges") {
      for (sc1 <- between; sc2 <- between) yield sccPairEdges((sc1, sc2))
    }
    val targetEdges = checkTimeout("automaton.AutomatonChecker#checkPolynomialComponentBetween:targetEdges") {
      sccPairEdges((target, target))
    }

    val g3 = checkTimeout("automaton.AutomatonChecker#checkPolynomialComponentBetween:g3") {
      Graph.from(
        (for {
          a <- multiNFA.alphabet.iterator
          (q11, q12) <- sourceEdges(a)
          (q21, q22) <- betweenEdges.flatMap(_(a))
          (q31, q32) <- targetEdges(a)
        } yield checkTimeout("automaton.AutomatonChecker#checkPolynomialComponentBetween:g3:edge") {
          ((q11, q21, q31), a, (q12, q22, q32))
        }).toIndexedSeq
      )
    }
    val g3back = checkTimeout("automaton.AutomatonChecker#checkPolynomialComponentBetween:g3back") {
      Graph.from(
        source.flatMap(q1 => target.map { q2 => ((q1, q2, q2), None, (q1, q1, q2)) }) ++
          g3.edges.map { case (qqq1, a, qqq2) => (qqq1, Some(a), qqq2) }
      )
    }

    checkTimeout("automaton.AutomatonChecker#checkPolynomialComponentBetween:result") {
      g3back.scc.iterator
        .flatMap { sc =>
          checkTimeout("automaton.AutomatonChecker#checkPolynomialComponentBetween:IDA") {
            // If there is a SCC of `g3back` contains `(q1, q1, q2)` and `(q1, q2, q2)`,
            // then an IDA structure exists between `source` and `target`.
            sc.collect { case (q1, q2, q3) if q1 == q2 && q2 != q3 && sc.contains((q1, q3, q3)) => (q1, q3) }
              .flatMap { case (q1, q2) => g3.path(Set((q1, q1, q2)), (q1, q2, q2)).map((q1, _, q2)) }
          }
        }
        .nextOption()
    }
  }

  /** Builds a witness object from pump strings and states. */
  private[this] def witness(pumps: Seq[Pump]): Witness[A] = {
    val (pumpPaths, qs) = pumps.foldLeft((Vector.empty[(Seq[A], Seq[A])], multiNFA.initSet.toSet)) {
      case ((pumpPaths, last), (q1, path, q2)) =>
        val prefix = graph.path(last, q1).get.map(_._1)
        (pumpPaths :+ (prefix, path.map(_._1)), Set(q2))
    }
    val suffix = reverseDFA.toGraph.path(Set(reverseDFA.init), qs.head._2).get.reverse
    Witness(pumpPaths, suffix)
  }
}
