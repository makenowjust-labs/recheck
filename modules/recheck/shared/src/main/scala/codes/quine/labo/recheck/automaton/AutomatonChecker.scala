package codes.quine.labo.recheck
package automaton

import scala.collection.mutable

import Complexity._
import common.Context
import data.Graph

/** ReDoS vulnerable RegExp checker based on automata theory. */
object AutomatonChecker {

  /** Checks a match time complexity of the NFA. */
  def check[A, Q](nfa: OrderedNFA[A, Q], maxNFASize: Int = Int.MaxValue)(implicit ctx: Context): Complexity[A] =
    ctx.interrupt(new AutomatonChecker(nfa, maxNFASize).check())
}

/** AutomatonChecker is a ReDoS vulnerable RegExp checker based on automata theory. */
private final class AutomatonChecker[A, Q](
    private[this] val nfa: OrderedNFA[A, Q],
    maxNFASize: Int
)(implicit private[this] implicit val ctx: Context) {

  // Introduces `ctx` methods into the scope.
  import ctx._

  /** A NFA with look-ahead constructed from [[nfa]].
    */
  private[this] val nfaWLA = nfa.toNFAwLA(maxNFASize)

  /** A [[nfaWLA]] transition graph. */
  private[this] val graph = nfaWLA.toGraph.reachable(nfaWLA.initSet.toSet)

  /** A [[nfaWLA]] transition graph's SCCs. */
  private[this] val scc = graph.scc

  /** A map from a state of [[nfaWLA]] to SCC. */
  private[this] val sccMap = interrupt((for (sc <- scc; q <- sc) yield q -> sc).toMap)

  /** A graph with transitions between SCCs. */
  private[this] val sccGraph = interrupt {
    val edges = graph.edges.iterator
      .map { case (q1, _, q2) => (sccMap(q1), (), sccMap(q2)) }
      .filter { case (sc1, _, sc2) => sc1 != sc2 }
    val edgeSet = mutable.LinkedHashSet.empty[(IndexedSeq[(Q, Set[Q])], Unit, IndexedSeq[(Q, Set[Q])])]
    for (edge <- edges) interrupt(edgeSet.addOne(edge))
    Graph.from(edgeSet.toIndexedSeq)
  }

  /** A map from SCC to reachable SCCs. */
  private[this] val sccReachableMap = sccGraph.reachableMap

  /** A map from SCC to reachable SCCs on reversed [[sccGraph]]. */
  private[this] val sccReverseReachableMap = sccGraph.reverse.reachableMap

  /** A map from a pair of SCCs to edges (transitions) between two SCCs. */
  private[this] val sccPairEdges = interrupt {
    graph.edges
      .groupMap { case (q1, _, q2) => (sccMap(q1), sccMap(q2)) } { case (q1, a, q2) => a -> (q1, q2) }
      .view
      .mapValues(_.groupMap(_._1)(_._2).withDefaultValue(Vector.empty))
      .toMap
      .withDefaultValue(Map.empty.withDefaultValue(Vector.empty))
  }

  /** A map from a SCC to the lookahead DFA states in this. */
  private[this] val sccLookaheadMap = scc.map { sc => sc -> sc.map(_._2).toSet }.toMap

  /** A type of [[nfaWLA]] state. */
  private type R = (Q, Set[Q])

  /** A type of pumps of witness. */
  private type Pump = (R, Seq[(A, Set[Q])], R)

  /** Tests whether the SCC is an atom, which is a singleton and does not have a self-loop. */
  private[this] def isAtom(sc: Seq[R]): Boolean =
    sc.size == 1 && !graph.neighbors(sc.head).exists(_._2 == sc.head)

  /** Runs a checker. */
  def check(): Complexity[A] = interrupt(checkExponential() match {
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
    interrupt(scc.iterator.filterNot(isAtom).flatMap(checkExponentialComponent).nextOption())

  /** Finds an EDA structure in the SCC. */
  private[this] def checkExponentialComponent(sc: IndexedSeq[R]): Option[Pump] =
    interrupt {
      val edges = interrupt(sccPairEdges((sc, sc)))

      val found = interrupt(edges.find { case (_, es) => es.size != es.distinct.size })
      found match {
        // The SCC has multi-transitions. In this case, we can find an EDA easily by using it.
        case Some((a, es)) =>
          for {
            (q1, q2) <- es.diff(es.distinct).headOption
            back <- graph.path(Set(q2), q1)
          } yield (q1, a +: back, q1)
        // In other cases, we need to use SCC of a pair graph.
        case None =>
          // A state pair graph with co-transition.
          val g2 = interrupt(Graph.from(for {
            a1 -> es <- edges.toIndexedSeq
            (q11, q12) <- es
            (q21, q22) <- es
          } yield ((q11, q21), a1, (q12, q22))))
          val result = interrupt {
            g2.scc.iterator
              .flatMap { sc =>
                val eda = interrupt(for {
                  // If there is a SCC of g2 contains `(q1, q1)` and `(q2, q3)` (s.t. `q2 != q3`),
                  // then the SCC contains an EDA structure.
                  p1 <- sc.find { case (q1, q2) => q1 == q2 }
                  p2 <- sc.find { case (q1, q2) => q1 != q2 }
                  path1 <- g2.path(Set(p1), p2)
                  path2 <- g2.path(Set(p2), p1)
                } yield (p1._1, path1 ++ path2, p1._1))
                eda
              }
              .nextOption()
          }
          result
      }
    }

  /** Finds an IDA structure chain in the graph. */
  private[this] def checkPolynomial(): (Int, Seq[Pump]) =
    interrupt(scc.map(checkPolynomialComponent).maxByOption(_._1).getOrElse((0, Seq.empty)))

  /** An internal cache of [[checkPolynomialComponent]] method's result. */
  private[this] val checkPolynomialComponentCache = mutable.Map.empty[Seq[R], (Int, Seq[Pump])]

  /** Finds an IDA structure chain from the SCC. */
  private[this] def checkPolynomialComponent(sc: IndexedSeq[R]): (Int, Seq[Pump]) =
    checkPolynomialComponentCache.getOrElseUpdate(
      sc,
      interrupt {
        // Computes the maximum IDA structure chain from neighbors.
        val (maxDegree, maxPumps) = interrupt {
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
          val result = interrupt {
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
          result
        }
      }
    )

  /** Finds an IDA structure between source and target SCCs. */
  private[this] def checkPolynomialComponentBetween(source: IndexedSeq[R], target: IndexedSeq[R]): Option[Pump] = {
    val sourceEdges = interrupt(sccPairEdges((source, source)))
    val between = interrupt(sccReachableMap(source) & sccReverseReachableMap(target))
    val betweenEdges = interrupt(for (sc1 <- between; sc2 <- between) yield sccPairEdges((sc1, sc2)))
    val targetEdges = interrupt(sccPairEdges((target, target)))

    val g3 = interrupt {
      Graph.from(
        (for {
          a <- nfaWLA.alphabet.iterator
          (q11, q12) <- sourceEdges(a)
          (q21, q22) <- betweenEdges.flatMap(_(a))
          (q31, q32) <- targetEdges(a)
        } yield {
          val edge = interrupt(((q11, q21, q31), a, (q12, q22, q32)))
          edge
        }).toIndexedSeq
      )
    }
    val g3back = interrupt {
      Graph.from(
        source.flatMap(q1 => target.map { q2 => ((q1, q2, q2), None, (q1, q1, q2)) }) ++
          g3.edges.map { case (qqq1, a, qqq2) => (qqq1, Some(a), qqq2) }
      )
    }

    val result = interrupt {
      g3back.scc.iterator
        .flatMap { sc =>
          val ida = interrupt {
            // If there is a SCC of `g3back` contains `(q1, q1, q2)` and `(q1, q2, q2)`,
            // then an IDA structure exists between `source` and `target`.
            sc.collect { case (q1, q2, q3) if q1 == q2 && q2 != q3 && sc.contains((q1, q3, q3)) => (q1, q3) }
              .flatMap { case (q1, q2) => g3.path(Set((q1, q1, q2)), (q1, q2, q2)).map((q1, _, q2)) }
          }
          ida
        }
        .nextOption()
    }
    result
  }

  /** Builds a witness object from pump strings and states. */
  private[this] def witness(pumps: Seq[Pump]): Witness[A] = {
    val (pumpPaths, qs) = pumps.foldLeft((Vector.empty[(Seq[A], Seq[A])], nfaWLA.initSet.toSet)) {
      case ((pumpPaths, last), (q1, path, q2)) =>
        val prefix = graph.path(last, q1).get.map(_._1)
        (pumpPaths :+ (prefix, path.map(_._1)), Set(q2))
    }
    val suffix = nfaWLA.lookAheadDFA.toGraph.path(Set(nfaWLA.lookAheadDFA.init), qs.head._2).get.reverse
    Witness(pumpPaths, suffix)
  }
}
