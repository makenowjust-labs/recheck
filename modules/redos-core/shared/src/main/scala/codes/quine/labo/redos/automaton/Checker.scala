package codes.quine.labo.redos
package automaton

import scala.collection.mutable
import scala.util.Try

import Complexity._
import automaton._
import data.Graph
import data.IChar
import util.Timeout

/** ReDoS vulnerable RegExp checker. */
object Checker {

  /** Checks a match time complexity of the ε-NFA. */
  def check[Q](epsNFA: EpsNFA[Q])(implicit timeout: Timeout): Try[Complexity[IChar]] =
    Try(new Checker(epsNFA, timeout).check())
}

/** Checker is a ReDoS vulnerable RegExp checker. */
private final class Checker[Q](
    private[this] val epsNFA: EpsNFA[Q],
    private[this] val timeout: Timeout
) {

  // Introduces `timeout` methods into the scope.
  import timeout._

  /** An ordered NFA constructed from [[epsNFA]]. */
  private[this] val nfa = checkTimeoutWith("nfa")(epsNFA.toOrderedNFA.rename)

  /** A reversed DFA constructed from [[orderedNFA]]. */
  private[this] val reverseDFA = checkTimeoutWith("reverseDFA")(nfa.reverse.toDFA)

  /** A NFA with multi-transitions constructed from [[orderedNFA]] (and [[reverseDFA]]). */
  private[this] val multiNFA = checkTimeoutWith("multiNFA")(nfa.toMultiNFA)

  /** A [[multiNFA]] transition graph. */
  private[this] val graph = checkTimeoutWith("graph")(multiNFA.toGraph.reachable(multiNFA.initSet.toSet))

  /** A [[multiNFA]] transition graph's SCCs. */
  private[this] val scc = checkTimeoutWith("scc")(graph.scc)

  /** A map from a state of [[multiNFA]] to SCC. */
  private[this] val sccMap = (for (sc <- scc; q <- sc) yield q -> sc).toMap

  /** A graph with transitions between SCCs. */
  private[this] val sccGraph = Graph.from(
    graph.edges
      .map { case (q1, _, q2) => (sccMap(q1), (), sccMap(q2)) }
      .filter { case (sc1, _, sc2) => sc1 != sc2 }
      .distinct
  )

  /** A map from SCC to reachable SCCs. */
  private[this] val sccReachableMap = sccGraph.reachableMap

  /** A map from SCC to reachable SCCs on reversed [[sccGraph]]. */
  private[this] val sccReverseReachableMap = sccGraph.reverse.reachableMap

  /** A map from a pair of SCCs to edges (transitions) between two SCCs. */
  private[this] val sccPairEdges = graph.edges
    .groupMap { case (q1, _, q2) => (sccMap(q1), sccMap(q2)) } { case (q1, a, q2) => a -> (q1, q2) }
    .view
    .mapValues(_.groupMap(_._1)(_._2).withDefaultValue(Seq.empty))
    .toMap
    .withDefaultValue(Map.empty.withDefaultValue(Seq.empty))

  /** A type of [[multiNFA]] state. */
  private type Q = (Int, Set[Int])

  /** A type of pumps of witness. */
  private type Pump = (Q, Seq[IChar], Q)

  /** Tests whether the SCC is an atom, which is a singleton and does not have a self-loop. */
  private[this] def isAtom(sc: Seq[Q]): Boolean =
    sc.size == 1 && !graph.neighbors(sc.head).exists(_._2 == sc.head)

  /** Runs a checker. */
  def check(): Complexity[IChar] =
    checkExponential() match {
      case Some(pump) => Exponential(witness(Seq(pump)))
      case None =>
        checkPolynomial() match {
          case (0, _)          => Constant
          case (1, _)          => Linear
          case (degree, pumps) => Polynomial(degree, witness(pumps))
        }
    }

  /** Finds an EDA structure in the graph. */
  private[this] def checkExponential(): Option[Pump] =
    checkTimeoutWith("checkExponential") {
      scc.iterator.filterNot(isAtom(_)).flatMap(checkExponentialComponent(_)).nextOption()
    }

  /** Finds an EDA structure in the SCC. */
  private[this] def checkExponentialComponent(sc: Seq[Q]): Option[Pump] = {
    checkTimeout("checkExponentialComponent: edges")
    val edges = sccPairEdges((sc, sc))

    checkTimeout("checkExponentialComponent: multi-transitions")
    edges.find { case (_, es) => es.size != es.distinct.size } match {
      // The SCC has multi-transitions. In this case, we can find an EDA easily by using it.
      case Some((a, es)) =>
        for {
          (q1, q2) <- es.diff(es.distinct).headOption
          back <- graph.path(Set(q2), q1)
        } yield (q1, a +: back, q1)
      // In other cases, we need to use SCC of a pair graph.
      case None =>
        // A state pair graph with co-transition.
        checkTimeout("checkExponentialComponent: G2")
        val g2 = Graph.from(for {
          a1 -> es <- edges.toSeq
          (q11, q12) <- es
          (q21, q22) <- es
        } yield ((q11, q21), a1, (q12, q22)))
        checkTimeout("checkExponentialComponent: EDA")
        g2.scc.iterator
          .flatMap { sc =>
            checkTimeout("checkExponentialComponent: EDA loop")
            for {
              // If there is a SCC of g2 contains `(q1, q1)` and `(q2, q3)` (s.t. `q2 != q3`),
              // then the SCC contains an EDA structure.
              p1 <- sc.find { case (q1, q2) => q1 == q2 }
              p2 <- sc.find { case (q1, q2) => q1 != q2 }
              path1 <- g2.path(Set(p2), p1)
              path2 <- g2.path(Set(p1), p2)
            } yield (p1._1, path1 ++ path2, p1._1)
          }
          .nextOption()
    }
  }

  /** Finds an IDA structure chain in the graph. */
  private[this] def checkPolynomial(): (Int, Seq[Pump]) =
    checkTimeoutWith("checkPolynomial") {
      scc.map(checkPolynomialComponent(_)).maxBy(_._1)
    }

  /** An internal cache of [[checkPolynomialComponent]] method's result. */
  private[this] val checkPolynomialComponentCache = mutable.Map.empty[Seq[Q], (Int, Seq[Pump])]

  /** Finds an IDA structure chain from the SCC. */
  private[this] def checkPolynomialComponent(sc: Seq[Q]): (Int, Seq[Pump]) =
    checkPolynomialComponentCache.getOrElseUpdate(
      sc, {
        // Computes the maximum IDA structure chain from neighbors.
        checkTimeout("checkPolynomialComponent: maximum IDA")
        val (maxDegree, maxPumps) =
          sccGraph
            .neighbors(sc)
            .map { case ((), sc) => checkPolynomialComponent(sc) }
            .maxByOption(_._1)
            .getOrElse((0, Seq.empty))
        if (maxDegree == 0) (if (isAtom(sc)) 0 else 1, Seq.empty)
        else if (isAtom(sc)) (maxDegree, maxPumps)
        else {
          // Appends an IDA structure between the SCC and the maximum chain's source into the chain.
          checkTimeout("checkPolynomialComponent: append IDA")
          sccReachableMap(sc).iterator
            .filter(target => sc != target && !isAtom(target) && checkPolynomialComponent(target)._1 == maxDegree)
            .flatMap(target => checkPolynomialComponentBetween(sc, target).map((target, _)))
            .map { case (target, pump) => (maxDegree + 1, pump +: checkPolynomialComponent(target)._2) }
            .nextOption()
            .getOrElse((maxDegree, maxPumps))
        }
      }
    )

  /** Finds an IDA structure between source and target SCCs. */
  private[this] def checkPolynomialComponentBetween(source: Seq[Q], target: Seq[Q]): Option[Pump] = {
    checkTimeout("checkPolynomialComponent: source edges")
    val sourceEdges = sccPairEdges((source, source))
    checkTimeout("checkPolynomialComponent: between")
    val between = sccReachableMap(source) & sccReverseReachableMap(target)
    checkTimeout("checkPolynomialComponent: between edges")
    val betweenEdges = for (sc1 <- between; sc2 <- between) yield sccPairEdges((sc1, sc2))
    checkTimeout("checkPolynomialComponent: target edges")
    val targetEdges = sccPairEdges((target, target))

    checkTimeout("checkPolynomialComponent: G3")
    val g3 = Graph.from(
      (for {
        a <- multiNFA.alphabet.iterator
        (q11, q12) <- sourceEdges(a)
        (q21, q22) <- betweenEdges.flatMap(_(a))
        (q31, q32) <- targetEdges(a)
      } yield ((q11, q21, q31), a, (q12, q22, q32))).toSeq
    )
    checkTimeout("checkPolynomialComponent: G3 with back edges")
    val g3back = Graph.from(
      source.flatMap(q1 => target.map { q2 => ((q1, q2, q2), None, (q1, q1, q2)) }) ++
        g3.edges.map { case (qqq1, a, qqq2) => (qqq1, Some(a), qqq2) }
    )

    checkTimeout("checkPolynomialComponent: IDA")
    g3back.scc.iterator
      .flatMap { sc =>
        checkTimeout("checkPolynomialComponent: IDA loop")
        // If there is a SCC of `g3back` contains `(q1, q1, q2)` and `(q1, q2, q2)`,
        // then an IDA structure exists between `source` and `target`.
        sc.collect { case (q1, q2, q3) if q1 == q2 && q2 != q3 && sc.contains((q1, q3, q3)) => (q1, q3) }
          .flatMap { case (q1, q2) => g3.path(Set((q1, q1, q2)), (q1, q2, q2)).map((q1, _, q2)) }
      }
      .nextOption()
  }

  /** Builds a witness object from pump strings and states. */
  private[this] def witness(pumps: Seq[Pump]): Witness[IChar] = {
    val (pumpPaths, qs) = pumps.foldLeft((Seq.empty[(Seq[IChar], Seq[IChar])], multiNFA.initSet.toSet)) {
      case ((pumpPaths, last), (q1, path, q2)) =>
        val prefix = graph.path(last, q1).get
        (pumpPaths :+ (prefix, path), Set(q2))
    }
    val suffix = reverseDFA.toGraph.path(Set(reverseDFA.init), qs.head._2).get.reverse
    Witness(pumpPaths, suffix)
  }
}
