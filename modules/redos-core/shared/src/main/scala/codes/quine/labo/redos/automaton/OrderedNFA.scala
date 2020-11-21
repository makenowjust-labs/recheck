package codes.quine.labo.redos
package automaton

import scala.collection.mutable

import data.MultiSet
import util.Timeout
import util.GraphvizUtil.escape

/** OrderedNFA is a NFA, but its transitions are priorized in the order. */
final case class OrderedNFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    inits: Seq[Q],
    acceptSet: Set[Q],
    delta: Map[(Q, A), Seq[Q]]
) {

  /** Renames its states as integers. */
  def rename: OrderedNFA[A, Int] = {
    val f = stateSet.zipWithIndex.toMap
    OrderedNFA(
      alphabet,
      f.values.toSet,
      inits.map(f),
      acceptSet.map(f),
      delta.map { case (q1, a) -> qs => (f(q1), a) -> qs.map(f) }
    )
  }

  /** Reverses this NFA.
    *
    * This method loses priorities, so the result type is usual [[NFA]].
    */
  def reverse(implicit timeout: Timeout = Timeout.NoTimeout): NFA[A, Q] =
    timeout.checkTimeout("automaton.OrderedNFA#reverse") {
      val reverseDelta = mutable.Map.empty[(Q, A), Set[Q]].withDefaultValue(Set.empty)
      for ((q1, a) -> qs <- delta; q2 <- qs) {
        reverseDelta((q2, a)) = reverseDelta((q2, a)) | Set(q1)
      }
      NFA(alphabet, stateSet, acceptSet, inits.toSet, reverseDelta.toMap)
    }

  /** Converts to Graphviz format text. */
  def toGraphviz: String = {
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append(s"  ${escape("")} [shape=point];\n")
    for ((init, i) <- inits.zipWithIndex) sb.append(s"  ${escape("")} -> ${escape(init)} [label=$i];\n")
    for (q <- stateSet) sb.append(s"  ${escape(q)} [shape=${if (acceptSet.contains(q)) "double" else ""}circle];\n")
    for (((q0, a), qs) <- delta; (q1, i) <- qs.zipWithIndex)
      sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"${i}, ${a}")}];\n")
    sb.append("}")

    sb.result()
  }
}

/** OrderedNFA utilities. */
object OrderedNFA {

  /** Prunes a transition function along with backtracking behavior for vulnerability detection.
    *
    * A result is pair of a reversed DFA and pruned NFA.
    */
  def prune[A, Q](nfa: OrderedNFA[A, Q])(implicit
      timeout: Timeout = Timeout.NoTimeout
  ): (DFA[A, Set[Q]], MultiNFA[(A, Set[Q]), (Q, Set[Q])]) = timeout.checkTimeout("automaton.OrderedNFA.prune") {
    val OrderedNFA(alphabet, stateSet, inits, acceptSet, delta) = nfa

    val reverseDFA = nfa.reverse.toDFA
    val reverseDelta =
      reverseDFA.delta.groupMap(_._1._2) { case (p2, _) -> p1 => (p1, p2) }.withDefaultValue(Vector.empty)

    val newAlphabet = for (a <- alphabet; p <- reverseDFA.stateSet) yield (a, p)
    val newStateSet = for (q <- stateSet; p <- reverseDFA.stateSet) yield (q, p)
    val newInits = MultiSet.from(for (q <- inits; p <- reverseDFA.stateSet) yield (q, p))
    val newAcceptSet = for (q <- acceptSet) yield (q, reverseDFA.init)

    val newDelta = mutable.Map.empty[((Q, Set[Q]), (A, Set[Q])), MultiSet[(Q, Set[Q])]].withDefaultValue(MultiSet.empty)
    for ((q1, a) -> qs <- delta) timeout.checkTimeout("automaton.OrderedNFA.prune:loop") {
      for ((p1, p2) <- reverseDelta(a)) {
        // There is a transition `q1 --(a)-> qs` in ordered NFA, and
        // there is a transition `p1 <-(a)-- p2` in reversed DFA.
        // The result NFA contains a transition `(q1, p1) --(a)-> (qs(i), p2)`
        // if and only if there is no `qs(j)` (`j < i`) in `p2`.
        val qp2s = qs
          .scanLeft(false)(_ || p2.contains(_))
          .zip(qs)
          .takeWhile(!_._1)
          .map { case (_, q2) => (q2, p2) }
        newDelta(((q1, p1), (a, p2))) = newDelta(((q1, p1), (a, p2))) ++ MultiSet.from(qp2s)
      }
    }

    val multiNFA = MultiNFA[(A, Set[Q]), (Q, Set[Q])](newAlphabet, newStateSet, newInits, newAcceptSet, newDelta.toMap)
    (reverseDFA, multiNFA)
  }
}
