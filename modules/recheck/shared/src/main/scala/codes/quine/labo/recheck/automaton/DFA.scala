package codes.quine.labo.recheck
package automaton

import scala.collection.mutable

import data.Graph
import util.GraphvizUtil.escape

/** DFA is a [[https://en.wikipedia.org/wiki/Deterministic_finite_automaton DFA (deterministic finite automaton)]] implementation. */
final case class DFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    init: Q,
    acceptSet: Set[Q],
    delta: Map[(Q, A), Q]
) {

  /** Exports this transition function as a grapg. */
  def toGraph: Graph[Q, A] = Graph.from(delta.iterator.map { case (q1, a) -> q2 => (q1, a, q2) }.toIndexedSeq)

  /** Converts to Graphviz format text. */
  def toGraphviz: String = {
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append(s"  ${escape("")} [shape=point];\n")
    sb.append(s"  ${escape("")} -> ${escape(init)};\n")
    for (q <- stateSet)
      sb.append(s"  ${escape(q)} [shape=${if (acceptSet.contains(q)) "double" else ""}circle];\n")
    for (((q0, a), q1) <- delta)
      sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(a)}];\n")
    sb.append("}")

    sb.result()
  }
}
