package codes.quine.labo.redos
package automaton

import scala.collection.mutable

import data.Graph
import data.MultiSet
import util.GraphvizUtil.escape

/** MultiNFA is a NFA, but each it uses multi-set instead of set
  * for representing non-deterministic transition.
  */
final case class MultiNFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    initSet: MultiSet[Q],
    acceptSet: Set[Q],
    delta: Map[(Q, A), MultiSet[Q]]
) {

  /** Exports this transition function as a grapg. */
  def toGraph: Graph[Q, A] = Graph.from(delta.iterator.flatMap { case (q1, a) -> qs =>
    qs.iterator.map((q1, a, _))
  }.toSeq)

  /** Converts to Graphviz format text. */
  def toGraphviz: String = {
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append(s"  ${escape("")} [shape=point];\n")
    for (init <- initSet) sb.append(s"  ${escape("")} -> ${escape(init)};\n")
    for (q <- stateSet) sb.append(s"  ${escape(q)} [shape=${if (acceptSet.contains(q)) "double" else ""}circle];\n")
    for (((q0, a), qs) <- delta; q1 <- qs)
      sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(a)}];\n")
    sb.append("}")

    sb.result()
  }
}
