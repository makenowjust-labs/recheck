package codes.quine.labo.redos.automaton

import scala.collection.MultiSet
import scala.collection.mutable

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

  /** Converts to Graphviz format text. */
  def toGraphviz: String = {
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append("  \"\" [shape=point];\n")
    for (init <- initSet) sb.append("  \"\" -> " ++ init.toString ++ ";\n")
    for (q <- stateSet) sb.append(s"  $q [shape=${if (acceptSet.contains(q)) "double" else ""}circle];\n")
    for (((q0, a), qs) <- delta; q1 <- qs)
      sb.append("  " ++ q0.toString ++ " -> " ++ q1.toString ++ " [label=\"" ++ a.toString ++ "\"];\n")
    sb.append("}")

    sb.result()
  }
}
