package codes.quine.labo.redos.automaton

import scala.collection.mutable

/** DFA is a [[https://en.wikipedia.org/wiki/Deterministic_finite_automaton DFA (deterministic finite automaton)]] implementation. */
final case class DFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    init: Q,
    acceptSet: Set[Q],
    delta: Map[(Q, A), Q]
) {

  /** Converts to Graphviz format text. */
  def toGraphviz: String = {
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append("  \"\" [shape=point];\n")
    sb.append("  \"\" -> " ++ init.toString ++ ";\n")
    for (q <- stateSet) sb.append(s"  $q [shape=${if (acceptSet.contains(q)) "double" else ""}circle];\n")
    for (((q0, a), q1) <- delta)
      sb.append("  " ++ q0.toString ++ " -> " ++ q1.toString ++ " [label=\"" ++ a.toString ++ "\"];\n")
    sb.append("}")

    sb.result()
  }
}
