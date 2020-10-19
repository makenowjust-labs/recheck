package codes.quine.labo.redos.automaton

import scala.collection.mutable

/** NFA is a [[https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton NFA (non-deterministic finite state automaton)]] implementation. */
final case class NFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    initSet: Set[Q],
    acceptSet: Set[Q],
    delta: Map[(Q, A), Set[Q]]
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
