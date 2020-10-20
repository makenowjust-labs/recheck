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

  /** Determinizes this NFA. */
  def toDFA: DFA[A, Set[Q]] = {
    val queue = mutable.Queue.empty[Set[Q]]
    val newStateSet = mutable.Set.empty[Set[Q]]
    val newAcceptSet = Set.newBuilder[Set[Q]]
    val newDelta = Map.newBuilder[(Set[Q], A), Set[Q]]

    queue.enqueue(initSet)
    newStateSet.add(initSet)

    while (queue.nonEmpty) {
      val qs = queue.dequeue()
      if ((qs & acceptSet).nonEmpty) {
        newAcceptSet.addOne(qs)
      }
      for (a <- alphabet) {
        val qs2 = qs.flatMap(q => delta.getOrElse((q, a), Set.empty))
        newDelta.addOne((qs, a) -> qs2)
        if (!newStateSet.contains(qs2)) {
          queue.enqueue(qs2)
          newStateSet.add(qs2)
        }
      }
    }

    DFA(alphabet, newStateSet.toSet, initSet, newAcceptSet.result(), newDelta.result())
  }
}
