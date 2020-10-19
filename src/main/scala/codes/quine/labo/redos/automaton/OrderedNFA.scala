package codes.quine.labo.redos.automaton

import scala.collection.mutable

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
  def reverse: NFA[A, Q] = {
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
    sb.append("  \"\" [shape=point];\n")
    for ((init, i) <- inits.zipWithIndex) sb.append("  \"\" -> " ++ init.toString ++ s" [label=$i];\n")
    for (q <- stateSet) sb.append(s"  $q [shape=${if (acceptSet.contains(q)) "double" else ""}circle];\n")
    for (((q0, a), qs) <- delta; (q1, i) <- qs.zipWithIndex)
      sb.append(
        "  " ++ q0.toString ++ " -> " ++ q1.toString ++ " [label=\"" ++ i.toString ++ ", " ++ a.toString ++ "\"];\n"
      )
    sb.append("}")

    sb.result()
  }
}
