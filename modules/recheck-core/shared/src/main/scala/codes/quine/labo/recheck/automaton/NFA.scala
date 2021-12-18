package codes.quine.labo.recheck
package automaton

import scala.collection.mutable

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.util.GraphvizUtil.escape

/** NFA is a
  * [[https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton NFA (non-deterministic finite state automaton)]]
  * implementation.
  */
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
    sb.append(s"  ${escape("")} [shape=point];\n")
    for (init <- initSet) sb.append(s"  ${escape("")} -> ${escape(init)};\n")
    for (q <- stateSet) sb.append(s"  ${escape(q)} [shape=${if (acceptSet.contains(q)) "double" else ""}circle];\n")
    for (((q0, a), qs) <- delta; q1 <- qs)
      sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(a)}];\n")
    sb.append("}")

    sb.result()
  }

  /** Determinizes this NFA. */
  def toDFA(implicit ctx: Context): DFA[A, Set[Q]] =
    ctx.interrupt {
      val queue = mutable.Queue.empty[Set[Q]]
      val newStateSet = mutable.Set.empty[Set[Q]]
      val newAcceptSet = Set.newBuilder[Set[Q]]
      val newDelta = Map.newBuilder[(Set[Q], A), Set[Q]]

      queue.enqueue(initSet)
      newStateSet.add(initSet)

      while (queue.nonEmpty) ctx.interrupt {
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

      val dfa = DFA(alphabet, newStateSet.toSet, initSet, newAcceptSet.result(), newDelta.result())
      ctx.log {
        s"""|automaton: DFA construction
            |     state size: ${dfa.stateSet.size}
            |  alphabet size: ${dfa.alphabet.size}""".stripMargin
      }
      dfa
    }
}
