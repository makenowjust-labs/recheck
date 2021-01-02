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

  /** Converts this NFA's alphabet by the function. */
  def mapAlphabet[B](f: A => B): OrderedNFA[B, Q] =
    OrderedNFA(
      alphabet.map(f),
      stateSet,
      inits,
      acceptSet,
      delta.map { case ((q1, a), q2) => ((q1, f(a)), q2) }
    )

  /** Reverses this NFA.
    *
    * This method loses priorities, so the result type is usual [[NFA]].
    */
  def reverse(implicit timeout: Timeout = Timeout.NoTimeout): NFA[A, Q] =
    timeout.checkTimeout("automaton.OrderedNFA#reverse") {
      val reverseDelta = mutable.Map.empty[(Q, A), Set[Q]].withDefaultValue(Set.empty)
      for ((q1, a) -> qs <- delta; q2 <- qs) timeout.checkTimeout("automaton.OrderedNFA#reverse:loop") {
        reverseDelta((q2, a)) = reverseDelta((q2, a)) | Set(q1)
      }
      NFA(alphabet, stateSet, acceptSet, inits.toSet, reverseDelta.toMap)
    }

  /** Converts this into [[NFAwLA]]. */
  def toNFAwLA(maxNFASize: Int = Int.MaxValue)(implicit timeout: Timeout = Timeout.NoTimeout): NFAwLA[A, Q] =
    timeout.checkTimeout("automaton.OrderedNFA.prune") {
      val reverseDFA = reverse.toDFA
      val reverseDelta = timeout.checkTimeout("automaton.OrderedNFA.prune:reverseDelta") {
        reverseDFA.delta
          .groupMap(_._1._2) { case (p2, _) -> p1 =>
            timeout.checkTimeout("automaton.OrderedNFA.prune:reverseDelta:loop")((p1, p2))
          }
          .withDefaultValue(Vector.empty)
      }

      val newAlphabet = Set.newBuilder[(A, Set[Q])]
      val newStateSet = Set.newBuilder[(Q, Set[Q])]
      val newInits = timeout.checkTimeout("automaton.OrderedNFA.prune:newInits") {
        MultiSet.from(for (q <- inits; p <- reverseDFA.stateSet) yield (q, p))
      }
      val newAcceptSet = timeout.checkTimeout("automaton.OrderedNFA.prune:newAcceptSet") {
        for (q <- acceptSet) yield (q, reverseDFA.init)
      }

      val newDelta =
        mutable.Map.empty[((Q, Set[Q]), (A, Set[Q])), MultiSet[(Q, Set[Q])]].withDefaultValue(MultiSet.empty)
      var deltaSize = 0
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

          newAlphabet.addOne((a, p2))
          newStateSet.addOne((q1, p1)).addAll(qp2s)

          deltaSize += qp2s.size
          if (deltaSize >= maxNFASize) throw new UnsupportedException("MultiNFA size is too large")
        }
      }

      NFAwLA(newAlphabet.result(), newStateSet.result(), newInits, newAcceptSet, newDelta.toMap, reverseDFA)
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
