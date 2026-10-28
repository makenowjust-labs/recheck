package codes.quine.labs.recheck
package automaton

import scala.collection.mutable

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.data.Graph
import codes.quine.labs.recheck.data.MultiSet
import codes.quine.labs.recheck.util.GraphvizUtil.escape
import codes.quine.labs.resyntax.ast.SourceLocation

/** NFAwLA is an NFA with a look-ahead DFA (see [[OrderedNFA.toLookAheadDFA]]).
  *
  * This NFA uses a multi-set instead of a set for duplicated transitions. A state of this NFA is a pair of the original
  * NFA state and the look-ahead DFA state `P`, and the transition function must be pruned along with the backtrack
  * behavior.
  */
final case class NFAwLA[A, Q, P](
    alphabet: Set[(A, P)],
    stateSet: Set[(Q, P)],
    initSet: MultiSet[(Q, P)],
    acceptSet: Set[(Q, P)],
    delta: Map[((Q, P), (A, P)), MultiSet[(Q, P)]],
    lookAheadDFA: DFA[A, P],
    sourcemap: Map[((Q, P), (A, P), (Q, P)), Seq[SourceLocation]] =
      Map.empty[((Q, P), (A, P), (Q, P)), Seq[SourceLocation]]
):

  /** Exports this transition function as a graph. */
  def toGraph(using ctx: Context): Graph[(Q, P), (A, P)] =
    ctx.interrupt:
      Graph.from(delta.iterator.flatMap { case (q1, a) -> qs =>
        qs.iterator.map((q1, a, _))
      }.toIndexedSeq)

  /** Converts to Graphviz format text. */
  def toGraphviz: String =
    val sb = new mutable.StringBuilder

    def showQ(qp: (Q, P)): String = escape(s"(${qp._1}, ${qp._2})")

    sb.append("digraph {\n")
    sb.append(s"  ${escape("")} [shape=point];\n")
    for init <- initSet do sb.append(s"  ${escape("")} -> ${showQ(init)};\n")
    for q <- stateSet do sb.append(s"  ${showQ(q)} [shape=${if acceptSet.contains(q) then "double" else ""}circle];\n")
    for ((q0, a), qs) <- delta; q1 <- qs do sb.append(s"  ${showQ(q0)} -> ${showQ(q1)} [label=${escape(a._1)}];\n")
    sb.append("}")

    sb.result()
