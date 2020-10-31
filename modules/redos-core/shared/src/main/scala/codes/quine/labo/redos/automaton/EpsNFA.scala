package codes.quine.labo.redos
package automaton

import scala.collection.mutable

import EpsNFA._
import data.IChar
import data.ICharSet
import util.GraphvizUtil.escape

/** EpsNFA is an ordered ε-NFA on unicode code points. */
final case class EpsNFA[Q](alphabet: ICharSet, stateSet: Set[Q], init: Q, accept: Q, tau: Map[Q, Transition[Q]]) {

  /** Converts to Graphviz format text. */
  def toGraphviz: String = {
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append(s"  ${escape("")} [shape=point];\n")
    sb.append(s"  ${escape("")} -> ${escape(init)};\n")
    for (q0 <- stateSet) {
      tau.get(q0) match {
        case Some(Eps(Seq(q1))) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)};\n")
        case Some(Eps(qs)) =>
          sb.append(s"  ${escape(q0)} [shape=diamond];\n")
          for ((q1, i) <- qs.zipWithIndex)
            sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=$i];\n")
        case Some(Assert(k, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(k)}];\n")
        case Some(Consume(chs, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(chs.mkString("{", ", ", "}"))}];\n")
        case None =>
          sb.append(s"  ${escape(q0)} [shape=doublecircle];\n")
      }
    }
    sb.append("}")

    sb.result()
  }

  /** Converts this ε-NFA to ordered NFA. */
  def toOrderedNFA: OrderedNFA[IChar, (CharInfo, Seq[Q])] = {
    // Skips ε-transition without context infotmation.
    def buildClosure0(q: Q, path: Seq[Q]): Seq[(Q, Seq[Q])] =
      // Exits this loop if a cyclic path is found.
      if (path.lastOption.exists(p => path.containsSlice(Seq(p, q)))) Vector.empty
      else
        tau.get(q) match {
          case Some(Eps(qs))       => qs.flatMap(buildClosure0(_, path :+ q))
          case Some(Assert(_, _))  => Vector((q, path))
          case Some(Consume(_, _)) => Vector((q, path))
          case None                => Vector((q, path))
        }
    val closure0Cache = mutable.Map.empty[Q, Seq[(Q, Seq[Q])]]
    def closure0(q: Q): Seq[(Q, Seq[Q])] = closure0Cache.getOrElseUpdate(q, buildClosure0(q, Vector.empty))

    // Skips ε-transition with context information.
    def buildClosure(c0: CharInfo, c1: CharInfo, q: Q, path: Seq[Q]): Seq[(Q, Option[Consume[Q]])] =
      // Exits this loop if a cyclic path is found.
      if (path.lastOption.exists(p => path.containsSlice(Seq(p, q)))) Vector.empty
      else
        tau.get(q) match {
          case Some(Eps(qs)) => qs.flatMap(buildClosure(c0, c1, _, path :+ q))
          case Some(Assert(k, q1)) =>
            if (k.accepts(c0, c1)) buildClosure(c0, c1, q1, path :+ q) else Vector.empty
          case Some(consume: Consume[Q]) => Vector((q, Some(consume)))
          case None                      => Vector((q, None))
        }
    val closureCache = mutable.Map.empty[(CharInfo, CharInfo, Q, Seq[Q]), Seq[(Q, Option[Consume[Q]])]]
    def closure(c0: CharInfo, c1: CharInfo, q: Q, path: Seq[Q]): Seq[(Q, Option[Consume[Q]])] =
      closureCache.getOrElseUpdate((c0, c1, q, path), buildClosure(c0, c1, q, path))

    val closure0Init = closure0(init)

    val queue = mutable.Queue.empty[(CharInfo, Seq[(Q, Seq[Q])])]
    val newStateSet = mutable.Set.empty[(CharInfo, Seq[Q])]
    val newInits = Vector((CharInfo(true, false), closure0Init.map(_._1)))
    val newAcceptSet = Set.newBuilder[(CharInfo, Seq[Q])]
    val newDelta = Map.newBuilder[((CharInfo, Seq[Q]), IChar), Seq[(CharInfo, Seq[Q])]]

    queue.enqueue((CharInfo(true, false), closure0Init))
    newStateSet.addAll(newInits)

    while (queue.nonEmpty) {
      val (c1, qps) = queue.dequeue()
      val qs = qps.map(_._1)
      val accepts =
        qps.exists { case (q, path) => closure(c1, CharInfo(true, false), q, path).exists(_._1 == accept) }
      if (accepts) newAcceptSet.addOne((c1, qs))
      for (ch <- alphabet.chars) {
        val c2 = CharInfo.from(ch)
        val d = Vector.newBuilder[(CharInfo, Seq[Q])]
        for ((q1, path) <- qps; (_, to) <- closure(c1, c2, q1, path))
          to match {
            case Some(Consume(chs, q2)) if chs.contains(ch) =>
              val qps1 = closure0(q2)
              val qs1 = qps1.map(_._1)
              d.addOne((c2, qs1))
              if (!newStateSet.contains((c2, qs1))) {
                queue.enqueue((c2, qps1))
                newStateSet.addOne((c2, qs1))
              }
            case Some(_) | None =>
              () // Nothing to do here because of terminal state or non-match consuming state.
          }
        newDelta.addOne(((c1, qs), ch) -> d.result())
      }
    }

    OrderedNFA(alphabet.chars.toSet, newStateSet.toSet, newInits, newAcceptSet.result(), newDelta.result())
  }
}

/** EpsNFA types and utilities. */
object EpsNFA {

  /** Transition is a transition of ε-NFA. */
  sealed abstract class Transition[Q] extends Serializable with Product

  /** Eps is an ε-NFA transition without consuming a character. */
  final case class Eps[Q](to: Seq[Q]) extends Transition[Q]

  /** Assert is an ε-NFA transition with consuming no character and assertion. */
  final case class Assert[Q](kind: AssertKind, to: Q) extends Transition[Q]

  /** Eps is an ε-NFA transition with consuming a character. */
  final case class Consume[Q](set: Set[IChar], to: Q) extends Transition[Q]

  /** AssertKind is assertion kind of this ε-NFA transition. */
  sealed abstract class AssertKind extends Serializable with Product {

    /** Tests the assertion on around character informations. */
    def accepts(prev: CharInfo, next: CharInfo): Boolean = this match {
      case AssertKind.LineBegin       => prev.isLineTerminator
      case AssertKind.LineEnd         => next.isLineTerminator
      case AssertKind.WordBoundary    => prev.isWord != next.isWord
      case AssertKind.NotWordBoundary => prev.isWord == next.isWord
    }
  }

  /** AssertKind values and utilities. */
  object AssertKind {

    /** LineBegin is `^` assertion. */
    case object LineBegin extends AssertKind

    /** LineBegin is `$` assertion. */
    case object LineEnd extends AssertKind

    /** LineBegin is `\b` assertion. */
    case object WordBoundary extends AssertKind

    /** LineBegin is `\B` assertion. */
    case object NotWordBoundary extends AssertKind
  }

  /** CharInfo is a minimum character information for assertion check. */
  final case class CharInfo(isLineTerminator: Boolean, isWord: Boolean)

  /** CharInfo utilities. */
  object CharInfo {

    /** Extracts a character information from the interval set. */
    def from(ch: IChar): CharInfo = CharInfo(ch.isLineTerminator, ch.isWord)
  }
}
