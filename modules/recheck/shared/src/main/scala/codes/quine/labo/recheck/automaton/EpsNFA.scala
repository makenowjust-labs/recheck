package codes.quine.labo.recheck
package automaton

import scala.collection.mutable

import EpsNFA._
import common.Context
import common.UnsupportedException
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
        case Some(LoopEnter(loop, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"Enter($loop)")}];\n")
        case Some(LoopExit(loop, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"Exit($loop)")}];\n")
        case None =>
          sb.append(s"  ${escape(q0)} [shape=doublecircle];\n")
      }
    }
    sb.append("}")

    sb.result()
  }

  /** Converts this ε-NFA to ordered NFA with ε-elimination. */
  def toOrderedNFA(implicit ctx: Context): OrderedNFA[IChar, (CharInfo, Seq[Q])] = toOrderedNFA(Int.MaxValue)

  /** Converts this ε-NFA to ordered NFA with ε-elimination. */
  def toOrderedNFA(maxNFASize: Int)(implicit ctx: Context): OrderedNFA[IChar, (CharInfo, Seq[Q])] =
    ctx.interrupt {
      // Obtains a set of possible character information.
      // `CharInfo(true, false)` means a beginning or ending marker.
      val charInfoSet = alphabet.chars.toSet.map(CharInfo.from) ++ Set(CharInfo(true, false))

      /** Skips ε-transitions from the state and collects not ε-transition or terminal state. */
      def buildClosure(c0: CharInfo, cs: Set[CharInfo], q: Q, loops: Set[Int]): Seq[(Q, Option[Consume[Q]])] =
        ctx.interrupt {
          if (cs.isEmpty) Vector.empty
          else
            tau.get(q) match {
              case Some(Eps(qs))       => qs.flatMap(buildClosure(c0, cs, _, loops))
              case Some(Assert(k, q1)) => buildClosure(c0, cs & k.toCharInfoSet(c0), q1, loops)
              case Some(LoopEnter(loop, q1)) =>
                if (loops.contains(loop)) Vector.empty // An ε-loop is detected.
                else buildClosure(c0, cs, q1, loops ++ Set(loop))
              case Some(LoopExit(loop, q1)) =>
                if (loops.contains(loop)) Vector.empty // An ε-loop is detected.
                else buildClosure(c0, cs, q1, loops)
              case Some(Consume(chs, q1)) =>
                val newChs = chs.filter(ch => cs.contains(CharInfo.from(ch)))
                if (newChs.nonEmpty) Vector((q, Some(Consume(newChs, q1))))
                else Vector.empty
              case None =>
                if (cs.contains(CharInfo(true, false))) Vector((q, None))
                else Vector.empty
            }
        }
      val closureCache = mutable.Map.empty[(CharInfo, Q), Seq[(Q, Option[Consume[Q]])]]
      def closure(c0: CharInfo, q: Q): Seq[(Q, Option[Consume[Q]])] =
        closureCache.getOrElseUpdate((c0, q), buildClosure(c0, charInfoSet, q, Set.empty))

      val closureInit = closure(CharInfo(true, false), init)

      val queue = mutable.Queue.empty[(CharInfo, Seq[(Q, Option[Consume[Q]])])]
      val newStateSet = mutable.Set.empty[(CharInfo, Seq[Q])]
      val newInits = Vector((CharInfo(true, false), closureInit.map(_._1)))
      val newAcceptSet = Set.newBuilder[(CharInfo, Seq[Q])]
      val newDelta = Map.newBuilder[((CharInfo, Seq[Q]), IChar), Seq[(CharInfo, Seq[Q])]]
      var deltaSize = 0

      queue.enqueue((CharInfo(true, false), closureInit))
      newStateSet.addAll(newInits)

      while (queue.nonEmpty) ctx.interrupt {
        val (c0, qps) = queue.dequeue()
        val qs0 = qps.map(_._1)
        if (qs0.contains(accept)) newAcceptSet.addOne((c0, qs0))
        val d = mutable.Map.empty[IChar, Seq[(CharInfo, Seq[Q])]].withDefaultValue(Vector.empty)
        for ((_, p) <- qps) {
          p match {
            case Some(Consume(chs, q1)) =>
              for (ch <- chs) {
                val c1 = CharInfo.from(ch)
                val qps1 = closure(c1, q1)
                val qs1 = qps1.map(_._1)
                d(ch) = d(ch) :+ (c1, qs1)
                if (!newStateSet.contains((c1, qs1))) {
                  queue.enqueue((c1, qps1))
                  newStateSet.addOne((c1, qs1))
                }
              }
            case None =>
              () // Nothing to do here because of terminal state.
          }
        }
        for ((ch, to) <- d) newDelta.addOne(((c0, qs0), ch) -> to)
        deltaSize += d.size
        if (deltaSize >= maxNFASize) throw new UnsupportedException("OrderedNFA size is too large")
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

  /** Consume is an ε-NFA transition with consuming a character. */
  final case class Consume[Q](set: Set[IChar], to: Q) extends Transition[Q]

  /** LoopEnter is an ε-NFA transition with consuming no character and marking a entering of the loop. */
  final case class LoopEnter[Q](loop: Int, to: Q) extends Transition[Q]

  /** LoopExit is an ε-NFA transition with consuming no character and marking a exiting of the loop. */
  final case class LoopExit[Q](loop: Int, to: Q) extends Transition[Q]

  /** AssertKind is assertion kind of this ε-NFA transition. */
  sealed abstract class AssertKind extends Serializable with Product {

    /** Tests the assertion on the around character information. */
    def accepts(prev: CharInfo, next: CharInfo): Boolean = this match {
      case AssertKind.LineBegin       => prev.isLineTerminator
      case AssertKind.LineEnd         => next.isLineTerminator
      case AssertKind.WordBoundary    => prev.isWord != next.isWord
      case AssertKind.NotWordBoundary => prev.isWord == next.isWord
    }

    /** Returns a set of next possible character information from the previous character information. */
    def toCharInfoSet(prev: CharInfo): Set[CharInfo]
  }

  /** AssertKind values and utilities. */
  object AssertKind {

    /** LineBegin is `^` assertion. */
    case object LineBegin extends AssertKind {
      def toCharInfoSet(prev: CharInfo): Set[CharInfo] =
        if (prev.isLineTerminator) Set(CharInfo(false, false), CharInfo(false, true), CharInfo(true, false))
        else Set.empty
    }

    /** LineEnd is `$` assertion. */
    case object LineEnd extends AssertKind {
      def toCharInfoSet(prev: CharInfo): Set[CharInfo] = Set(CharInfo(true, false))
    }

    /** WordBoundary is `\b` assertion. */
    case object WordBoundary extends AssertKind {
      def toCharInfoSet(prev: CharInfo): Set[CharInfo] =
        if (prev.isWord) Set(CharInfo(false, false), CharInfo(true, false))
        else Set(CharInfo(false, true))
    }

    /** NotWordBoundary is `\B` assertion. */
    case object NotWordBoundary extends AssertKind {
      def toCharInfoSet(prev: CharInfo): Set[CharInfo] =
        if (prev.isWord) Set(CharInfo(false, true))
        else Set(CharInfo(false, false), CharInfo(true, false))
    }
  }

  /** CharInfo is a minimum character information for assertion check.
    *
    * Note that both of `isLineTerminator` and `isWord` must not be `true` at the same time
    * because there is no character which is a line terminator and also a word.
    */
  final case class CharInfo(isLineTerminator: Boolean, isWord: Boolean)

  /** CharInfo utilities. */
  object CharInfo {

    /** Extracts a character information from the interval set. */
    def from(ch: IChar): CharInfo = CharInfo(ch.isLineTerminator, ch.isWord)
  }
}
