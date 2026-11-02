package codes.quine.labs.recheck
package automaton

import scala.collection.mutable

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.util.ECMAScriptNotation
import codes.quine.labs.recheck.util.GraphvizUtil.escape
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.alphabet.CharKind
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** EpsNFA is an ordered ε-NFA on unicode code points. */
final case class EpsNFA[Q](alphabet: PatternAlphabet, stateSet: Set[Q], init: Q, accept: Q, tau: Map[Q, Transition[Q]]):

  /** Converts to Graphviz format text. */
  def toGraphviz: String =
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append(s"  ${escape("")} [shape=point];\n")
    sb.append(s"  ${escape("")} -> ${escape(init)};\n")
    for q0 <- stateSet do
      tau.get(q0) match
        case Some(Eps(Seq(q1))) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)};\n")
        case Some(Eps(qs)) =>
          sb.append(s"  ${escape(q0)} [shape=diamond];\n")
          for (q1, i) <- qs.zipWithIndex do sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=$i];\n")
        case Some(Assert(k, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(k)}];\n")
        case Some(Consume(chs, q1, _)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(
            s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(chs.map((c, k) => s"(${ECMAScriptNotation.showClass(c)},$k)").mkString("{", ", ", "}"))}];\n"
          )
        case Some(LoopEnter(loop, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"Enter($loop)")}];\n")
        case Some(LoopExit(loop, q1)) =>
          sb.append(s"  ${escape(q0)} [shape=circle];\n")
          sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"Exit($loop)")}];\n")
        case None =>
          sb.append(s"  ${escape(q0)} [shape=doublecircle];\n")
    sb.append("}")

    sb.result()

  /** Converts this ε-NFA to ordered NFA with ε-elimination. */
  def toOrderedNFA(using ctx: Context): OrderedNFA[CharSet, (CharKindState, Seq[(Q, Set[CharKindState])])] =
    toOrderedNFA(Int.MaxValue)

  /** Converts this ε-NFA to ordered NFA with ε-elimination.
    *
    * Its state is a [[CharKindState]] and the ε-NFA's states, each with the next states that its assertions allow.
    */
  def toOrderedNFA(maxNFASize: Int)(using
      ctx: Context
  ): OrderedNFA[CharSet, (CharKindState, Seq[(Q, Set[CharKindState])])] =
    ctx.interrupt:
      val kinds = tau.values.collect { case Assert(k, _) => k }.toSet
      val afterCRLF = kinds.exists:
        case AssertKind.LineBegin(_, _, afterCRLF, _) => afterCRLF
        case _                                        => false
      val last = kinds.exists(_.isInstanceOf[AssertKind.ChompTextEnd])
      val follow = kinds.exists:
        case AssertKind.LineEnd(_, beforeCRLF, _) => beforeCRLF
        case AssertKind.ChompTextEnd(_, crlf, _)  => crlf
        case _                                    => false
      val marks = kinds.exists:
        case _: AssertKind.MarkWordBoundary | _: AssertKind.NotMarkWordBoundary => true
        case _                                                                  => false
      val baseKinds = alphabet.pairs.iterator.map(_._2).toSet

      // The begin and the end of the input are distinguished only for the assertions looking at them, and they are
      // line terminators if the assertions see no difference, which keeps the automaton smaller.
      val inputTerminatorKind =
        if afterCRLF || last || follow || marks then CharKind.Boundary
        else if !kinds.exists(_.seesBoundary) then CharKind.Normal
        else if kinds.forall(_.treatsBoundaryAsLineTerminator(baseKinds)) then CharKind.LineTerminator
        else CharKind.Boundary
      val inputTerminator = CharKindState(inputTerminatorKind)

      def advance(s: CharKindState, k: CharKind): Seq[CharKindState] = s.advance(k, afterCRLF, last, follow)

      val nextStatesCache = mutable.Map.empty[CharKindState, Set[CharKindState]]
      def nextStates(s: CharKindState): Set[CharKindState] =
        nextStatesCache.getOrElseUpdate(s, (baseKinds.flatMap(advance(s, _)) + inputTerminator).filter(s.admits))

      type P = (Q, Set[CharKindState])

      type Step = (Set[(CharSet, CharKindState)], Q, Option[SourceLocation])

      /** Skips ε-transitions from the state and collects not ε-transition or terminal state. */
      def buildClosure(s0: CharKindState, ss: Set[CharKindState], q: Q, loops: Set[Int]): Seq[(P, Option[Step])] =
        ctx.interrupt:
          if ss.isEmpty then Vector.empty
          else
            tau.get(q) match
              case Some(Eps(qs))             => qs.flatMap(buildClosure(s0, ss, _, loops))
              case Some(Assert(k, q1))       => buildClosure(s0, ss.filter(k.accepts(s0, _)), q1, loops)
              case Some(LoopEnter(loop, q1)) =>
                if loops.contains(loop) then Vector.empty // An ε-loop is detected.
                else buildClosure(s0, ss, q1, loops ++ Set(loop))
              case Some(LoopExit(loop, q1)) =>
                if loops.contains(loop) then Vector.empty // An ε-loop is detected.
                else buildClosure(s0, ss, q1, loops)
              case Some(Consume(chs, q1, loc)) =>
                val next = chs.flatMap((ch, k) => advance(s0, k).filter(ss.contains).map((ch, _)))
                if next.nonEmpty then Vector(((q, ss), Some((next, q1, loc))))
                else Vector.empty
              case None =>
                if ss.contains(inputTerminator) then Vector(((q, Set.empty), None))
                else Vector.empty

      val closureCache = mutable.Map.empty[(CharKindState, Q), Seq[(P, Option[Step])]]
      def closure(s0: CharKindState, q: Q): Seq[(P, Option[Step])] =
        closureCache.getOrElseUpdate((s0, q), buildClosure(s0, nextStates(s0), q, Set.empty))

      val closureInit = closure(inputTerminator, init)

      val queue = mutable.Queue.empty[(CharKindState, Seq[(P, Option[Step])])]
      val newStateSet = mutable.Set.empty[(CharKindState, Seq[P])]
      val newInits = Vector((inputTerminator, closureInit.map(_._1)))
      val newAcceptSet = Set.newBuilder[(CharKindState, Seq[P])]
      val newDelta = Map.newBuilder[((CharKindState, Seq[P]), CharSet), Seq[(CharKindState, Seq[P])]]
      val newSourcemap = mutable.Map
        .empty[((CharKindState, Seq[P]), CharSet, (CharKindState, Seq[P])), Seq[SourceLocation]]
        .withDefaultValue(Vector.empty)
      var deltaSize = 0

      queue.enqueue((inputTerminator, closureInit))
      newStateSet.addAll(newInits)

      while queue.nonEmpty do
        ctx.interrupt:
          val (s0, qps) = queue.dequeue()
          val qs0 = qps.map(_._1)
          if qs0.map(_._1).contains(accept) then newAcceptSet.addOne((s0, qs0))
          val d = mutable.Map.empty[CharSet, Seq[(CharKindState, Seq[P])]].withDefaultValue(Vector.empty)
          for (_, step) <- qps; (chs, q1, loc) <- step; (ch, s) <- chs do
            val s1 = s.forgetMarks
            val qps1 = closure(s1, q1)
            val qs1 = qps1.map(_._1)
            d(ch) = d(ch) :+ (s1, qs1)
            newSourcemap(((s0, qs0), ch, (s1, qs1))) ++= loc
            if !newStateSet.contains((s1, qs1)) then
              queue.enqueue((s1, qps1))
              newStateSet.addOne((s1, qs1))
          for (ch, to) <- d do newDelta.addOne(((s0, qs0), ch) -> to)
          deltaSize += d.size
          if deltaSize >= maxNFASize then
            ctx.log("auto: exceed maxNFASize on OrderedNFA construction")
            throw new UnsupportedException("OrderedNFA size is too large")

      val orderedNFA = OrderedNFA(
        alphabet.any.map(_._1),
        newStateSet.toSet,
        newInits,
        newAcceptSet.result(),
        newDelta.result(),
        newSourcemap.toMap.filter(_._2.nonEmpty)
      )
      ctx.log:
        s"""|automaton: OrderedNFA construction
            |     state size: ${orderedNFA.stateSet.size}
            |  alphabet size: ${orderedNFA.alphabet.size}""".stripMargin
      orderedNFA

/** EpsNFA types and utilities. */
object EpsNFA:

  /** Transition is a transition of ε-NFA. */
  sealed abstract class Transition[Q] extends Serializable with Product

  /** Eps is an ε-NFA transition without consuming a character. */
  final case class Eps[Q](to: Seq[Q]) extends Transition[Q]

  /** Assert is an ε-NFA transition with consuming no character and assertion. */
  final case class Assert[Q](kind: AssertKind, to: Q) extends Transition[Q]

  /** Consume is an ε-NFA transition with consuming a character. */
  final case class Consume[Q](set: Set[(CharSet, CharKind)], to: Q, loc: Option[SourceLocation] = None)
      extends Transition[Q]

  /** LoopEnter is an ε-NFA transition with consuming no character and marking a entering of the loop. */
  final case class LoopEnter[Q](loop: Int, to: Q) extends Transition[Q]

  /** LoopExit is an ε-NFA transition with consuming no character and marking a exiting of the loop. */
  final case class LoopExit[Q](loop: Int, to: Q) extends Transition[Q]

  /** AssertKind is assertion kind of this ε-NFA transition. */
  sealed abstract class AssertKind extends Serializable with Product:

    /** Tests the assertion at a position between the states before and after the character there.
      *
      * The state after is that of the end of the input at the end.
      */
    def accepts(prev: CharKindState, next: CharKindState): Boolean

    /** Whether the assertion distinguishes the begin and the end of the input from the characters. */
    def seesBoundary: Boolean = true

    /** Whether the assertion tests the begin and the end of the input as a line terminator among the kinds. */
    def treatsBoundaryAsLineTerminator(kinds: Iterable[CharKind]): Boolean =
      val (b, t) = (CharKindState(CharKind.Boundary), CharKindState(CharKind.LineTerminator))
      (kinds.map(CharKindState(_)) ++ Seq(b, t)).forall: s =>
        accepts(b, s) == accepts(t, s) && accepts(s, b) == accepts(s, t)

  /** AssertKind values. */
  object AssertKind:

    private def betweenCRLF(prev: CharKindState, next: CharKindState): Boolean = prev.kind.cr && next.kind.lf

    /** LineBegin is `^` assertion with the `set`-th line terminator set, which `atEnd` tells where it matches at the
      * end of the input.
      *
      * With `afterCRLF`, it matches after `\r\n` too, where `\n` is not a line terminator by itself (PCRE's `(*CRLF)`).
      * With `atomic`, it does not match between `\r` and `\n` (Java).
      */
    final case class LineBegin(set: Int, atEnd: IRLineBeginAtEnd, afterCRLF: Boolean, atomic: Boolean)
        extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        val afterTerminator = prev.kind.isLineTerminatorOf(set) || (afterCRLF && prev.afterCR)
        (prev.kind.boundary || afterTerminator) && !(atomic && betweenCRLF(prev, next)) &&
        (!next.kind.boundary || (atEnd match
          case IRLineBeginAtEnd.AfterTerminator => true
          case IRLineBeginAtEnd.InputBegin      => prev.kind.boundary
          case IRLineBeginAtEnd.Never           => false))

    /** LineEnd is `$` assertion with the `set`-th line terminator set.
      *
      * With `beforeCRLF`, it matches before `\r\n` too, where `\r` is not a line terminator by itself (PCRE's
      * `(*CRLF)`). With `atomic`, it does not match between `\r` and `\n` (Java).
      */
    final case class LineEnd(set: Int, beforeCRLF: Boolean, atomic: Boolean) extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        val beforeTerminator =
          next.kind.isLineTerminatorOf(set) || (beforeCRLF && next.kind.cr && !next.follow.contains(CRFollow.Other))
        (next.kind.boundary || beforeTerminator) && !(atomic && betweenCRLF(prev, next))

    /** TextBegin is `\A` assertion. */
    case object TextBegin extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean = prev.kind.boundary

    /** TextEnd is `\z` assertion. */
    case object TextEnd extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean = next.kind.boundary

    /** ChompTextEnd is `\Z` assertion with the `set`-th line terminator set, which matches at the end or before a line
      * terminator at the end.
      *
      * With `crlf`, it matches before `\r\n` at the end too. With `atomic`, it does not match between `\r` and `\n`
      * (Java).
      */
    final case class ChompTextEnd(set: Int, crlf: Boolean, atomic: Boolean) extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        val beforeFinal = (next.kind.isLineTerminatorOf(set) && next.last.contains(true)) ||
          (crlf && next.kind.cr && next.follow.contains(CRFollow.FinalLF))
        (next.kind.boundary || beforeFinal) && !(atomic && betweenCRLF(prev, next))

    /** WordBoundary is `\b` assertion of the `set`-th word set. */
    final case class WordBoundary(set: Int) extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        prev.kind.isWordOf(set) != next.kind.isWordOf(set)

      override def seesBoundary: Boolean = false

    /** NotWordBoundary is `\B` assertion of the `set`-th word set. */
    final case class NotWordBoundary(set: Int) extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        prev.kind.isWordOf(set) == next.kind.isWordOf(set)

      override def seesBoundary: Boolean = false

    /** Tests whether the character before a position is a word character of the `word`-th word set, or an extender of
      * the `marks`-th word marks after a base.
      */
    private def isMarkWordBefore(prev: CharKindState, word: Int, marks: Int): Boolean =
      prev.kind.isWordOf(word) || prev.kind.isExtenderOf(marks) && prev.isBasedOf(marks)

    /** Tests whether the character after a position is a word character of the `word`-th word set, or a mark of the
      * `marks`-th word marks after a base.
      */
    private def isMarkWordAfter(prev: CharKindState, next: CharKindState, word: Int, marks: Int): Boolean =
      next.kind.isWordOf(word) || next.kind.isMarkOf(marks) && prev.isBasedOf(marks)

    /** MarkWordBoundary is `\b` assertion of the `word`-th word set and the `marks`-th word marks. */
    final case class MarkWordBoundary(word: Int, marks: Int) extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        isMarkWordBefore(prev, word, marks) != isMarkWordAfter(prev, next, word, marks)

      override def seesBoundary: Boolean = false

    /** NotMarkWordBoundary is `\B` assertion of the `word`-th word set and the `marks`-th word marks. */
    final case class NotMarkWordBoundary(word: Int, marks: Int) extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean =
        isMarkWordBefore(prev, word, marks) == isMarkWordAfter(prev, next, word, marks)

      override def seesBoundary: Boolean = false

    /** NotBetweenCRLF is an assertion that the position is not between `\r` and `\n`. */
    case object NotBetweenCRLF extends AssertKind:
      def accepts(prev: CharKindState, next: CharKindState): Boolean = !betweenCRLF(prev, next)

      override def seesBoundary: Boolean = false
