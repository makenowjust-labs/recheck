package codes.quine.labs.recheck
package automaton

import scala.collection.mutable

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.util.EcmaScriptNotation
import codes.quine.labs.recheck.util.GraphvizUtil.escape
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.alphabet.CRFollow
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
            s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(chs.map((c, k) => s"(${EcmaScriptNotation.showClass(c)},$k)").mkString("{", ", ", "}"))}];\n"
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
  def toOrderedNFA(using ctx: Context): OrderedNFA[CharSet, (CharKind, Seq[(Q, Set[CharKind])])] =
    toOrderedNFA(Int.MaxValue)

  /** Converts this ε-NFA to ordered NFA with ε-elimination. */
  def toOrderedNFA(maxNFASize: Int)(using ctx: Context): OrderedNFA[CharSet, (CharKind, Seq[(Q, Set[CharKind])])] =
    ctx.interrupt:
      val kinds = tau.values.collect { case Assert(k, _) => k }.toSet
      val guesses = Guesses.of(kinds)
      val baseKinds = alphabet.pairs.iterator.map(_._2).toSet

      // The begin and the end of the input are distinguished only for the assertions looking at them, and they are
      // line terminators if the assertions see no difference, which keeps the automaton smaller.
      val inputTerminatorKind =
        if !guesses.any && !kinds.exists(_.seesBoundary) then CharKind.Normal
        else if !guesses.any && kinds.forall(_.treatsBoundaryAsLineTerminator(baseKinds)) then CharKind.LineTerminator
        else CharKind.Boundary

      // Obtains a set of possible character kinds.
      val charKindSet = baseKinds.flatMap(guesses.variants) ++ Set(inputTerminatorKind)

      type P = (Q, Set[CharKind])

      /** Skips ε-transitions from the state and collects not ε-transition or terminal state. */
      def buildClosure(k0: CharKind, ks: Set[CharKind], q: Q, loops: Set[Int]): Seq[(P, Option[Consume[Q]])] =
        ctx.interrupt:
          if ks.isEmpty then Vector.empty
          else
            tau.get(q) match
              case Some(Eps(qs))             => qs.flatMap(buildClosure(k0, ks, _, loops))
              case Some(Assert(k, q1))       => buildClosure(k0, ks.filter(k.accepts(k0, _)), q1, loops)
              case Some(LoopEnter(loop, q1)) =>
                if loops.contains(loop) then Vector.empty // An ε-loop is detected.
                else buildClosure(k0, ks, q1, loops ++ Set(loop))
              case Some(LoopExit(loop, q1)) =>
                if loops.contains(loop) then Vector.empty // An ε-loop is detected.
                else buildClosure(k0, ks, q1, loops)
              case Some(Consume(chs, q1, pos)) =>
                val newChs = chs.flatMap((ch, k) => guesses.variants(k).filter(ks.contains).map((ch, _)))
                if newChs.nonEmpty then Vector(((q, ks), Some(Consume(newChs, q1, pos))))
                else Vector.empty
              case None =>
                if ks.contains(inputTerminatorKind) then Vector(((q, Set.empty), None))
                else Vector.empty
      // A character tells the following ones only whether it is an extender after a base among its marks, so the other
      // marks are dropped from the kinds of the states.
      def asPrevious(k: CharKind): CharKind =
        if k.marks == 0 then k
        else
          val extenders = (0 until guesses.marks).filter(i => k.isExtenderOf(i) && k.isBasedOf(i))
          k.copy(marks = extenders.map(i => 1 << (3 * i + 1)).sum)

      val closureCache = mutable.Map.empty[(CharKind, Q), Seq[(P, Option[Consume[Q]])]]
      def closure(k0: CharKind, q: Q): Seq[(P, Option[Consume[Q]])] =
        closureCache.getOrElseUpdate((k0, q), buildClosure(k0, charKindSet.filter(guesses.valid(k0, _)), q, Set.empty))

      val closureInit = closure(inputTerminatorKind, init)

      val queue = mutable.Queue.empty[(CharKind, Seq[(P, Option[Consume[Q]])])]
      val newStateSet = mutable.Set.empty[(CharKind, Seq[P])]
      val newInits = Vector((inputTerminatorKind, closureInit.map(_._1)))
      val newAcceptSet = Set.newBuilder[(CharKind, Seq[P])]
      val newDelta = Map.newBuilder[((CharKind, Seq[P]), CharSet), Seq[(CharKind, Seq[P])]]
      val newSourcemap = mutable.Map
        .empty[((CharKind, Seq[P]), CharSet, (CharKind, Seq[P])), Seq[SourceLocation]]
        .withDefaultValue(Vector.empty)
      var deltaSize = 0

      queue.enqueue((inputTerminatorKind, closureInit))
      newStateSet.addAll(newInits)

      while queue.nonEmpty do
        ctx.interrupt:
          val (c0, qps) = queue.dequeue()
          val qs0 = qps.map(_._1)
          if qs0.map(_._1).contains(accept) then newAcceptSet.addOne((c0, qs0))
          val d = mutable.Map.empty[CharSet, Seq[(CharKind, Seq[P])]].withDefaultValue(Vector.empty)
          for (_, p) <- qps do
            p match
              case Some(Consume(chs, q1, loc)) =>
                for (ch, k) <- chs do
                  val k1 = asPrevious(k)
                  val qps1 = closure(k1, q1)
                  val qs1 = qps1.map(_._1)
                  d(ch) = d(ch) :+ (k1, qs1)
                  newSourcemap(((c0, qs0), ch, (k1, qs1))) ++= loc
                  if !newStateSet.contains((k1, qs1)) then
                    queue.enqueue((k1, qps1))
                    newStateSet.addOne((k1, qs1))
              case None =>
                () // Nothing to do here because of terminal state.
          for (ch, to) <- d do newDelta.addOne(((c0, qs0), ch) -> to)
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

  /** Guesses is which guesses on the context the character kinds make (see [[CharKind]]).
    *
    * A guess makes a character have more than one kind, and the next (or the previous) kind checks it, so that only the
    * right one survives.
    *
    * With `last`, a line terminator, `\r` or `\n` guesses whether it is the last character (for
    * [[AssertKind.ChompEnd]]). With `afterCR`, `\n` guesses whether `\r` precedes it (for [[AssertKind.LineBeginOf]]
    * after `\r\n`). With `follow`, `\r` guesses what follows it (for [[AssertKind.LineEndOf]] before `\r\n` and
    * [[AssertKind.ChompEnd]] before the final `\r\n`). `marks` is the number of the word marks, for each of which an
    * extender guesses whether the text up to it has a base (for [[AssertKind.MarkWordBoundaryOf]]).
    */
  final case class Guesses(last: Boolean, afterCR: Boolean, follow: Boolean, marks: Int = 0):

    def any: Boolean = last || afterCR || follow || marks > 0

    /** Returns the kinds of the guesses on a character of the kind. */
    def variants(k: CharKind): Seq[CharKind] =
      if !any then Seq(k)
      else
        val lasts = if last && (k.isLineTerminator || k.cr || k.lf) then Seq(Some(false), Some(true)) else Seq(None)
        val afterCRs = if afterCR && k.lf then Seq(Some(false), Some(true)) else Seq(None)
        val follows =
          if follow && k.cr then (Seq(CRFollow.Other, CRFollow.LF) ++ Option.when(last)(CRFollow.FinalLF)).map(Some(_))
          else Seq(None)
        // A base has its base, an extender may have one before it, and the others have none.
        val baseds = (0 until marks).foldLeft(Seq(0)): (bs, i) =>
          if k.isBaseOf(i) then bs.map(_ | (1 << i))
          else if k.isExtenderOf(i) then bs.flatMap(b => Seq(b, b | (1 << i)))
          else bs
        for l <- lasts; a <- afterCRs; f <- follows; b <- baseds
        yield k.copy(last = l, afterCR = a, follow = f, based = b)

    /** Checks the guesses between the kinds of adjacent characters (or the begin and the end of the input). */
    def valid(prev: CharKind, next: CharKind): Boolean =
      val lastOK = prev.last.forall(_ == next.boundary)
      val afterCROK = next.afterCR.forall(_ == prev.cr)
      val followOK = prev.follow.forall:
        case CRFollow.Other   => !next.lf
        case CRFollow.LF      => next.lf && !next.last.contains(true)
        case CRFollow.FinalLF => next.lf && next.last.contains(true)
      val basedOK = (0 until marks).forall: i =>
        !next.isExtenderOf(i) || next.isBasedOf(i) == prev.isBasedOf(i)
      lastOK && afterCROK && followOK && basedOK

  object Guesses:

    /** Returns the guesses that the assertions need. */
    def of(kinds: Set[AssertKind]): Guesses =
      val last = kinds.exists(_.isInstanceOf[AssertKind.ChompEnd])
      val afterCR = kinds.exists:
        case AssertKind.LineBeginOf(_, _, afterCRLF, _) => afterCRLF
        case _                                          => false
      val follow = kinds.exists:
        case AssertKind.LineEndOf(_, beforeCRLF, _) => beforeCRLF
        case AssertKind.ChompEnd(_, crlf, _)        => crlf
        case _                                      => false
      val marks = kinds
        .collect:
          case AssertKind.MarkWordBoundaryOf(_, m)    => m + 1
          case AssertKind.MarkWordBoundaryNotOf(_, m) => m + 1
        .maxOption
        .getOrElse(0)
      Guesses(last, afterCR, follow, marks)

  /** AssertKind is assertion kind of this ε-NFA transition. */
  sealed abstract class AssertKind extends Serializable with Product:

    /** Tests the assertion between the kinds of the previous and the next characters. */
    def accepts(prev: CharKind, next: CharKind): Boolean

    /** Whether the assertion distinguishes the begin and the end of the input from the characters. */
    def seesBoundary: Boolean = true

    /** Whether the assertion tests the begin and the end of the input as a line terminator among the kinds. */
    def treatsBoundaryAsLineTerminator(kinds: Iterable[CharKind]): Boolean =
      val (b, t) = (CharKind.Boundary, CharKind.LineTerminator)
      (kinds ++ Seq(b, t)).forall(k => accepts(b, k) == accepts(t, k) && accepts(k, b) == accepts(k, t))

    /** Returns a set of next possible basic character kinds from the previous character kind. */
    def toCharKindSet(prev: CharKind): Set[CharKind] = CharKind.basic.filter(accepts(prev, _)).toSet

  /** AssertKind values and utilities. */
  object AssertKind:

    private def betweenCRLF(prev: CharKind, next: CharKind): Boolean = prev.cr && next.lf

    /** LineBegin is `^` assertion, which matches after a line terminator of the first set or at the begin. */
    case object LineBegin extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean = prev.boundary || prev.isLineTerminatorOf(0)

    /** LineEnd is `$` assertion, which matches before a line terminator of the first set or at the end. */
    case object LineEnd extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean = next.boundary || next.isLineTerminatorOf(0)

    /** WordBoundary is `\b` assertion of the first word set. */
    case object WordBoundary extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean = prev.word != next.word
      override def seesBoundary: Boolean = false

    /** WordBoundaryNot is `\B` assertion of the first word set. */
    case object WordBoundaryNot extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean = prev.word == next.word
      override def seesBoundary: Boolean = false

    /** WordBoundaryOf is `\b` assertion of the `set`-th word set. */
    final case class WordBoundaryOf(set: Int) extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean = prev.isWordOf(set) != next.isWordOf(set)
      override def seesBoundary: Boolean = false

    /** WordBoundaryNotOf is `\B` assertion of the `set`-th word set. */
    final case class WordBoundaryNotOf(set: Int) extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean = prev.isWordOf(set) == next.isWordOf(set)
      override def seesBoundary: Boolean = false

    /** Tests whether the character of the kind before a position is a word character of the `word`-th word set, or an
      * extender of the `marks`-th word marks after a base.
      */
    private def isMarkWordBefore(prev: CharKind, word: Int, marks: Int): Boolean =
      prev.isWordOf(word) || prev.isExtenderOf(marks) && prev.isBasedOf(marks)

    /** Tests whether the character of the kind after a position is a word character of the `word`-th word set, or a
      * mark of the `marks`-th word marks after a base.
      */
    private def isMarkWordAfter(prev: CharKind, next: CharKind, word: Int, marks: Int): Boolean =
      next.isWordOf(word) || next.isMarkOf(marks) && prev.isBasedOf(marks)

    /** MarkWordBoundaryOf is `\b` assertion of the `word`-th word set and the `marks`-th word marks. */
    final case class MarkWordBoundaryOf(word: Int, marks: Int) extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean =
        isMarkWordBefore(prev, word, marks) != isMarkWordAfter(prev, next, word, marks)
      override def seesBoundary: Boolean = false

    /** MarkWordBoundaryNotOf is `\B` assertion of the `word`-th word set and the `marks`-th word marks. */
    final case class MarkWordBoundaryNotOf(word: Int, marks: Int) extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean =
        isMarkWordBefore(prev, word, marks) == isMarkWordAfter(prev, next, word, marks)
      override def seesBoundary: Boolean = false

    /** InputBegin is `\A` assertion. */
    case object InputBegin extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean = prev.boundary

    /** InputEnd is `\z` assertion. */
    case object InputEnd extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean = next.boundary

    /** LineBeginOf is `^` assertion with the `set`-th line terminator set, which `atEnd` tells where it matches at the
      * end of the input.
      *
      * With `afterCRLF`, it matches after `\r\n` too, where `\n` is not a line terminator by itself (PCRE's `(*CRLF)`).
      * With `atomic`, it does not match between `\r` and `\n` (Java).
      */
    final case class LineBeginOf(set: Int, atEnd: IRLineBeginAtEnd, afterCRLF: Boolean, atomic: Boolean)
        extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean =
        val afterTerminator = prev.isLineTerminatorOf(set) || (afterCRLF && prev.lf && prev.afterCR.contains(true))
        (prev.boundary || afterTerminator) && !(atomic && betweenCRLF(prev, next)) &&
        (!next.boundary || (atEnd match
          case IRLineBeginAtEnd.AfterTerminator => true
          case IRLineBeginAtEnd.InputBegin      => prev.boundary
          case IRLineBeginAtEnd.Never           => false))

    /** LineEndOf is `$` assertion with the `set`-th line terminator set.
      *
      * With `beforeCRLF`, it matches before `\r\n` too, where `\r` is not a line terminator by itself (PCRE's
      * `(*CRLF)`). With `atomic`, it does not match between `\r` and `\n` (Java).
      */
    final case class LineEndOf(set: Int, beforeCRLF: Boolean, atomic: Boolean) extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean =
        val beforeTerminator =
          next.isLineTerminatorOf(set) || (beforeCRLF && next.cr && !next.follow.contains(CRFollow.Other))
        (next.boundary || beforeTerminator) && !(atomic && betweenCRLF(prev, next))

    /** ChompEnd is `\Z` assertion with the `set`-th line terminator set, which matches at the end or before a line
      * terminator at the end.
      *
      * With `crlf`, it matches before `\r\n` at the end too. With `atomic`, it does not match between `\r` and `\n`
      * (Java).
      */
    final case class ChompEnd(set: Int, crlf: Boolean, atomic: Boolean) extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean =
        val beforeFinal = (next.isLineTerminatorOf(set) && next.last.contains(true)) ||
          (crlf && next.cr && next.follow.contains(CRFollow.FinalLF))
        (next.boundary || beforeFinal) && !(atomic && betweenCRLF(prev, next))

    /** NotBetweenCRLF is an assertion that the position is not between `\r` and `\n`. */
    case object NotBetweenCRLF extends AssertKind:
      def accepts(prev: CharKind, next: CharKind): Boolean = !betweenCRLF(prev, next)
      override def seesBoundary: Boolean = false
