package codes.quine.labs.recheck.regexp

import scala.annotation.tailrec

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.resyntax.charset.CaseMapping
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRAssertKind
import codes.quine.labs.resyntax.ir.IRCondition
import codes.quine.labs.resyntax.ir.IRLineTerminators
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRNodeData.*
import codes.quine.labs.resyntax.ir.IRQuantifier
import codes.quine.labs.resyntax.ir.IRWordMarks

object PatternExtensions:

  /** CaptureRange is a range of capture indexes. */
  final case class CaptureRange(range: Option[(Int, Int)]) extends AnyVal:

    /** Merges two ranges. */
    def merge(other: CaptureRange): CaptureRange =
      (range, other.range) match
        case (Some((min1, max1)), Some((min2, max2))) =>
          CaptureRange(Some((Math.min(min1, min2), Math.max(max1, max2))))
        case (Some((min, max)), None) => CaptureRange(Some((min, max)))
        case (None, Some((min, max))) => CaptureRange(Some((min, max)))
        case (None, None)             => CaptureRange(None)

  /** Checks whether the set is described more simply by its complement, as a negated class or `.` is. */
  def prefersComplement(set: CharSet, mode: UnitMode): Boolean =
    set.complement(mode).intervalCount < set.intervalCount

  /** Checks whether the set is one character or the case variants of one character, as a literal is. */
  def isOneCharUpToCase(set: CharSet): Boolean =
    set.nonEmpty && (set.size == 1 || {
      val c = CharSet.single(set.head)
      val variants = CaseMapping.SimpleFold.closure(c) union CaseMapping.SimpleUpper.closure(c) union
        CaseMapping.SimpleLower.closure(c)
      set.subsetOf(variants)
    })

  /** Checks whether the assertion is at a begin point (`^` or `\A`). */
  def isBeginAssertion(kind: IRAssertKind): Boolean = kind match
    case IRAssertKind.TextBegin | _: IRAssertKind.LineBegin => true
    case _                                                  => false

  /** Checks whether the assertion is at an end point (`$`, `\z` or `\Z`). */
  def isEndAssertion(kind: IRAssertKind): Boolean = kind match
    case IRAssertKind.TextEnd | _: IRAssertKind.LineEnd | _: IRAssertKind.ChompTextEnd => true
    case _                                                                             => false

  extension (pattern: ParsedPattern)

    /** Tests whether the pattern contains a begin or end assertion (e.g. `^`, `$` or `\A`). */
    def existsLineAssertion(using ctx: Context): Boolean = ctx.interrupt:
      pattern.node.exists:
        case Assert(kind) => isBeginAssertion(kind) || isEndAssertion(kind)
        case _            => false

    /** Tests whether a begin assertion (resp. an end assertion) is placed not at begin point (resp. end point). */
    def existsLineAssertionInMiddle(using ctx: Context): Boolean = ctx.interrupt:
      def loop(isBegin: Boolean, isEnd: Boolean, node: IRNode): Boolean = ctx.interrupt:
        node.data match
          case Disjunction(ns) => ns.exists(loop(isBegin, isEnd, _))
          case Sequence(ns)    =>
            ns match
              case n1 +: ns :+ n2 =>
                loop(isBegin, false, n1) || ns.exists(loop(false, false, _)) || loop(false, isEnd, n2)
              case Seq(n) => loop(isBegin, isEnd, n)
              case Seq()  => false
          case Capture(_, _, n)                 => loop(isBegin, isEnd, n)
          case Atomic(n)                        => loop(isBegin, isEnd, n)
          case Repeat(n, _)                     => loop(false, false, n)
          case Assert(k) if isBeginAssertion(k) => !isBegin
          case Assert(k) if isEndAssertion(k)   => !isEnd
          case _                                => node.children.exists(loop(false, false, _))
      loop(true, true, pattern.node)

    /** Tests whether a begin assertion is placed at every begin point. */
    def everyBeginPointIsLineBegin(using ctx: Context): Boolean = everyBeginPointIs(isBeginAssertion)

    /** Tests whether none of begin assertions is placed at every begin point. */
    def everyBeginPointIsNotLineBegin(using ctx: Context): Boolean = everyBeginPointIs(!isBeginAssertion(_), true)

    /** Tests whether an end assertion is placed at every end point. */
    def everyEndPointIsLineEnd(using ctx: Context): Boolean = everyEndPointIs(isEndAssertion)

    /** Tests whether none of end assertions is placed at every end point. */
    def everyEndPointIsNotLineEnd(using ctx: Context): Boolean = everyEndPointIs(!isEndAssertion(_), true)

    /** Tests whether an assertion satisfying `p` is placed at every begin point.
      *
      * A node other than an assertion is regarded as `otherwise`.
      */
    def everyBeginPointIs(p: IRAssertKind => Boolean, otherwise: Boolean = false)(using ctx: Context): Boolean =
      ctx.interrupt:
        def loop(node: IRNode): Boolean = ctx.interrupt:
          node.data match
            case Disjunction(ns)  => ns.forall(loop)
            case Sequence(ns)     => ns.headOption.exists(loop)
            case Capture(_, _, n) => loop(n)
            case Atomic(n)        => loop(n)
            case Assert(k)        => p(k)
            case _                => otherwise
        loop(pattern.node)

    /** Tests whether an assertion satisfying `p` is placed at every end point.
      *
      * A node other than an assertion is regarded as `otherwise`.
      */
    def everyEndPointIs(p: IRAssertKind => Boolean, otherwise: Boolean = false)(using ctx: Context): Boolean =
      ctx.interrupt:
        def loop(node: IRNode): Boolean = ctx.interrupt:
          node.data match
            case Disjunction(ns)  => ns.forall(loop)
            case Sequence(ns)     => ns.lastOption.exists(loop)
            case Capture(_, _, n) => loop(n)
            case Atomic(n)        => loop(n)
            case Assert(k)        => p(k)
            case _                => otherwise
        loop(pattern.node)

    /** Tests the pattern has no infinite repetition. */
    def isConstant: Boolean =
      !pattern.node.exists:
        case Repeat(_, _: IRQuantifier.Unbounded) => true
        // An unsupported node may be a recursion.
        case _: Unsupported => true
        case _              => false

    /** Returns this pattern's size. */
    def size: Int =
      def loop(node: IRNode): Int = node.data match
        case Disjunction(ns)                          => ns.map(loop).sum + ns.size - 1
        case Sequence(ns)                             => ns.map(loop).sum
        case Empty                                    => 0
        case Capture(_, _, n)                         => loop(n)
        case Atomic(n)                                => loop(n)
        case Repeat(n, IRQuantifier.Exact(m))         => loop(n) * m
        case Repeat(n, IRQuantifier.Unbounded(m, _))  => loop(n) * Math.max(m, 1) + 1
        case Repeat(n, IRQuantifier.Bounded(m, l, _)) => loop(n) * l + (l - m)
        case LookAhead(_, n)                          => loop(n) + 1
        case LookBehind(_, n, _)                      => loop(n) + 1
        case NonAtomicLookAhead(n)                    => loop(n) + 1
        case NonAtomicLookBehind(n, _)                => loop(n) + 1
        case Conditional(_, _, _)                     => node.children.map(loop).sum + 1
        case _                                        => 1
      loop(pattern.node)

    /** Returns a sum of the counts of the repetitions other than `*`, `+` and `?` (e.g. `{2,3}` counts `3`). */
    def repeatCount(using ctx: Context): Int = ctx.interrupt:
      def loop(node: IRNode): Int = ctx.interrupt:
        node.data match
          case Repeat(n, q) =>
            val count = loop(n)
            q match
              case IRQuantifier.Exact(m)            => count + m
              case IRQuantifier.Unbounded(0 | 1, _) => count
              case IRQuantifier.Unbounded(min, _)   => count + min
              case IRQuantifier.Bounded(0, 1, _)    => count
              case IRQuantifier.Bounded(_, max, _)  => count + max
          case _ => node.children.map(loop).sum
      loop(pattern.node)

    /** Returns the distinct line terminators of the assertions. */
    def lineTerminators: Seq[IRLineTerminators] =
      pattern.node
        .collect:
          case Assert(IRAssertKind.LineBegin(t, _)) => t
          case Assert(IRAssertKind.LineEnd(t))      => t
          case Assert(IRAssertKind.ChompTextEnd(t)) => t
        .distinct

    /** Returns the text end assertions before a final line terminator (e.g. Python's `$`) not at an end point. */
    def chompTextEndsInMiddle: Seq[IRNode] =
      def loop(isEnd: Boolean, node: IRNode): Seq[IRNode] =
        node.data match
          case Disjunction(ns) => ns.flatMap(loop(isEnd, _))
          case Sequence(ns)    =>
            val last = ns.size - 1
            ns.zipWithIndex.flatMap((n, i) => loop(isEnd && i == last, n))
          case Capture(_, _, n)                     => loop(isEnd, n)
          case Atomic(n)                            => loop(isEnd, n)
          case Assert(_: IRAssertKind.ChompTextEnd) => if isEnd then Seq.empty else Seq(node)
          case _                                    => node.children.flatMap(loop(false, _))
      loop(true, pattern.node)

    /** Returns the distinct line terminators that the character kinds tell, which are of the line assertions and the
      * text end assertions in the middle.
      */
    def kindLineTerminators: Seq[IRLineTerminators] =
      val chomps = chompTextEndsInMiddle.collect { case IRNode(Assert(IRAssertKind.ChompTextEnd(t)), _) => t }
      (pattern.node.collect {
        case Assert(IRAssertKind.LineBegin(t, _)) => t
        case Assert(IRAssertKind.LineEnd(t))      => t
      } ++ chomps).distinct

    /** Tests whether the character kinds tell `\r` and `\n`, for an assertion seeing `\r\n` as one line terminator. */
    def needsCRLFDistinction: Boolean =
      lineTerminators.exists(_.crlf == IRLineTerminators.CRLF.Atomic) ||
        pattern.node.exists(_ == Assert(IRAssertKind.NotBetweenCRLF)) ||
        kindLineTerminators.exists(t =>
          t.crlf != IRLineTerminators.CRLF.Ignored && !(t.chars(0x0a) && t.chars(0x0d))
        ) ||
        chompTextEndsInMiddle.exists:
          case IRNode(Assert(IRAssertKind.ChompTextEnd(t)), _) => t.crlf != IRLineTerminators.CRLF.Ignored
          case _                                               => false

    /** Returns the distinct word sets of the word boundary assertions. */
    def wordSets: Seq[CharSet] =
      pattern.node
        .collect:
          case Assert(IRAssertKind.WordBoundary(w))           => w
          case Assert(IRAssertKind.NotWordBoundary(w))        => w
          case Assert(IRAssertKind.MarkWordBoundary(w, _))    => w
          case Assert(IRAssertKind.NotMarkWordBoundary(w, _)) => w
        .distinct

    /** Returns the distinct marks of the word boundary assertions taking marks after a base as word characters. */
    def wordMarks: Seq[IRWordMarks] =
      pattern.node
        .collect:
          case Assert(IRAssertKind.MarkWordBoundary(_, m))    => m
          case Assert(IRAssertKind.NotMarkWordBoundary(_, m)) => m
        .distinct

    /** The characters that JavaScript's ignore-case matching canonicalizes to, which stand for their cases. */
    def preferredChars: Option[CharSet] =
      Option.when(pattern.dialect.isJavaScript && pattern.flagSet.ignoreCase):
        val unicode = pattern.ir.mode == UnitMode.CodePoint
        EcmaScriptChars.canonicalize(pattern.ir.mode.universe, unicode)

    /** Returns the character standing for the set of a literal, which may be closed under case-insensitivity. */
    def representative(set: CharSet, preferred: Option[CharSet]): Int =
      preferred.map(set.intersect).filter(_.nonEmpty).getOrElse(set).head

    /** Computes alphabet from this pattern. */
    def alphabet(using ctx: Context): PatternAlphabet =
      ctx.interrupt:
        def loop(node: IRNode): Seq[CharSet] = ctx.interrupt:
          node.data match
            case Chars(set) =>
              // A set like a negated class takes part in the order of atoms as its complement.
              Vector(if prefersComplement(set, pattern.ir.mode) then set.complement(pattern.ir.mode) else set)
            case Strings(set) =>
              set.strings.toVector.flatMap(_.iterator.map(CharSet.single)) :+ set.chars
            case _ => node.children.flatMap(loop)

        val terminators = lineTerminators
        val terminatorSets = terminators.flatMap: t =>
          val chars = CharSet.of(t.chars.toSeq*)
          if t.crlf == IRLineTerminators.CRLF.Ignored then Seq(chars)
          else Seq(chars, CharSet.of(0x0d), CharSet.of(0x0a))
        val words = wordSets
        val kinds = kindLineTerminators.map(t => CharSet.of(t.chars.toSeq*))
        val lineTerminator = kinds.headOption.filter(_.nonEmpty)
        val extra = terminatorSets.filterNot(kinds.contains)

        PatternAlphabet.build(
          pattern.ir.mode,
          loop(pattern.node) ++ extra,
          lineTerminator,
          words.headOption,
          preferredChars,
          needsCRLFDistinction,
          kinds.drop(1),
          words.drop(1),
          wordMarks
        )

    /** Tests whether the pattern needs line terminator distinction or not. */
    def needsLineTerminatorDistinction(using ctx: Context): Boolean = ctx.interrupt:
      pattern.node.exists:
        case Assert(_: IRAssertKind.LineBegin | _: IRAssertKind.LineEnd) => true
        case _                                                           => false

    /** Tests whether the pattern nedds input terminator distinction or not. */
    def needsInputTerminatorDistinction(using ctx: Context): Boolean = ctx.interrupt:

      // When `^` or `$` exists and `m` flags is enabled, then the pattern needs input terminator distinction too.
      if needsLineTerminatorDistinction then return true
      // When there is no line assertion, then the pattern does not need input terminator distinction.
      if !existsLineAssertion then return false
      // When line assertion appears in middle part of the pattern, then the pattern needs input terminator distinction.
      if existsLineAssertionInMiddle then return true

      // When line begin (resp. end) assertion appears at every begin (resp. end) point with consistency,
      // then the pattern needs input terminator distinction.
      (!everyBeginPointIsLineBegin && !everyBeginPointIsNotLineBegin) || (!everyEndPointIsLineEnd && !everyEndPointIsNotLineEnd)

    /** Tests whether the pattern has implicit `.*?` at begin. */
    def needsSigmaStarAtBegin(using ctx: Context): Boolean =
      ctx.interrupt(!pattern.ir.anchored && !everyBeginPointIs(_ == IRAssertKind.TextBegin))

    /** Tests whether the pattern has implicit `.*?` at end. */
    def needsSigmaStarAtEnd(using ctx: Context): Boolean =
      ctx.interrupt:
        !everyEndPointIs:
          case IRAssertKind.TextEnd | _: IRAssertKind.ChompTextEnd => true
          case _                                                   => false

    /** Tests whether the pattern needs word character distinction or not. */
    def needsWordDistinction(using ctx: Context): Boolean = ctx.interrupt(wordSets.nonEmpty)

    /** Extracts parts within the pattern. */
    def parts: Set[UString] =
      val preferred = preferredChars

      def toUString(values: Seq[Int]): UString =
        if pattern.ir.mode == UnitMode.CodePoint then UStringUtil.fromCodePoints(values) else UString.from(values)

      def literal(node: IRNode): Option[Int] = node.data match
        case Chars(set) if isOneCharUpToCase(set) => Some(representative(set, preferred))
        case _                                    => None

      @tailrec
      def extract(ns: Seq[IRNode], set: Set[UString] = Set.empty): Set[UString] =
        if ns.isEmpty then set
        else
          val (pre, suf) = ns.span(literal(_).isDefined)
          val s = toUString(pre.flatMap(literal))
          val newSet = set ++ (if s.length > 1 then Set(s) else Set.empty)
          extract(suf.dropWhile(literal(_).isEmpty), newSet)

      def loop(node: IRNode): Set[UString] = node.data match
        case Sequence(ns) => extract(ns) ++ ns.flatMap(loop).toSet
        case _            => node.children.flatMap(loop).toSet

      loop(pattern.node)

    /** Returns a maximum capture index or `0`. */
    def capturesSize: Int = pattern.ir.captureCount

  extension (node: IRNode)

    def children: Seq[IRNode] = node.data match
      case Disjunction(ns)                 => ns
      case Sequence(ns)                    => ns
      case Repeat(n, _)                    => Seq(n)
      case Atomic(n)                       => Seq(n)
      case LookAhead(_, n)                 => Seq(n)
      case LookBehind(_, n, _)             => Seq(n)
      case Capture(_, _, n)                => Seq(n)
      case NonAtomicLookAhead(n)           => Seq(n)
      case NonAtomicLookBehind(n, _)       => Seq(n)
      case Conditional(condition, yes, no) =>
        condition match
          case IRCondition.LookAhead(_, n)     => Seq(n, yes, no)
          case IRCondition.LookBehind(_, n, _) => Seq(n, yes, no)
          case IRCondition.Captured(_)         => Seq(yes, no)
      case _ => Seq.empty

    /** Checks whether the node or a descendant satisfies the predicate. */
    def exists(p: IRNodeData => Boolean): Boolean =
      p(node.data) || node.children.exists(_.exists(p))

    /** Collects the values of the node and its descendants in the pre-order. */
    def collect[A](pf: PartialFunction[IRNodeData, A]): Seq[A] =
      pf.lift(node.data).toSeq ++ node.children.flatMap(_.collect(pf))

    /** Returns a capture range within this node. */
    def captureRange: CaptureRange = node.data match
      case Capture(index, _, n) => CaptureRange(Some((index, index))).merge(n.captureRange)
      case _                    => node.children.map(_.captureRange).foldLeft(CaptureRange(None))(_.merge(_))

    /** Checks this node can match an empty string. */
    def canMatchEmpty: Boolean = node.data match
      case Disjunction(ns)  => ns.exists(_.canMatchEmpty)
      case Sequence(ns)     => ns.forall(_.canMatchEmpty)
      case Capture(_, _, n) => n.canMatchEmpty
      case Atomic(n)        => n.canMatchEmpty
      case Repeat(n, q)     =>
        q match
          case IRQuantifier.Exact(m)         => m == 0 || n.canMatchEmpty
          case IRQuantifier.Unbounded(m, _)  => m == 0 || n.canMatchEmpty
          case IRQuantifier.Bounded(m, _, _) => m == 0 || n.canMatchEmpty
      case Conditional(_, yes, no) => yes.canMatchEmpty || no.canMatchEmpty
      case Chars(_)                => false
      case Strings(set)            => set.strings.exists(_.isEmpty)
      case _                       => true
