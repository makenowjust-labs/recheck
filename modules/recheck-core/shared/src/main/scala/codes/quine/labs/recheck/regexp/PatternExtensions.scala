package codes.quine.labs.recheck.regexp

import scala.annotation.tailrec
import scala.util.chaining.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.regexp.Pattern.*
import codes.quine.labs.recheck.unicode.IChar
import codes.quine.labs.recheck.unicode.ICharSet
import codes.quine.labs.recheck.unicode.ICharSet.CharKind
import codes.quine.labs.recheck.unicode.UString

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

  extension (pattern: Pattern)

    /** Tests whether the pattern contains line assertion (`^` or `$`). */
    def existsLineAssertion(using ctx: Context): Boolean = ctx.interrupt:
      def loop(node: Node): Boolean = ctx.interrupt:
        node match
          case Disjunction(ns)         => ns.exists(loop)
          case Sequence(ns)            => ns.exists(loop)
          case Capture(_, n)           => loop(n)
          case NamedCapture(_, _, n)   => loop(n)
          case Group(n)                => loop(n)
          case Repeat(_, n)            => loop(n)
          case LookAhead(_, n)         => loop(n)
          case LookBehind(_, n)        => loop(n)
          case LineBegin() | LineEnd() => true
          case _                       => false
      loop(pattern.node)

    /** Tests whether line begin assertion `^` (resp. line end assertion `$`) is placed not at begin point (resp. end
      * point).
      */
    def existsLineAssertionInMiddle(using ctx: Context): Boolean = ctx.interrupt:
      def loop(isBegin: Boolean, isEnd: Boolean, node: Node): Boolean = ctx.interrupt:
        node match
          case Disjunction(ns) => ns.exists(loop(isBegin, isEnd, _))
          case Sequence(ns)    =>
            ns match
              case n1 +: ns :+ n2 =>
                loop(isBegin, false, n1) || ns.exists(loop(false, false, _)) || loop(false, isEnd, n2)
              case Seq(n) => loop(isBegin, isEnd, n)
              case Seq()  => false
          case Capture(_, n)         => loop(isBegin, isEnd, n)
          case NamedCapture(_, _, n) => loop(isBegin, isEnd, n)
          case Group(n)              => loop(isBegin, isEnd, n)
          case Repeat(_, n)          => loop(false, false, n)
          case LookAhead(_, n)       => loop(false, false, n)
          case LookBehind(_, n)      => loop(false, false, n)
          case LineBegin()           => !isBegin
          case LineEnd()             => !isEnd
          case _                     => false
      loop(true, true, pattern.node)

    /** Tests whether line begin assertion (`^`) is placed at every begin point. */
    def everyBeginPointIsLineBegin(using ctx: Context): Boolean = ctx.interrupt:
      def loop(node: Node): Boolean = ctx.interrupt:
        node match
          case Disjunction(ns)       => ns.forall(loop)
          case Sequence(ns)          => ns.headOption.exists(loop)
          case Capture(_, n)         => loop(n)
          case NamedCapture(_, _, n) => loop(n)
          case Group(n)              => loop(n)
          case LineBegin()           => true
          case _                     => false
      loop(pattern.node)

    /** Tests whether none of line begin assertion (`^`) is placed at every begin point. */
    def everyBeginPointIsNotLineBegin(using ctx: Context): Boolean = ctx.interrupt:
      def loop(node: Node): Boolean = ctx.interrupt:
        node match
          case Disjunction(ns)       => ns.forall(loop)
          case Sequence(ns)          => ns.headOption.exists(loop)
          case Capture(_, n)         => loop(n)
          case NamedCapture(_, _, n) => loop(n)
          case Group(n)              => loop(n)
          case LineBegin()           => false
          case _                     => true
      loop(pattern.node)

    /** Tests whether line end assertion (`^`) is placed at every end point. */
    def everyEndPointIsLineEnd(using ctx: Context): Boolean = ctx.interrupt:
      def loop(node: Node): Boolean = ctx.interrupt:
        node match
          case Disjunction(ns)       => ns.forall(loop)
          case Sequence(ns)          => ns.lastOption.exists(loop)
          case Capture(_, n)         => loop(n)
          case NamedCapture(_, _, n) => loop(n)
          case Group(n)              => loop(n)
          case LineEnd()             => true
          case _                     => false
      loop(pattern.node)

    /** Tests whether none of line end assertion (`^`) is placed at every end point. */
    def everyEndPointIsNotLineEnd(implicit ctx: Context): Boolean = ctx.interrupt:
      def loop(node: Node): Boolean = ctx.interrupt:
        node match
          case Disjunction(ns)       => ns.forall(loop)
          case Sequence(ns)          => ns.lastOption.exists(loop)
          case Capture(_, n)         => loop(n)
          case NamedCapture(_, _, n) => loop(n)
          case Group(n)              => loop(n)
          case LineEnd()             => false
          case _                     => true
      loop(pattern.node)

    /** Tests the pattern has no infinite repetition. */
    def isConstant: Boolean =
      def loop(node: Node): Boolean = node match
        case Disjunction(ns)       => ns.forall(loop)
        case Sequence(ns)          => ns.forall(loop)
        case Capture(_, n)         => loop(n)
        case NamedCapture(_, _, n) => loop(n)
        case Group(n)              => loop(n)
        case Repeat(q, n)          =>
          q.normalized match
            case Quantifier.Unbounded(_, _) => false
            case _                          => loop(n)
        case LookAhead(_, n)  => loop(n)
        case LookBehind(_, n) => loop(n)
        case _                => true
      loop(pattern.node)

    /** Returns this pattern's size. */
    def size: Int =
      def loop(node: Node): Int = node match
        case Disjunction(ns)                        => ns.map(loop).sum + ns.size - 1
        case Sequence(ns)                           => ns.map(loop).sum
        case Capture(_, n)                          => loop(n)
        case NamedCapture(_, _, n)                  => loop(n)
        case Group(n)                               => loop(n)
        case Repeat(Quantifier.Star(_), n)          => loop(n) + 1
        case Repeat(Quantifier.Plus(_), n)          => loop(n) + 1
        case Repeat(Quantifier.Question(_), n)      => loop(n) + 1
        case Repeat(Quantifier.Exact(m, _), n)      => loop(n) * m
        case Repeat(Quantifier.Unbounded(m, _), n)  => loop(n) * (m + 1) + 1
        case Repeat(Quantifier.Bounded(m, l, _), n) => loop(n) * l + (l - m)
        case LookAhead(_, n)                        => loop(n) + 1
        case LookBehind(_, n)                       => loop(n) + 1
        case _                                      => 1
      loop(pattern.node)

    /** Computes alphabet from this pattern. */
    def alphabet(using ctx: Context): ICharSet =
      ctx.interrupt:
        val FlagSet(_, ignoreCase, _, dotAll, unicode, _) = pattern.flagSet
        val set = ICharSet
          .any(ignoreCase, unicode)
          .pipe(set =>
            if needsLineTerminatorDistinction then set.add(IChar.LineTerminator, CharKind.LineTerminator) else set
          )
          .pipe(set => if needsWordDistinction then set.add(IChar.Word, CharKind.Word) else set)

        def loop(node: Node): Seq[IChar] = ctx.interrupt:
          node match
            case Disjunction(ns)       => ns.flatMap(loop)
            case Sequence(ns)          => ns.flatMap(loop)
            case Capture(_, n)         => loop(n)
            case NamedCapture(_, _, n) => loop(n)
            case Group(n)              => loop(n)
            case Repeat(_, n)          => loop(n)
            case LookAhead(_, n)       => loop(n)
            case LookBehind(_, n)      => loop(n)
            case atom: AtomNode        =>
              val ch = atom.toIChar(unicode)
              Vector(if (ignoreCase) IChar.canonicalize(ch, unicode) else ch)
            case Dot() => Vector(IChar.dot(ignoreCase, dotAll, unicode))
            case _     => Vector.empty

        loop(pattern.node).toSet.foldLeft(set)(_.add(_))

    /** Tests whether the pattern needs line terminator distinction or not. */
    def needsLineTerminatorDistinction(using ctx: Context): Boolean =
      import pattern.*
      ctx.interrupt(flagSet.multiline && existsLineAssertion)

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
      import pattern.*
      ctx.interrupt(!flagSet.sticky && (flagSet.multiline || !everyBeginPointIsLineBegin))

    /** Tests whether the pattern has implicit `.*?` at end. */
    def needsSigmaStarAtEnd(using ctx: Context): Boolean =
      import pattern.*
      ctx.interrupt(!flagSet.sticky && (flagSet.multiline || !everyEndPointIsLineEnd))

    /** Tests whether the pattern needs word character distinction or not. */
    def needsWordDistinction(using ctx: Context): Boolean = ctx.interrupt:
      def loop(node: Node): Boolean = ctx.interrupt:
        node match
          case Disjunction(ns)       => ns.exists(loop)
          case Sequence(ns)          => ns.exists(loop)
          case Capture(_, n)         => loop(n)
          case NamedCapture(_, _, n) => loop(n)
          case Group(n)              => loop(n)
          case Repeat(_, n)          => loop(n)
          case LookAhead(_, n)       => loop(n)
          case LookBehind(_, n)      => loop(n)
          case WordBoundary(_)       => true
          case _                     => false
      loop(pattern.node)

    /** Extracts parts within the pattern. */
    def parts: Set[UString] =
      @tailrec
      def extract(ns: Seq[Node], set: Set[UString] = Set.empty): Set[UString] =
        if ns.isEmpty then set
        else
          val (pre, suf) = ns.span(_.isInstanceOf[Character])
          val s = UString(pre.collect { case Character(c) => c.asString }.mkString)
          val newSet = set ++ (if s.sizeAsString > 1 then Set(s) else Set.empty)
          extract(suf.dropWhile(!_.isInstanceOf[Character]), newSet)

      def loop(node: Node): Set[UString] = node match
        case Disjunction(ns)       => ns.flatMap(loop).toSet
        case Sequence(ns)          => extract(ns) ++ ns.flatMap(loop).toSet
        case Capture(_, n)         => loop(n)
        case NamedCapture(_, _, n) => loop(n)
        case Group(n)              => loop(n)
        case Repeat(_, n)          => loop(n)
        case LookAhead(_, n)       => loop(n)
        case LookBehind(_, n)      => loop(n)
        case _                     => Set.empty

      val set = loop(pattern.node)
      if pattern.flagSet.ignoreCase then set.map(UString.canonicalize(_, pattern.flagSet.unicode)) else set

    /** Returns a maximum capture index or `0`. */
    def capturesSize: Int = pattern.node.captureRange.range.map(_._2).getOrElse(0)

  extension (node: Node)

    /** Returns a capture range within this node. */
    def captureRange: CaptureRange = node match
      case Disjunction(ns)           => ns.map(_.captureRange).foldLeft(CaptureRange(None))(_.merge(_))
      case Sequence(ns)              => ns.map(_.captureRange).foldLeft(CaptureRange(None))(_.merge(_))
      case Capture(index, n)         => CaptureRange(Some((index, index))).merge(n.captureRange)
      case NamedCapture(index, _, n) => CaptureRange(Some((index, index))).merge(n.captureRange)
      case Group(n)                  => n.captureRange
      case Repeat(_, n)              => n.captureRange
      case LookAhead(_, n)           => n.captureRange
      case LookBehind(_, n)          => n.captureRange
      case _                         => CaptureRange(None)

    /** Checks this node can match an empty string. */
    def canMatchEmpty: Boolean = node match
      case Disjunction(ns)       => ns.exists(_.canMatchEmpty)
      case Sequence(ns)          => ns.forall(_.canMatchEmpty)
      case Capture(_, n)         => n.canMatchEmpty
      case NamedCapture(_, _, n) => n.canMatchEmpty
      case Group(n)              => n.canMatchEmpty
      case Repeat(q, n)          =>
        q.normalized match
          case Quantifier.Exact(m, _)      => m == 0 || n.canMatchEmpty
          case Quantifier.Unbounded(m, _)  => m == 0 || n.canMatchEmpty
          case Quantifier.Bounded(m, _, _) => m == 0 || n.canMatchEmpty
      case WordBoundary(_) | LineBegin() | LineEnd()   => true
      case LookAhead(_, _) | LookBehind(_, _)          => true
      case BackReference(_) | NamedBackReference(_, _) => true
      case _                                           => false

  extension (atom: AtomNode)

    /** Converts this pattern to a corresponding interval set. */
    def toIChar(unicode: Boolean): IChar = atom match
      case Character(c)                 => IChar(c)
      case SimpleEscapeClass(invert, k) =>
        val char = k match
          case EscapeClassKind.Digit => IChar.Digit
          case EscapeClassKind.Word  => IChar.Word
          case EscapeClassKind.Space => IChar.Space
        if (invert) char.complement(unicode) else char
      case UnicodeProperty(_, _, contents)         => contents
      case UnicodePropertyValue(_, _, _, contents) => contents
      case CharacterClass(_, ns)                   => IChar.union(ns.map(_.toIChar(unicode)))
      case ClassRange(b, e)                        => IChar.range(b, e)
