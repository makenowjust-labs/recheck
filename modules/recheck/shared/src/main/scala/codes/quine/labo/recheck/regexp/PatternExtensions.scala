package codes.quine.labo.recheck.regexp

import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.chaining._

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.regexp.Pattern._
import codes.quine.labo.recheck.unicode.IChar
import codes.quine.labo.recheck.unicode.ICharSet
import codes.quine.labo.recheck.unicode.ICharSet.CharKind
import codes.quine.labo.recheck.unicode.UString
import codes.quine.labo.recheck.util.TryUtil

object PatternExtensions {

  /** CaptureRange is a range of capture indexes. */
  final case class CaptureRange(range: Option[(Int, Int)]) extends AnyVal {

    /** Merges two ranges. */
    def merge(other: CaptureRange): CaptureRange =
      (range, other.range) match {
        case (Some((min1, max1)), Some((min2, max2))) =>
          new CaptureRange(Some((Math.min(min1, min2), Math.max(max1, max2))))
        case (Some((min, max)), None) => CaptureRange(Some((min, max)))
        case (None, Some((min, max))) => CaptureRange(Some((min, max)))
        case (None, None)             => CaptureRange(None)
      }
  }

  implicit final class PatternOps(private val pattern: Pattern) extends AnyVal {
    import pattern._

    /** Tests the pattern has line-begin assertion `^` at its begin position. */
    def hasLineBeginAtBegin(implicit ctx: Context): Boolean =
      ctx.interrupt {
        def loop(node: Node): Boolean = ctx.interrupt(node match {
          case Disjunction(ns)       => ns.forall(loop)
          case Sequence(ns)          => ns.headOption.exists(loop)
          case Capture(_, n)         => loop(n)
          case NamedCapture(_, _, n) => loop(n)
          case Group(n)              => loop(n)
          case LineBegin()           => true
          case _                     => false
        })
        !flagSet.multiline && loop(node)
      }

    /** Tests the pattern has line-end assertion `$` at its end position. */
    def hasLineEndAtEnd(implicit ctx: Context): Boolean =
      ctx.interrupt {
        def loop(node: Node): Boolean = ctx.interrupt(node match {
          case Disjunction(ns)       => ns.forall(loop)
          case Sequence(ns)          => ns.lastOption.exists(loop)
          case Capture(_, n)         => loop(n)
          case NamedCapture(_, _, n) => loop(n)
          case Group(n)              => loop(n)
          case LineEnd()             => true
          case _                     => false
        })
        !flagSet.multiline && loop(node)
      }

    /** Tests the pattern has no infinite repetition. */
    def isConstant: Boolean = {
      def loop(node: Node): Boolean = node match {
        case Disjunction(ns)             => ns.forall(loop)
        case Sequence(ns)                => ns.forall(loop)
        case Capture(_, n)               => loop(n)
        case NamedCapture(_, _, n)       => loop(n)
        case Group(n)                    => loop(n)
        case Star(_, _)                  => false
        case Plus(_, _)                  => false
        case Question(_, n)              => loop(n)
        case Repeat(_, _, Some(None), _) => false
        case Repeat(_, _, _, n)          => loop(n)
        case LookAhead(_, n)             => loop(n)
        case LookBehind(_, n)            => loop(n)
        case _                           => true
      }
      loop(node)
    }

    /** Returns this pattern's size. */
    def size: Int = {
      def loop(node: Node): Int = node match {
        case Disjunction(ns)                => ns.map(loop).sum + ns.size - 1
        case Sequence(ns)                   => ns.map(loop).sum
        case Capture(_, n)                  => loop(n)
        case NamedCapture(_, _, n)          => loop(n)
        case Group(n)                       => loop(n)
        case Star(_, n)                     => loop(n) + 1
        case Plus(_, n)                     => loop(n) + 1
        case Question(_, n)                 => loop(n) + 1
        case Repeat(_, m, None, n)          => loop(n) * m
        case Repeat(_, m, Some(None), n)    => loop(n) * (m + 1) + 1
        case Repeat(_, m, Some(Some(l)), n) => loop(n) * l + (l - m)
        case LookAhead(_, n)                => loop(n) + 1
        case LookBehind(_, n)               => loop(n) + 1
        case _                              => 1
      }
      loop(node)
    }

    /** Computes alphabet from this pattern. */
    def alphabet(implicit ctx: Context): Try[ICharSet] =
      ctx.interrupt {
        val FlagSet(_, ignoreCase, _, dotAll, unicode, _) = flagSet
        val set = ICharSet
          .any(ignoreCase, unicode)
          .pipe(set =>
            if (needsLineTerminatorDistinction) set.add(IChar.LineTerminator, CharKind.LineTerminator) else set
          )
          .pipe(set => if (needsWordDistinction) set.add(IChar.Word, CharKind.Word) else set)

        def loop(node: Node): Try[Seq[IChar]] = ctx.interrupt(node match {
          case Disjunction(ns)       => TryUtil.traverse(ns)(loop).map(_.flatten)
          case Sequence(ns)          => TryUtil.traverse(ns)(loop).map(_.flatten)
          case Capture(_, n)         => loop(n)
          case NamedCapture(_, _, n) => loop(n)
          case Group(n)              => loop(n)
          case Star(_, n)            => loop(n)
          case Plus(_, n)            => loop(n)
          case Question(_, n)        => loop(n)
          case Repeat(_, _, _, n)    => loop(n)
          case LookAhead(_, n)       => loop(n)
          case LookBehind(_, n)      => loop(n)
          case atom: AtomNode =>
            atom.toIChar(unicode).map { ch =>
              Vector(if (ignoreCase) IChar.canonicalize(ch, unicode) else ch)
            }
          case Dot() => Success(Vector(IChar.dot(ignoreCase, dotAll, unicode)))
          case _     => Success(Vector.empty)
        })

        loop(node).map(_.foldLeft(set)(_.add(_)))
      }

    /** Tests whether the pattern needs line terminator distinction or not. */
    private[regexp] def needsLineTerminatorDistinction: Boolean = {
      def loop(node: Node): Boolean = node match {
        case Disjunction(ns)         => ns.exists(loop)
        case Sequence(ns)            => ns.exists(loop)
        case Capture(_, n)           => loop(n)
        case NamedCapture(_, _, n)   => loop(n)
        case Group(n)                => loop(n)
        case Star(_, n)              => loop(n)
        case Plus(_, n)              => loop(n)
        case Question(_, n)          => loop(n)
        case Repeat(_, _, _, n)      => loop(n)
        case LookAhead(_, n)         => loop(n)
        case LookBehind(_, n)        => loop(n)
        case LineBegin() | LineEnd() => true
        case _                       => false
      }
      flagSet.multiline && loop(node)
    }

    /** Tests whether the pattern needs word character distinction or not. */
    private[regexp] def needsWordDistinction: Boolean = {
      def loop(node: Node): Boolean = node match {
        case Disjunction(ns)       => ns.exists(loop)
        case Sequence(ns)          => ns.exists(loop)
        case Capture(_, n)         => loop(n)
        case NamedCapture(_, _, n) => loop(n)
        case Group(n)              => loop(n)
        case Star(_, n)            => loop(n)
        case Plus(_, n)            => loop(n)
        case Question(_, n)        => loop(n)
        case Repeat(_, _, _, n)    => loop(n)
        case LookAhead(_, n)       => loop(n)
        case LookBehind(_, n)      => loop(n)
        case WordBoundary(_)       => true
        case _                     => false
      }
      loop(node)
    }

    /** Extracts parts within the pattern. */
    def parts: Set[UString] = {
      @tailrec
      def extract(ns: Seq[Node], set: Set[UString] = Set.empty): Set[UString] =
        if (ns.isEmpty) set
        else {
          val (pre, suf) = ns.span(_.isInstanceOf[Character])
          val s = UString(pre.collect { case Character(c) => c.asString }.mkString)
          val newSet = set ++ (if (s.sizeAsString > 1) Set(s) else Set.empty)
          extract(suf.dropWhile(!_.isInstanceOf[Character]), newSet)
        }

      def loop(node: Node): Set[UString] = node match {
        case Disjunction(ns)       => ns.flatMap(loop).toSet
        case Sequence(ns)          => extract(ns) ++ ns.flatMap(loop).toSet
        case Capture(_, n)         => loop(n)
        case NamedCapture(_, _, n) => loop(n)
        case Group(n)              => loop(n)
        case Star(_, n)            => loop(n)
        case Plus(_, n)            => loop(n)
        case Question(_, n)        => loop(n)
        case Repeat(_, _, _, n)    => loop(n)
        case LookAhead(_, n)       => loop(n)
        case LookBehind(_, n)      => loop(n)
        case _                     => Set.empty
      }

      val set = loop(node)
      if (flagSet.ignoreCase) set.map(UString.canonicalize(_, flagSet.unicode)) else set
    }

    /** Returns a maximum capture index or `0`. */
    def capturesSize: Int = node.captureRange.range.map(_._2).getOrElse(0)

    /** Extracts capture names of the pattern. */
    def names: Try[Map[String, Int]] = {
      def merge(tm1: Try[Map[String, Int]], m2: Map[String, Int]): Try[Map[String, Int]] =
        tm1.flatMap { m1 =>
          if (m1.keySet.intersect(m2.keySet).nonEmpty) Failure(new InvalidRegExpException("duplicated named capture"))
          else Success(m1 ++ m2)
        }

      def loop(node: Node): Try[Map[String, Int]] = node match {
        case Disjunction(ns) =>
          TryUtil.traverse(ns)(loop).flatMap(_.foldLeft(Try(Map.empty[String, Int]))(merge))
        case Sequence(ns) =>
          TryUtil.traverse(ns)(loop).flatMap(_.foldLeft(Try(Map.empty[String, Int]))(merge))
        case Capture(_, n)                => loop(n)
        case NamedCapture(index, name, n) => loop(n).map(_ + (name -> index))
        case Group(n)                     => loop(n)
        case Star(_, n)                   => loop(n)
        case Plus(_, n)                   => loop(n)
        case Question(_, n)               => loop(n)
        case Repeat(_, _, _, n)           => loop(n)
        case LookAhead(_, n)              => loop(n)
        case LookBehind(_, n)             => loop(n)
        case _                            => Success(Map.empty)
      }

      loop(node)
    }
  }

  implicit final class NodeOps(private val node: Node) extends AnyVal {

    /** Returns a capture range within this node. */
    def captureRange: CaptureRange = node match {
      case Disjunction(ns)           => ns.map(_.captureRange).foldLeft(CaptureRange(None))(_.merge(_))
      case Sequence(ns)              => ns.map(_.captureRange).foldLeft(CaptureRange(None))(_.merge(_))
      case Capture(index, n)         => CaptureRange(Some((index, index))).merge(n.captureRange)
      case NamedCapture(index, _, n) => CaptureRange(Some((index, index))).merge(n.captureRange)
      case Group(n)                  => n.captureRange
      case Star(_, n)                => n.captureRange
      case Plus(_, n)                => n.captureRange
      case Question(_, n)            => n.captureRange
      case Repeat(_, _, _, n)        => n.captureRange
      case LookAhead(_, n)           => n.captureRange
      case LookBehind(_, n)          => n.captureRange
      case _                         => CaptureRange(None)
    }

    /** Checks this node can match an empty string. */
    def isEmpty: Boolean = node match {
      case Disjunction(ns)                           => ns.exists(_.isEmpty)
      case Sequence(ns)                              => ns.forall(_.isEmpty)
      case Capture(_, n)                             => n.isEmpty
      case NamedCapture(_, _, n)                     => n.isEmpty
      case Group(n)                                  => n.isEmpty
      case Star(_, _)                                => true
      case Plus(_, n)                                => n.isEmpty
      case Question(_, _)                            => true
      case Repeat(_, min, _, n)                      => min == 0 || n.isEmpty
      case WordBoundary(_) | LineBegin() | LineEnd() => true
      case LookAhead(_, _) | LookBehind(_, _)        => true
      case BackReference(_) | NamedBackReference(_)  => true
      case _                                         => false
    }
  }

  implicit final class AtomNodeOps(private val atom: AtomNode) extends AnyVal {

    /** Converts this pattern to a corresponding interval set. */
    def toIChar(unicode: Boolean): Try[IChar] = atom match {
      case Character(c) => Success(IChar(c))
      case SimpleEscapeClass(invert, k) =>
        val char = k match {
          case EscapeClassKind.Digit => IChar.Digit
          case EscapeClassKind.Word  => IChar.Word
          case EscapeClassKind.Space => IChar.Space
        }
        Success(if (invert) char.complement(unicode) else char)
      case UnicodeProperty(invert, name) =>
        IChar.UnicodeProperty(name) match {
          case Some(char) => Success(if (invert) char.complement(unicode) else char)
          case None       => Failure(new InvalidRegExpException(s"unknown Unicode property: $name"))
        }
      case UnicodePropertyValue(invert, name, value) =>
        IChar.UnicodePropertyValue(name, value) match {
          case Some(char) => Success(if (invert) char.complement(unicode) else char)
          case None       => Failure(new InvalidRegExpException(s"unknown Unicode property-value: $name=$value"))
        }
      case CharacterClass(_, ns) =>
        // Inversion will be done in automaton translation instead of here.
        TryUtil.traverse(ns)(_.toIChar(unicode)).map(IChar.union)
      case ClassRange(b, e) =>
        val char = IChar.range(b, e)
        if (char.isEmpty) Failure(new InvalidRegExpException("an empty range"))
        else Success(char)
    }
  }
}
