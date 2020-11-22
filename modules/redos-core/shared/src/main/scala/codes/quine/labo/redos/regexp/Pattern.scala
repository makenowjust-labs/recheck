package codes.quine.labo.redos
package regexp

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.chaining._

import Pattern._
import data.IChar
import data.ICharSet
import data.UChar
import data.UString
import util.TryUtil
import util.Timeout

/** Pattern is ECMA-262 `RegExp` pattern. */
final case class Pattern(node: Node, flagSet: FlagSet) {

  /** Tests the pattern has line-begin assertion `^` at its begin position. */
  def hasLineBeginAtBegin: Boolean = {
    def loop(node: Node): Boolean = node match {
      case Disjunction(ns)       => ns.forall(loop)
      case Sequence(ns)          => ns.headOption.exists(loop)
      case Capture(_, n)         => loop(n)
      case NamedCapture(_, _, n) => loop(n)
      case Group(n)              => loop(n)
      case LineBegin             => true
      case _                     => false
    }
    !flagSet.multiline && loop(node)
  }

  /** Tests the pattern has line-end assertion `$` at its end position. */
  def hasLineEndAtEnd: Boolean = {
    def loop(node: Node): Boolean = node match {
      case Disjunction(ns)       => ns.forall(loop)
      case Sequence(ns)          => ns.lastOption.exists(loop)
      case Capture(_, n)         => loop(n)
      case NamedCapture(_, _, n) => loop(n)
      case Group(n)              => loop(n)
      case LineEnd               => true
      case _                     => false
    }
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
      case Repeat(_, m, Some(None), n)    => loop(n) * m + 1
      case Repeat(_, m, Some(Some(l)), n) => loop(n) * l + (l - m)
      case LookAhead(_, n)                => loop(n) + 1
      case LookBehind(_, n)               => loop(n) + 1
      case _                              => 1
    }
    loop(node)
  }

  /** Computes alphabet from this pattern. */
  def alphabet(implicit timeout: Timeout = Timeout.NoTimeout): Try[ICharSet] =
    timeout.checkTimeout("regexp.Pattern#alphabet") {
      val FlagSet(_, ignoreCase, _, dotAll, unicode, _) = flagSet
      val set = ICharSet
        .any(ignoreCase, unicode)
        .pipe(set => if (needsLineTerminatorDistinction) set.add(IChar.LineTerminator.withLineTerminator) else set)
        .pipe(set => if (needsWordDistinction) set.add(IChar.Word.withWord) else set)

      def loop(node: Node): Try[Seq[IChar]] = timeout.checkTimeout("regexp.Pattern#alphabet:loop")(node match {
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
        case Dot => Success(Vector(IChar.dot(ignoreCase, dotAll, unicode)))
        case _   => Success(Vector.empty)
      })

      loop(node).map(_.foldLeft(set)(_.add(_)))
    }

  /** Tests whether the pattern needs line terminator distinction or not. */
  private[regexp] def needsLineTerminatorDistinction: Boolean = {
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
      case LineBegin | LineEnd   => true
      case _                     => false
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
        val s = UString(pre.collect { case Character(c) => c }.toIndexedSeq)
        val newSet = set ++ (if (s.size > 1) Set(s) else Set.empty)
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

  override def toString: String =
    s"/${showNode(node)}/${showFlagSet(flagSet)}"
}

/** Pattern nodes and utilities. */
object Pattern {

  /** FlagSet is a set of flag of pattern.
    *
    *   - `global` is `g` flag.
    *   - `ignoreCase` is `i` flag.
    *   - `multiline` is `m` flag.
    *   - `dotAll` is `s` flag.
    *   - `unicode` is `u` flag.
    *   - `sticky` is `y` flag.
    */
  final case class FlagSet(
      global: Boolean,
      ignoreCase: Boolean,
      multiline: Boolean,
      dotAll: Boolean,
      unicode: Boolean,
      sticky: Boolean
  )

  /** Node is a node of pattern [[https://en.wikipedia.org/wiki/Abstract_syntax_tree AST (abstract syntax tree)]]. */
  sealed abstract class Node extends Serializable with Product

  /** AtomNode is a node of pattern to match a character. */
  sealed trait AtomNode extends Serializable with Product {

    /** Converts this pattern to a corresponding interval set. */
    def toIChar(unicode: Boolean): Try[IChar]
  }

  /** ClassNode is a node of pattern AST, but it can appear as a class child.
    *
    * This type does not inherit [[Node]] because [[ClassRange]] can appear as a class child only.
    */
  sealed trait ClassNode extends AtomNode

  /** Disjunction is a disjunction of patterns. (e.g. `/x|y|z/`) */
  final case class Disjunction(children: Seq[Node]) extends Node

  /** Sequence is a sequence of patterns. (e.g. `/xyz/`) */
  final case class Sequence(children: Seq[Node]) extends Node

  /** Capture is a capture pattern. (e.g. `/(x)/`) */
  final case class Capture(index: Int, child: Node) extends Node

  /** NamedCapture is a named capture pattern. (e.g. `/(?<foo>x)/`) */
  final case class NamedCapture(index: Int, name: String, child: Node) extends Node

  /** Group is a grouping of a pattern. (e.g. `/(?:x)/`) */
  final case class Group(child: Node) extends Node

  /** Star is a zero-or-more repetition pattern. (e.g. `/x*&#47; or `/x*?/`) */
  final case class Star(nonGreedy: Boolean, child: Node) extends Node

  /** Plus is a one-or-more repetition pattern. (e.g. `/x+/` or `/x+?/`) */
  final case class Plus(nonGreedy: Boolean, child: Node) extends Node

  /** Question is a zero-or-one matching pattern. (e.g. `/x?/` or `/x??/`) */
  final case class Question(nonGreedy: Boolean, child: Node) extends Node

  /** Repeat is a repetition pattern. (e.g. `/x{1,2}/`)
    *
    *   - If `max` is `None`, it means `max` is missing (i.e. fixed count repetition).
    *   - If `max` is `Some(None)`, it means `max` is not specified (i.e. unlimited repetition).
    *   - If `max` is `Some(n)`, it means `max` is specified.
    */
  final case class Repeat(nonGreedy: Boolean, min: Int, max: Option[Option[Int]], child: Node) extends Node

  /** WordBoundary is a word-boundary assertion pattern. (e.g. `/\b/` or `/\B/`) */
  final case class WordBoundary(invert: Boolean) extends Node

  /** LineBegin is a begin-of-line assertion pattern. (e.g. `/^/`) */
  case object LineBegin extends Node

  /** LineEnd is an end-of-line assertion pattern. (e.g. `/$/`) */
  case object LineEnd extends Node

  /** LookAhead is a look-ahead assertion of a pattern. (e.g. `/(?=x)/` or `/(?!x)/`) */
  final case class LookAhead(negative: Boolean, child: Node) extends Node

  /** LookBehind is a look-behind assertion of a pattern. (e.g. `/(?<=x)/` or `/(?<!x)/`) */
  final case class LookBehind(negative: Boolean, child: Node) extends Node

  /** Character is a single character in pattern. (e.g. `/x/`) */
  final case class Character(value: UChar) extends Node with ClassNode {
    def toIChar(unicode: Boolean): Try[IChar] = Success(IChar(value))
  }

  /** SimpleEscapeClass is an escape class. (e.g. `/\w/` or `/\s/`) */
  final case class SimpleEscapeClass(invert: Boolean, kind: EscapeClassKind) extends Node with ClassNode {
    def toIChar(unicode: Boolean): Try[IChar] = {
      val char = kind match {
        case EscapeClassKind.Digit => IChar.Digit
        case EscapeClassKind.Word  => IChar.Word
        case EscapeClassKind.Space => IChar.Space
      }
      Success(if (invert) char.complement(unicode) else char)
    }
  }

  /** UnicodeProperty is an escape class of Unicode property. (e.g. `/\p{ASCII}/` or `/\P{L}/`) */
  final case class UnicodeProperty(invert: Boolean, name: String) extends Node with ClassNode {
    def toIChar(unicode: Boolean): Try[IChar] = IChar.UnicodeProperty(name) match {
      case Some(char) => Success(if (invert) char.complement(unicode) else char)
      case None       => Failure(new InvalidRegExpException(s"unknown Unicode property: $name"))
    }
  }

  /** UnicodePropertyValue is an escape class of Unicode property and value.
    * (e.g. `/\p{sc=Hira}/` or `/\P{General_Category=No}/`)
    */
  final case class UnicodePropertyValue(invert: Boolean, name: String, value: String) extends Node with ClassNode {
    def toIChar(unicode: Boolean): Try[IChar] = IChar.UnicodePropertyValue(name, value) match {
      case Some(char) => Success(if (invert) char.complement(unicode) else char)
      case None       => Failure(new InvalidRegExpException(s"unknown Unicode property-value: $name=$value"))
    }
  }

  /** CharacterClass is a class (set) pattern of characters. (e.g. `/[a-z]/` or `/[^A-Z]/`) */
  final case class CharacterClass(invert: Boolean, children: Seq[ClassNode]) extends Node with AtomNode {
    def toIChar(unicode: Boolean): Try[IChar] =
      // Inversion will be done in automaton translation instead of here.
      TryUtil.traverse(children)(_.toIChar(unicode)).map(IChar.union(_))
  }

  /** ClassRange is a character rane pattern in a class. */
  final case class ClassRange(begin: UChar, end: UChar) extends ClassNode {
    def toIChar(unicode: Boolean): Try[IChar] = {
      val char = IChar.range(begin, end)
      if (char.isEmpty) Failure(new InvalidRegExpException("an empty range"))
      else Success(char)
    }
  }

  /** Dot is any characters pattern. (e.g. `/./`)
    *
    * This does not inherit `AtomNode` intentionally because this `toIChar` needs `dotAll` flag information.
    */
  case object Dot extends Node

  /** BackReference is a back-reference pattern. (e.g. `/\1/`) */
  final case class BackReference(index: Int) extends Node

  /** NamedBackReference is a back-reference pattern. (e.g. `/\k<foo>/`) */
  final case class NamedBackReference(name: String) extends Node

  /** EscapeClassKind is a kind of [[SimpleEscapeClass]]. */
  sealed abstract class EscapeClassKind extends Serializable with Product

  /** EscapeClassKind values. */
  object EscapeClassKind {

    /** Digit is a `\d` escape class. */
    case object Digit extends EscapeClassKind

    /** Digit is a `\w` escape class. */
    case object Word extends EscapeClassKind

    /** Digit is a `\s` escape class. */
    case object Space extends EscapeClassKind

    /** Show an escape class as a pattern. */
    private[Pattern] def showEscapeClassKind(kind: EscapeClassKind): String = kind match {
      case Digit => "\\d"
      case Word  => "\\w"
      case Space => "\\s"
    }
  }

  /** Shows a [[Node]] as a pattern. */
  private[regexp] def showNode(node: Node): String = node match {
    case Disjunction(ns)                        => ns.map(showNodeInDisjunction(_)).mkString("|")
    case Sequence(ns)                           => ns.map(showNodeInSequence(_)).mkString
    case Capture(_, n)                          => s"(${showNode(n)})"
    case NamedCapture(_, name, n)               => s"(?<$name>${showNode(n)})"
    case Group(n)                               => s"(?:${showNode(n)})"
    case Star(false, n)                         => s"${showNodeInRepeat(n)}*"
    case Star(true, n)                          => s"${showNodeInRepeat(n)}*?"
    case Plus(false, n)                         => s"${showNodeInRepeat(n)}+"
    case Plus(true, n)                          => s"${showNodeInRepeat(n)}+?"
    case Question(false, n)                     => s"${showNodeInRepeat(n)}?"
    case Question(true, n)                      => s"${showNodeInRepeat(n)}??"
    case Repeat(false, min, None, n)            => s"${showNodeInRepeat(n)}{$min}"
    case Repeat(true, min, None, n)             => s"${showNodeInRepeat(n)}{$min}?"
    case Repeat(false, min, Some(None), n)      => s"${showNodeInRepeat(n)}{$min,}"
    case Repeat(true, min, Some(None), n)       => s"${showNodeInRepeat(n)}{$min,}?"
    case Repeat(false, min, Some(Some(max)), n) => s"${showNodeInRepeat(n)}{$min,$max}"
    case Repeat(true, min, Some(Some(max)), n)  => s"${showNodeInRepeat(n)}{$min,$max}?"
    case WordBoundary(false)                    => "\\b"
    case WordBoundary(true)                     => "\\B"
    case LineBegin                              => "^"
    case LineEnd                                => "$"
    case LookAhead(false, n)                    => s"(?=${showNode(n)})"
    case LookAhead(true, n)                     => s"(?!${showNode(n)})"
    case LookBehind(false, n)                   => s"(?<=${showNode(n)})"
    case LookBehind(true, n)                    => s"(?<!${showNode(n)})"
    case Character(u)                           => showUChar(u)
    case CharacterClass(false, items)           => s"[${items.map(showClassNode(_)).mkString}]"
    case CharacterClass(true, items)            => s"[^${items.map(showClassNode(_)).mkString}]"
    case SimpleEscapeClass(false, k)            => EscapeClassKind.showEscapeClassKind(k)
    case SimpleEscapeClass(true, k)             => EscapeClassKind.showEscapeClassKind(k).toUpperCase
    case UnicodeProperty(false, p)              => s"\\p{$p}"
    case UnicodeProperty(true, p)               => s"\\P{$p}"
    case UnicodePropertyValue(false, p, v)      => s"\\p{$p=$v}"
    case UnicodePropertyValue(true, p, v)       => s"\\P{$p=$v}"
    case Dot                                    => "."
    case BackReference(i)                       => s"\\$i"
    case NamedBackReference(name)               => s"\\k<$name>"
  }

  /** Shows a node as a [[Disjunction]] child.
    *
    * It wraps a node by parentheses if needed.
    */
  private def showNodeInDisjunction(node: Node): String =
    node match {
      case _: Disjunction => s"(?:${showNode(node)})"
      case _              => showNode(node)
    }

  /** Shows a node as a [[Sequence]] child.
    *
    * It wraps a node by parentheses if needed.
    */
  private def showNodeInSequence(node: Node): String =
    node match {
      case _: Disjunction | _: Sequence => s"(?:${showNode(node)})"
      case _                            => showNode(node)
    }

  /** Shows a node as a [[Repeat]] and similar classes child.
    *
    * It wraps a node by parentheses if needed.
    */
  private def showNodeInRepeat(node: Node): String =
    node match {
      case _: Disjunction | _: Sequence | _: Star | _: Plus | _: Question | _: Repeat | _: WordBoundary | LineBegin |
          LineEnd | _: LookAhead | _: LookBehind =>
        s"(?:${showNode(node)})"
      case _ => showNode(node)
    }

  /** Shows a [[ClassNode]] as a pattern. */
  private def showClassNode(node: ClassNode): String =
    node match {
      case Character(u)       => showUCharInClass(u)
      case ClassRange(u1, u2) => s"${showUCharInClass(u1)}-${showUCharInClass(u2)}"
      case node: Node         => showNode(node)
    }

  /** Shows a [[FlagSet]] as a pattern. */
  private[regexp] def showFlagSet(flagSet: FlagSet): String = {
    val sb = new mutable.StringBuilder
    if (flagSet.global) sb.append('g')
    if (flagSet.ignoreCase) sb.append('i')
    if (flagSet.multiline) sb.append('m')
    if (flagSet.dotAll) sb.append('s')
    if (flagSet.unicode) sb.append('u')
    if (flagSet.sticky) sb.append('y')
    sb.result()
  }

  /** Shows a character as a pattern. */
  private def showUChar(u: UChar): String =
    if (u.value.isValidChar && "^$\\.*+?()[]{}|/".contains(u.value.toChar)) s"\\${u.value.toChar}"
    else u.toString

  /** Shows a character as a class in pattern. */
  private def showUCharInClass(u: UChar): String =
    if (u.value.isValidChar && "[^-]".contains(u.value.toChar)) s"\\${u.value.toChar}"
    else u.toString
}
