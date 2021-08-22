package codes.quine.labo.recheck.regexp

import scala.collection.mutable

import codes.quine.labo.recheck.regexp.Pattern._
import codes.quine.labo.recheck.unicode.UChar

/** Pattern is ECMA-262 `RegExp` pattern. */
final case class Pattern(node: Node, flagSet: FlagSet) {
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
  sealed abstract class Node extends Cloneable with Serializable with Product {

    /** A internal field of the position of this node. */
    private var _loc: Option[(Int, Int)] = None

    /** Returns the location of this node. */
    def loc: Option[(Int, Int)] = _loc

    /** Returns a new node with the given position. */
    private[regexp] def withLoc(start: Int, end: Int): this.type = {
      // If the position is already set, it returns `this` instead of an allocation.
      if (loc.exists { case (s, e) => s == start && e == end }) this
      else {
        val cloned = clone().asInstanceOf[this.type]
        cloned._loc = Some((start, end))
        cloned
      }
    }

    /** Copies the location from the given node if possible. */
    private[regexp] def withLoc(node: Node): this.type =
      node.loc match {
        case Some((start, end)) => withLoc(start, end)
        case None               => this
      }
  }

  /** AtomNode is a node of pattern to match a character. */
  sealed trait AtomNode extends Serializable with Product

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

  /** Star is a zero-or-more repetition pattern. (e.g. <code>/x*&#47;</code> or <code>/x*?/</code>) */
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
  final case class LineBegin() extends Node

  /** LineEnd is an end-of-line assertion pattern. (e.g. `/$/`) */
  final case class LineEnd() extends Node

  /** LookAhead is a look-ahead assertion of a pattern. (e.g. `/(?=x)/` or `/(?!x)/`) */
  final case class LookAhead(negative: Boolean, child: Node) extends Node

  /** LookBehind is a look-behind assertion of a pattern. (e.g. `/(?<=x)/` or `/(?<!x)/`) */
  final case class LookBehind(negative: Boolean, child: Node) extends Node

  /** Character is a single character in pattern. (e.g. `/x/`) */
  final case class Character(value: UChar) extends Node with ClassNode

  /** SimpleEscapeClass is an escape class. (e.g. `/\w/` or `/\s/`) */
  final case class SimpleEscapeClass(invert: Boolean, kind: EscapeClassKind) extends Node with ClassNode

  /** UnicodeProperty is an escape class of Unicode property. (e.g. `/\p{ASCII}/` or `/\P{L}/`) */
  final case class UnicodeProperty(invert: Boolean, name: String) extends Node with ClassNode

  /** UnicodePropertyValue is an escape class of Unicode property and value. (e.g. `/\p{sc=Hira}/` or
    * `/\P{General_Category=No}/`)
    */
  final case class UnicodePropertyValue(invert: Boolean, name: String, value: String) extends Node with ClassNode

  /** CharacterClass is a class (set) pattern of characters. (e.g. `/[a-z]/` or `/[^A-Z]/`) */
  final case class CharacterClass(invert: Boolean, children: Seq[ClassNode]) extends Node with AtomNode

  /** ClassRange is a character range pattern in a class. */
  final case class ClassRange(begin: UChar, end: UChar) extends ClassNode

  /** Dot is any characters pattern. (e.g. `/./`)
    *
    * This does not inherit `AtomNode` intentionally because this `toIChar` needs `dotAll` flag information.
    */
  final case class Dot() extends Node

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
    case Disjunction(ns)                        => ns.map(showNodeInDisjunction).mkString("|")
    case Sequence(ns)                           => ns.map(showNodeInSequence).mkString
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
    case LineBegin()                            => "^"
    case LineEnd()                              => "$"
    case LookAhead(false, n)                    => s"(?=${showNode(n)})"
    case LookAhead(true, n)                     => s"(?!${showNode(n)})"
    case LookBehind(false, n)                   => s"(?<=${showNode(n)})"
    case LookBehind(true, n)                    => s"(?<!${showNode(n)})"
    case Character(u)                           => showUChar(u)
    case CharacterClass(false, items)           => s"[${items.map(showClassNode).mkString}]"
    case CharacterClass(true, items)            => s"[^${items.map(showClassNode).mkString}]"
    case SimpleEscapeClass(false, k)            => EscapeClassKind.showEscapeClassKind(k)
    case SimpleEscapeClass(true, k)             => EscapeClassKind.showEscapeClassKind(k).toUpperCase
    case UnicodeProperty(false, p)              => s"\\p{$p}"
    case UnicodeProperty(true, p)               => s"\\P{$p}"
    case UnicodePropertyValue(false, p, v)      => s"\\p{$p=$v}"
    case UnicodePropertyValue(true, p, v)       => s"\\P{$p=$v}"
    case Dot()                                  => "."
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
      case _: Disjunction | _: Sequence | _: Star | _: Plus | _: Question | _: Repeat | _: WordBoundary | _: LineBegin |
          _: LineEnd | _: LookAhead | _: LookBehind =>
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
