package codes.quine.labs.recheck.regexp

import scala.collection.mutable

import codes.quine.labs.recheck.regexp.Pattern._
import codes.quine.labs.recheck.unicode.IChar
import codes.quine.labs.recheck.unicode.UChar

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

  /** Location is a location of a node in a source. */
  final case class Location(start: Int, end: Int)

  /** HasLocation is a base class having a location. */
  trait HasLocation extends Cloneable {

    /** A internal field of the position of this node. */
    private var _loc: Option[Location] = None

    /** Returns the location of this node. */
    def loc: Option[Location] = _loc

    /** Returns a new node with the given position. */
    private[regexp] def withLoc(start: Int, end: Int): this.type = {
      // If the position is already set, it returns `this` immediately, for avoiding allocation.
      if (loc.exists { case Location(s, e) => s == start && e == end }) this
      else {
        val cloned = clone().asInstanceOf[this.type]
        cloned._loc = Some(Location(start, end))
        cloned
      }
    }

    /** Copies the location from the given node if possible. */
    private[regexp] def withLoc(node: HasLocation): this.type =
      node.loc match {
        case Some(Location(start, end)) => withLoc(start, end)
        case None                       => this
      }

    override protected def clone(): AnyRef = super.clone()
  }

  /** Node is a node of pattern [[https://en.wikipedia.org/wiki/Abstract_syntax_tree AST (abstract syntax tree)]]. */
  sealed abstract class Node extends HasLocation with Serializable with Product

  /** AtomNode is a node of pattern to match a character. */
  sealed trait AtomNode extends HasLocation with Serializable with Product

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

  /** Repeat is a repetition pattern. */
  final case class Repeat(quantifier: Quantifier, child: Node) extends Node

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
  final case class UnicodeProperty(invert: Boolean, name: String, contents: IChar) extends Node with ClassNode

  /** UnicodePropertyValue is an escape class of Unicode property and value. (e.g. `/\p{sc=Hira}/` or
    * `/\P{General_Category=No}/`)
    */
  final case class UnicodePropertyValue(invert: Boolean, name: String, value: String, contents: IChar)
      extends Node
      with ClassNode

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
  final case class NamedBackReference(index: Int, name: String) extends Node

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

  /** Quantifier is a repetition quantifier. */
  sealed abstract class Quantifier extends HasLocation with Serializable with Product {

    /** Checks backtracking strategy of this quantifier is lazy. */
    def isLazy: Boolean

    /** Returns a normalized version of this quantifier. */
    def normalized: NormalizedQuantifier = this match {
      case Quantifier.Star(isLazy)                            => Quantifier.Unbounded(0, isLazy).withLoc(this)
      case Quantifier.Plus(isLazy)                            => Quantifier.Unbounded(1, isLazy).withLoc(this)
      case Quantifier.Question(isLazy)                        => Quantifier.Bounded(0, 1, isLazy).withLoc(this)
      case q: Quantifier.Exact                                => q
      case q: Quantifier.Unbounded                            => q
      case Quantifier.Bounded(min, max, isLazy) if min == max => Quantifier.Exact(min, isLazy).withLoc(this)
      case q: Quantifier.Bounded                              => q
    }
  }

  /** NormalizedQuantifier is a normalized version of [[Quantifier]]. */
  sealed abstract class NormalizedQuantifier extends Quantifier

  object Quantifier {

    /** Star is a star quantifier `*`. */
    final case class Star(isLazy: Boolean) extends Quantifier

    /** Plus is a plus quantifier `+`. */
    final case class Plus(isLazy: Boolean) extends Quantifier

    /** Question is a question quantifier `?`. */
    final case class Question(isLazy: Boolean) extends Quantifier

    /** Exact is an exact repetition quantifier `{n}`. */
    final case class Exact(n: Int, isLazy: Boolean) extends NormalizedQuantifier

    /** Unbounded is an unbounded repetition quantifier `{min,}`. */
    final case class Unbounded(min: Int, isLazy: Boolean) extends NormalizedQuantifier

    /** Bounded is a bonded repetition quantifier `{min,max}`. */
    final case class Bounded(min: Int, max: Int, isLazy: Boolean) extends NormalizedQuantifier
  }

  /** Shows a [[Node]] as a pattern. */
  private[regexp] def showNode(node: Node): String = node match {
    case Disjunction(ns)                      => ns.map(showNodeInDisjunction).mkString("|")
    case Sequence(ns)                         => ns.map(showNodeInSequence).mkString
    case Capture(_, n)                        => s"(${showNode(n)})"
    case NamedCapture(_, name, n)             => s"(?<$name>${showNode(n)})"
    case Group(n)                             => s"(?:${showNode(n)})"
    case Repeat(q, n)                         => s"${showNodeInRepeat(n)}${showQuantifier(q)}"
    case WordBoundary(false)                  => "\\b"
    case WordBoundary(true)                   => "\\B"
    case LineBegin()                          => "^"
    case LineEnd()                            => "$"
    case LookAhead(false, n)                  => s"(?=${showNode(n)})"
    case LookAhead(true, n)                   => s"(?!${showNode(n)})"
    case LookBehind(false, n)                 => s"(?<=${showNode(n)})"
    case LookBehind(true, n)                  => s"(?<!${showNode(n)})"
    case Character(u)                         => showUChar(u)
    case CharacterClass(false, items)         => s"[${items.map(showClassNode).mkString}]"
    case CharacterClass(true, items)          => s"[^${items.map(showClassNode).mkString}]"
    case SimpleEscapeClass(false, k)          => EscapeClassKind.showEscapeClassKind(k)
    case SimpleEscapeClass(true, k)           => EscapeClassKind.showEscapeClassKind(k).toUpperCase
    case UnicodeProperty(false, p, _)         => s"\\p{$p}"
    case UnicodeProperty(true, p, _)          => s"\\P{$p}"
    case UnicodePropertyValue(false, p, v, _) => s"\\p{$p=$v}"
    case UnicodePropertyValue(true, p, v, _)  => s"\\P{$p=$v}"
    case Dot()                                => "."
    case BackReference(i)                     => s"\\$i"
    case NamedBackReference(_, name)          => s"\\k<$name>"
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
      case _: Disjunction | _: Sequence | _: Repeat | _: WordBoundary | _: LineBegin | _: LineEnd | _: LookAhead |
          _: LookBehind =>
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

  /** Shows a [[Quantifier]] as a pattern. */
  private[regexp] def showQuantifier(quantifier: Quantifier): String = {
    val s = quantifier match {
      case Quantifier.Star(_)              => "*"
      case Quantifier.Plus(_)              => "+"
      case Quantifier.Question(_)          => "?"
      case Quantifier.Exact(n, _)          => s"{$n}"
      case Quantifier.Unbounded(min, _)    => s"{$min,}"
      case Quantifier.Bounded(min, max, _) => s"{$min,$max}"
    }
    if (quantifier.isLazy) s"$s?" else s
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
