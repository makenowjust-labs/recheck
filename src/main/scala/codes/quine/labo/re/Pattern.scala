package codes.quine.labo.re

import Pattern.{Node, FlagSet}

final case class Pattern(node: Node,flagSet: FlagSet)

object Pattern {
  final case class FlagSet(
    ignoreCase: Boolean,
    multiline: Boolean,
    dotAll: Boolean,
    unicode: Boolean,
  )

  sealed trait Node
  sealed trait ClassItem

  final case class Disjunction(children: Seq[Node]) extends Node
  final case class Sequence(children: Seq[Node]) extends Node
  final case class Star(nonGreedy: Boolean, child: Node) extends Node
  final case class WordBoundary(invert: Boolean) extends Node
  case object LineBegin extends Node
  case object LineEnd extends Node
  final case class Character(value: Int) extends Node with ClassItem
  final case class SimpleEscapeClass(invert: Boolean, kind: EscapeClassKind) extends Node with ClassItem
  final case class UnicodeProperty(invert: Boolean, property: String) extends Node with ClassItem
  final case class UnicodePropertyValue(invert: Boolean, property: String, value: String) extends Node with ClassItem
  final case class CharacterClass(invert: Boolean, children: Seq[ClassItem]) extends Node
  final case class ClassRange(begin: Int, end: Int) extends ClassItem
  case object Dot extends Node

  sealed trait EscapeClassKind
  object EscapeClassKind {
    case object Digit extends EscapeClassKind
    case object Word extends EscapeClassKind
    case object Space extends EscapeClassKind
  }
}
