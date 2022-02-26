package codes.quine.labs.resyntax.ast

/** Node is a AST (abstract syntax tree) node for regular expression pattern. */
sealed abstract class Node extends Product with Serializable {
  def loc: Option[SourceLocation]
}

object Node {

  /** Disjunction is a disjunction pattern node.
    *
    * {{{
    * Disjunction ::= Sequence ("|" Sequence)*
    * }}}
    */
  final case class Disjunction(nodes: Seq[Node], loc: Option[SourceLocation]) extends Node

  /** Sequence is a sequence pattern node.
    *
    * {{{
    * Sequence ::= Repeat*
    * }}}
    */
  final case class Sequence(nodes: Seq[Node], loc: Option[SourceLocation]) extends Node

  /** Repeat is a repetition pattern node.
    *
    * {{{
    * Repeat ::= Group Quantifier
    * }}}
    */
  final case class Repeat(node: Node, quantifier: Quantifier, loc: Option[SourceLocation]) extends Node

  /** Group is a group pattern node.
    *
    * {{{
    * Group ::= "(" GroupKind Disjunction ")"
    * }}}
    */
  final case class Group(kind: GroupKind, node: Node, loc: Option[SourceLocation]) extends Node

  /** Command is a command pattern node.
    *
    * Command looks like a group, however it has no child node or child node has special form like a condition node.
    *
    * {{{
    * Command ::= "(" CommandKind ")"
    * }}}
    */
  final case class Command(kind: CommandKind, loc: Option[SourceLocation]) extends Node

  /** Literal is a character literal node. */
  final case class Literal(value: Int, loc: Option[SourceLocation]) extends Node
}
