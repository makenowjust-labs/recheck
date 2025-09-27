package codes.quine.labs.resyntax.ast

/** Node is a AST (abstract syntax tree) node for regular expression pattern. */
sealed abstract class NodeData extends Product with Serializable {
  def equalsWithoutLoc(that: NodeData): Boolean = (this, that) match {
    case (NodeData.Disjunction(ls), NodeData.Disjunction(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (NodeData.Sequence(ls), NodeData.Sequence(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (NodeData.Repeat(l, ql), NodeData.Repeat(r, qr)) =>
      l.equalsWithoutLoc(r) && ql == qr
    case (NodeData.Group(kl, l), NodeData.Group(kr, r)) =>
      kl == kr && l.equalsWithoutLoc(r)
    case (NodeData.Command(kl), NodeData.Command(kr)) =>
      kl.equalsWithoutLoc(kr)
    case (NodeData.Class(li, l), NodeData.Class(ri, r)) =>
      li == ri && l.equalsWithoutLoc(r)
    case (l, r) => l == r
  }
}

object NodeData {

  /** Disjunction is a disjunction pattern node.
    *
    * {{{
    * Disjunction ::= Sequence ("|" Sequence)*
    * }}}
    */
  final case class Disjunction(nodes: Seq[Node]) extends NodeData

  object Disjunction {
    def apply(dataSeq: NodeData*)(implicit dummy: DummyImplicit): Disjunction = Disjunction(dataSeq.map(Node(_)))
  }

  /** Sequence is a sequence pattern node.
    *
    * {{{
    * Sequence ::= Repeat*
    * }}}
    */
  final case class Sequence(nodes: Seq[Node]) extends NodeData

  object Sequence {
    def apply(dataSeq: NodeData*)(implicit dummy: DummyImplicit): Sequence = Sequence(dataSeq.map(Node(_)))
  }

  /** Repeat is a repetition pattern node.
    *
    * {{{
    * Repeat ::= Group Quantifier
    * }}}
    */
  final case class Repeat(node: Node, quantifier: Quantifier) extends NodeData

  object Repeat {
    def apply(data: NodeData, quantifier: Quantifier): Repeat = Repeat(Node(data), quantifier)
  }

  /** Group is a group pattern node.
    *
    * {{{
    * Group ::=
    *   "(" GroupKind Disjunction ")"
    *   Command
    *   Caret
    *   Dollar
    *   Dot
    *   Backslash
    *   Class
    *   Literal
    * }}}
    */
  final case class Group(kind: GroupKind, node: Node) extends NodeData

  object Group {
    def apply(kind: GroupKind, data: NodeData): Group = Group(kind, Node(data))
  }

  /** Command is a command pattern node.
    *
    * Command looks like a group, however it has no child node or child node has special form like a condition node.
    *
    * {{{
    * Command ::= "(" CommandKind ")"
    * }}}
    */
  final case class Command(kind: CommandKind) extends NodeData

  /** Caret is a caret pattern node.
    *
    * {{{
    * Caret ::= "^"
    * }}}
    */
  case object Caret extends NodeData

  /** Dollar is a dollar pattern node.
    *
    * {{{
    * Dollar ::= "$"
    * }}}
    */
  case object Dollar extends NodeData

  /** Dot is a dot pattern node.
    *
    * {{{
    * Dot ::= "."
    * }}}
    */
  case object Dot extends NodeData

  /** Backslash is a backslash pattern node. */
  final case class Backslash(kind: BackslashKind) extends NodeData

  /** Class is a class pattern node.
    *
    * {{{
    * Class = "[" "^"? ClassItem "]"
    * }}}
    */
  final case class Class(invert: Boolean, item: ClassItem) extends NodeData

  object Class {
    def apply(invert: Boolean, item: ClassItemData): Class = Class(invert, ClassItem(item))
  }

  /** Literal is a character literal node. */
  final case class Literal(value: Int) extends NodeData
}
