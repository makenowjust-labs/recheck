package codes.quine.labs.resyntax.ast

sealed abstract class NodeData extends Product with Serializable:

  def equalsWithoutLoc(that: NodeData): Boolean = (this, that) match
    case (NodeData.Disjunction(ls), NodeData.Disjunction(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (NodeData.Sequence(ls), NodeData.Sequence(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (NodeData.Repeat(l, ql, wl), NodeData.Repeat(r, qr, wr)) =>
      l.equalsWithoutLoc(r) && ql == qr && wl == wr
    case (NodeData.Group(kl, l), NodeData.Group(kr, r)) =>
      kl == kr && l.equalsWithoutLoc(r)
    case (NodeData.Command(kl), NodeData.Command(kr)) =>
      kl.equalsWithoutLoc(kr)
    case (NodeData.Class(li, l), NodeData.Class(ri, r)) =>
      li == ri && l.equalsWithoutLoc(r)
    case (NodeData.UnicodeList(ls, wl), NodeData.UnicodeList(rs, wr)) =>
      ls.length == rs.length && ls.zip(rs).forall((l, r) => l.equalsWithoutLoc(r)) && wl == wr
    case (l, r) => l == r

object NodeData:

  /** Disjunction is a disjunction pattern node.
    *
    * {{{
    * Disjunction ::= Sequence ("|" Sequence)*
    * }}}
    */
  final case class Disjunction(nodes: Seq[Node]) extends NodeData

  object Disjunction:

    def apply(dataSeq: NodeData*)(using DummyImplicit): Disjunction = Disjunction(dataSeq.map(Node(_)))

  /** Sequence is a sequence pattern node.
    *
    * {{{
    * Sequence ::= (Repeat | Blank)*
    * }}}
    */
  final case class Sequence(nodes: Seq[Node]) extends NodeData

  object Sequence:

    def apply(dataSeq: NodeData*)(using DummyImplicit): Sequence = Sequence(dataSeq.map(Node(_)))

  /** Repeat is a repetition pattern node.
    *
    * {{{
    * Repeat ::= Group Quantifier
    * }}}
    *
    * `written` is the text after the node as written, with the blanks and comments before the quantifier (e.g. `{02}`,
    * ` *` or `(?#c)+`), or `None` for the plain text of the quantifier.
    */
  final case class Repeat(node: Node, quantifier: Quantifier, written: Option[String] = None) extends NodeData

  object Repeat:

    def apply(data: NodeData, quantifier: Quantifier): Repeat = Repeat(Node(data), quantifier)

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

  object Group:

    def apply(kind: GroupKind, data: NodeData): Group = Group(kind, Node(data))

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

  final case class Backslash(kind: BackslashKind) extends NodeData

  /** Class is a class pattern node.
    *
    * {{{
    * Class = "[" "^"? ClassItem "]"
    * }}}
    */
  final case class Class(invert: Boolean, item: ClassItem) extends NodeData

  object Class:

    def apply(invert: Boolean, item: ClassItemData): Class = Class(invert, ClassItem(item))

  final case class Literal(value: Int) extends NodeData

  /** Blank is the whitespace and comments that the extended mode skips, as they are written (e.g. ` # x\n`).
    *
    * A curly quantifier after it that Java ignores, since nothing is before it, is in its text (e.g. ` {2}`).
    */
  final case class Blank(text: String) extends NodeData

  /** UnicodeList is Ruby's `\u{...}` of several code points (e.g. `\u{41 42}`), which Ruby expands into an escape for
    * each before reading the pattern.
    *
    * `nodes` are the escapes, the last of which is a repeat if a quantifier follows (e.g. `\u{41 42}*` is
    * `\u{41}\u{42}*`). `written` is the text after `\u` as written (e.g. `{41  42}`), or `None` for the plain text.
    */
  final case class UnicodeList(nodes: Seq[Node], written: Option[String] = None) extends NodeData
