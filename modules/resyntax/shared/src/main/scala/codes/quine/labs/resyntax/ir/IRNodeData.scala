package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.NodeData

/** IRNodeData is a data of internal representation node. */
sealed abstract class IRNodeData extends Product with Serializable:
  def equalsWithoutLoc(that: IRNodeData): Boolean = (this, that) match
    case (IRNodeData.Disjunction(ls), IRNodeData.Disjunction(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (IRNodeData.Sequence(ls), IRNodeData.Sequence(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (IRNodeData.Repeat(l, lq), IRNodeData.Repeat(r, rq)) =>
      lq == rq && l.equalsWithoutLoc(r)
    case (IRNodeData.Atomic(l), IRNodeData.Atomic(r)) =>
      l.equalsWithoutLoc(r)
    case (IRNodeData.LookAhead(ln, l), IRNodeData.LookAhead(rn, r)) =>
      ln == rn && l.equalsWithoutLoc(r)
    case (IRNodeData.LookBehind(ln, l), IRNodeData.LookBehind(rn, r)) =>
      ln == rn && l.equalsWithoutLoc(r)
    case (IRNodeData.Capture(li, ln, l), IRNodeData.Capture(ri, rn, r)) =>
      li == ri && ln == rn && l.equalsWithoutLoc(r)
    case (IRNodeData.Unsupported(l), IRNodeData.Unsupported(r)) => l.equalsWithoutLoc(r)
    case (l, r)                                                 => l == r

object IRNodeData:

  /** Empty is an empty string pattern. */
  case object Empty extends IRNodeData

  /** Sequence is a sequence of nodes. */
  final case class Sequence(nodes: Seq[IRNode]) extends IRNodeData

  object Sequence:
    def apply(nodes: IRNodeData*): IRNodeData =
      Sequence(nodes.map(IRNode(_)))

  /** Disjunction is a disjunction of nodes. */
  final case class Disjunction(nodes: Seq[IRNode]) extends IRNodeData

  object Disjunction:
    def apply(nodes: IRNodeData*): IRNodeData =
      Disjunction(nodes.map(IRNode(_)))

  /** Repeat is a repetition node.
    *
    * Its strategy is greedy or lazy; a possessive repetition is a repetition in [[Atomic]].
    */
  final case class Repeat(node: IRNode, quantifier: IRQuantifier) extends IRNodeData

  object Repeat:
    def apply(node: IRNodeData, quantifier: IRQuantifier): IRNodeData =
      Repeat(IRNode(node), quantifier)

  /** Atomic is an atomic group, which does not backtrack into its node once it matches. */
  final case class Atomic(node: IRNode) extends IRNodeData

  object Atomic:
    def apply(data: IRNodeData): IRNodeData = Atomic(IRNode(data))

  /** LookAhead is a look-ahead assertion. */
  final case class LookAhead(negative: Boolean, node: IRNode) extends IRNodeData

  object LookAhead:
    def apply(negative: Boolean, data: IRNodeData): IRNodeData = LookAhead(negative, IRNode(data))

  /** LookBehind is a look-behind assertion. */
  final case class LookBehind(negative: Boolean, node: IRNode) extends IRNodeData

  object LookBehind:
    def apply(negative: Boolean, data: IRNodeData): IRNodeData = LookBehind(negative, IRNode(data))

  /** Assert is a simple zero-width assertion. */
  final case class Assert(kind: IRAssertKind) extends IRNodeData

  /** Capture is a capture group. */
  final case class Capture(index: Int, name: Option[String], node: IRNode) extends IRNodeData

  object Capture:
    def apply(index: Int, name: Option[String], data: IRNodeData): IRNodeData =
      Capture(index, name, IRNode(data))

  /** BackReference is a back-reference to the captures of the indices.
    *
    * It has more than one index for a name shared by groups, and no index for a group that does not exist (Java). How a
    * reference chooses among the indices, and matches an unset capture, is [[IRBackReferenceSemantics]] of the pattern.
    *
    * @param flags
    *   the flags at the reference, which decide how case-insensitive comparison works
    */
  final case class BackReference(indices: Seq[Int], flags: IRCharFlags) extends IRNodeData

  /** Unsupported is a wrapper of unsupported AST node. */
  final case class Unsupported(data: NodeData) extends IRNodeData
