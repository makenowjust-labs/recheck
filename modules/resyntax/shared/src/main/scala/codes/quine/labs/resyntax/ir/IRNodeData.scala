package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.NodeData

/** IRNodeData is a data of internal representation node. */
sealed abstract class IRNodeData extends Product with Serializable {
  def equalsWithoutLoc(that: IRNodeData): Boolean = (this, that) match {
    case (IRNodeData.Disjunction(ls), IRNodeData.Disjunction(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (IRNodeData.Sequence(ls), IRNodeData.Sequence(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (IRNodeData.Repeat(l, lq), IRNodeData.Repeat(r, rq)) =>
      lq == rq && l.equalsWithoutLoc(r)
    case (IRNodeData.Capture(li, ln, l), IRNodeData.Capture(ri, rn, r)) =>
      li == ri && ln == rn && l.equalsWithoutLoc(r)
    case (IRNodeData.Unsupported(l), IRNodeData.Unsupported(r)) => l.equalsWithoutLoc(r)
    case (l, r)                                                 => l == r
  }
}

object IRNodeData {

  /** Empty is an empty string pattern. */
  case object Empty extends IRNodeData

  /** Sequence is a sequence of nodes. */
  final case class Sequence(nodes: Seq[IRNode]) extends IRNodeData

  object Sequence {
    def apply(nodes: IRNodeData*): IRNodeData =
      Sequence(nodes.map(IRNode(_)))
  }

  /** Disjunction is a disjunction of nodes. */
  final case class Disjunction(nodes: Seq[IRNode]) extends IRNodeData

  object Disjunction {
    def apply(nodes: IRNodeData*): IRNodeData =
      Disjunction(nodes.map(IRNode(_)))
  }

  /** Repeat is a repetition node. */
  final case class Repeat(node: IRNode, quantifier: IRQuantifier) extends IRNodeData

  object Repeat {
    def apply(node: IRNodeData, quantifier: IRQuantifier): IRNodeData =
      Repeat(IRNode(node), quantifier)
  }

  /** Assert is a simple zero-width assertion. */
  final case class Assert(kind: IRAssertKind) extends IRNodeData

  /** Capture is a capture group. */
  final case class Capture(index: Int, name: Option[String], node: IRNode) extends IRNodeData

  object Capture {
    def apply(index: Int, name: Option[String], data: IRNodeData): IRNodeData =
      Capture(index, name, IRNode(data))
  }

  /** Unsupported is a wrapper of unsupported AST node. */
  final case class Unsupported(data: NodeData) extends IRNodeData
}
