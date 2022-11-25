package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.NodeData

/** IRNodeData is a data of internal representation node. */
sealed abstract class IRNodeData extends Product with Serializable {
  def equalsWithoutLoc(that: IRNodeData): Boolean = (this, that) match {
    case (IRNodeData.Unsupported(l), IRNodeData.Unsupported(r)) => l.equalsWithoutLoc(r)
    case (IRNodeData.Capture(li, l), IRNodeData.Capture(ri, r)) => li == ri && l.equalsWithoutLoc(r)
    case (l, r)                                                 => l == r
  }
}

object IRNodeData {

  /** Empty is an empty string pattern. */
  case object Empty extends IRNodeData

  /** Assert is a simple zero-width assertion. */
  final case class Assert(kind: IRAssertKind) extends IRNodeData

  /** Capture is a capture group. */
  final case class Capture(index: Int, node: IRNode) extends IRNodeData

  object Capture {
    def apply(index: Int, data: IRNodeData): IRNodeData =
      Capture(index, IRNode(data))
  }

  /** Unsupported is a wrapper of AST node. */
  final case class Unsupported(data: NodeData) extends IRNodeData
}
