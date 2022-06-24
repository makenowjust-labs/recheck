package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.NodeData

/** IRNodeData is a data of internal representation node. */
sealed abstract class IRNodeData extends Product with Serializable {
  def equalsWithoutLoc(that: IRNodeData): Boolean = (this, that) match {
    case (IRNodeData.Unsupported(l), IRNodeData.Unsupported(r)) => l.equalsWithoutLoc(r)
    case (l, r)                                                 => l == r
  }
}

object IRNodeData {

  /** Assert is a simple zero-width assertion. */
  final case class Assert(kind: IRAssertKind) extends IRNodeData

  /** Unsupported is a wrapper of AST node. */
  final case class Unsupported(data: NodeData) extends IRNodeData
}
