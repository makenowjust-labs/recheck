package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.SourceLocation

/** IRNode is a pair of internal representation node data of its corresponding location. */
final case class IRNode(data: IRNodeData, loc: SourceLocation) {
  def equalsWithoutLoc(that: IRNode): Boolean =
    data.equalsWithoutLoc(that.data)
}

object IRNode {
  def apply(data: IRNodeData): IRNode = IRNode(data, SourceLocation.Invalid)
}
