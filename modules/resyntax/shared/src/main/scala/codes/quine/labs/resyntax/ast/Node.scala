package codes.quine.labs.resyntax.ast

final case class Node(data: NodeData, loc: SourceLocation) {
  def equalsWithoutLoc(that: Node): Boolean =
    data.equalsWithoutLoc(that.data)
}

object Node {
  def apply(data: NodeData): Node = Node(data, SourceLocation.Invalid)
}
