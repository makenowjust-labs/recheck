package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.{Dialect, FlagSet, Node, NodeData}
import codes.quine.labs.resyntax.ir.IRBuilder.BuildingContext

/** IRBuilder is a builder from AST node to IR. */
object IRBuilder {

  final case class BuildingContext(
      multiline: Boolean
  )

  object BuildingContext {
    def from(flagSet: FlagSet, featureSet: IRFeatureSet): BuildingContext =
      BuildingContext(
        multiline = flagSet.multiline
      )
  }

  /** Returns an IR node built from the given AST. */
  def build(dialect: Dialect, node: Node, flagSet: FlagSet): IRNode = {
    val featureSet = IRFeatureSet.from(dialect, flagSet)
    val builder = new IRBuilder(dialect, node, flagSet, featureSet)
    builder.build()
  }
}

private[ir] class IRBuilder(
    private[this] val dialect: Dialect,
    private[this] val node: Node,
    private[this] val flagSet: FlagSet,
    private[this] val featureSet: IRFeatureSet
) {
  var context: BuildingContext = BuildingContext.from(flagSet, featureSet)

  def build(): IRNode = build(node)

  def build(node: Node): IRNode = {
    val data = node.data match {
      case NodeData.Disjunction(_) => ???
      case NodeData.Sequence(_)    => ???
      case NodeData.Repeat(_, _)   => ???
      case NodeData.Group(_, _)    => ???
      case NodeData.Command(_)     => ???
      case NodeData.Caret          => buildCaret()
      case NodeData.Dollar         => buildDollar()
      case NodeData.Dot            => ???
      case NodeData.Backslash(_)   => ???
      case NodeData.Class(_, _)    => ???
      case NodeData.Literal(_)     => ???
    }
    IRNode(data, node.loc)
  }

  def buildCaret(): IRNodeData =
    if (featureSet.caretIsLineBegin || context.multiline) IRNodeData.Assert(IRAssertKind.LineBegin)
    else IRNodeData.Assert(IRAssertKind.TextBegin)

  def buildDollar(): IRNodeData =
    if (featureSet.dollarIsLineEnd || context.multiline) IRNodeData.Assert(IRAssertKind.LineEnd)
    else if (featureSet.dollarIsChompTextEnd) IRNodeData.Assert(IRAssertKind.ChompTextEnd)
    else IRNodeData.Assert(IRAssertKind.TextEnd)
}
