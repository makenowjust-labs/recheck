package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ir.IRBuilder.BuildingContext

/** IRBuilder is a builder from AST node to IR. */
object IRBuilder {

  final case class BuildingContext(
      multiline: Boolean
  ) {
    def update(diff: FlagSetDiff): BuildingContext =
      copy(multiline = (multiline || diff.added.multiline) && !diff.removed.exists(_.multiline))
  }

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
    val result = node.data match {
      case NodeData.Disjunction(_)    => ???
      case NodeData.Sequence(_)       => ???
      case NodeData.Repeat(_, _)      => ???
      case NodeData.Group(kind, node) => buildGroup(kind, node)
      case NodeData.Command(_)        => ???
      case NodeData.Caret             => Left(buildCaret())
      case NodeData.Dollar            => Left(buildDollar())
      case NodeData.Dot               => ???
      case NodeData.Backslash(_)      => ???
      case NodeData.Class(_, _)       => ???
      case NodeData.Literal(_)        => ???
    }
    result match {
      case Left(data) => IRNode(data, node.loc)
      case Right(ir)  => ir
    }
  }

  def buildGroup(kind: GroupKind, node: Node): Either[IRNodeData, IRNode] = kind match {
    case GroupKind.IndexedCapture                 => ???
    case GroupKind.NonCapture                     => ???
    case GroupKind.NamedCapture(_, _)             => ???
    case GroupKind.Balance(_, _, _)               => ???
    case GroupKind.PNamedCapture(_)               => ???
    case _: GroupKind.LookAround                  => ???
    case GroupKind.Atomic(_)                      => Left(buildAtomic(node))
    case GroupKind.NonAtomicPositiveLookAhead(_)  => Left(buildNonAtomicPositiveLookAhead(node))
    case GroupKind.NonAtomicPositiveLookBehind(_) => Left(buildNonAtomicPositiveLookBehind(node))
    case GroupKind.ScriptRun(_)                   => Left(buildScriptRun(node))
    case GroupKind.AtomicScriptRun(_)             => Left(buildAtomicScriptRun(node))
    case GroupKind.InlineFlag(diff)               => Right(buildInlineFlagGroup(diff))
    case GroupKind.ResetFlag(_)                   => ???
    case GroupKind.Absence                        => Left(buildAbsence(node))
  }

  def buildAtomic(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildNonAtomicPositiveLookAhead(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildNonAtomicPositiveLookBehind(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildScriptRun(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildAtomicScriptRun(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildInlineFlagGroup(diff: FlagSetDiff): IRNode =
    withContext(_.update(diff))(build(node))

  def buildAbsence(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildCaret(): IRNodeData =
    if (featureSet.caretIsLineBegin || context.multiline) IRNodeData.Assert(IRAssertKind.LineBegin)
    else IRNodeData.Assert(IRAssertKind.TextBegin)

  def buildDollar(): IRNodeData =
    if (featureSet.dollarIsLineEnd || context.multiline) IRNodeData.Assert(IRAssertKind.LineEnd)
    else if (featureSet.dollarIsChompTextEnd) IRNodeData.Assert(IRAssertKind.ChompTextEnd)
    else IRNodeData.Assert(IRAssertKind.TextEnd)

  def withContext[A](update: BuildingContext => BuildingContext)(x: => A): A = {
    val oldContext = context
    context = update(context)
    try x
    finally {
      context = oldContext
    }
  }
}
