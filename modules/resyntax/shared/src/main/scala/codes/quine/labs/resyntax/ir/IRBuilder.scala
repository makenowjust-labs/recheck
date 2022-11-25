package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.CommandKind
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

    def reset(flagSet: FlagSet): BuildingContext =
      copy(multiline = flagSet.multiline)
  }

  object BuildingContext {
    def from(flagSet: FlagSet, featureSet: IRFeatureSet): BuildingContext =
      BuildingContext(
        multiline = flagSet.multiline
      )
  }

  /** Returns an IR node built from the given AST. */
  def build(node: Node, flagSet: FlagSet, dialect: Dialect): IRNode = {
    val featureSet = IRFeatureSet.from(flagSet, dialect)
    val builder = new IRBuilder(node, flagSet, dialect, featureSet)
    builder.build()
  }
}

private[ir] class IRBuilder(
    private[this] val node: Node,
    private[this] val flagSet: FlagSet,
    private[this] val dialect: Dialect,
    private[this] val featureSet: IRFeatureSet
) {
  var context: BuildingContext = BuildingContext.from(flagSet, featureSet)

  var nextIndex: Int = 1

  def build(): IRNode = build(node)

  def build(node: Node): IRNode = {
    val result = node.data match {
      case NodeData.Disjunction(_)     => ???
      case NodeData.Sequence(children) => buildSequence(node, children)
      case NodeData.Repeat(_, _)       => ???
      case NodeData.Group(kind, child) => buildGroup(node, kind, child)
      case NodeData.Command(kind)      => buildCommand(node, kind)
      case NodeData.Caret              => Left(buildCaret())
      case NodeData.Dollar             => Left(buildDollar())
      case NodeData.Dot                => ???
      case NodeData.Backslash(_)       => ???
      case NodeData.Class(_, _)        => ???
      case NodeData.Literal(_)         => ???
    }
    result match {
      case Left(data) => IRNode(data, node.loc)
      case Right(ir)  => ir
    }
  }

  def buildSequence(node: Node, children: Seq[Node]): Either[IRNodeData, IRNode] =
    children match {
      case Seq()      => Left(IRNodeData.Empty)
      case Seq(child) => Right(build(child))
      case children   => Left(IRNodeData.Sequence(children.map(build)))
    }

  def buildCommand(node: Node, kind: CommandKind): Either[IRNodeData, IRNode] = kind match {
    case CommandKind.InlineFlag(_)          => ???
    case CommandKind.ResetFlag(_)           => ???
    case CommandKind.PBackReference(_)      => ???
    case CommandKind.RCall                  => Left(buildRCallCommand(node))
    case CommandKind.IndexedCall(_)         => Left(buildIndexedCallCommand(node))
    case CommandKind.RelativeCall(_)        => Left(buildRelativeCallCommand(node))
    case CommandKind.NamedCall(_)           => Left(buildNamedCallCommand(node))
    case CommandKind.PNamedCall(_)          => Left(buildPNamedCallCommand(node))
    case CommandKind.Comment(_)             => Left(buildCommentCommand())
    case CommandKind.InlineCode(_)          => Left(buildInlineCodeCommand(node))
    case CommandKind.EmbedCode(_)           => Left(buildEmbedCodeCommand(node))
    case CommandKind.Callout                => Left(buildCalloutCommand(node))
    case CommandKind.CalloutInt(_)          => Left(buildCalloutIntCommand(node))
    case CommandKind.CalloutString(_, _, _) => Left(buildCalloutStringCommand(node))
    case CommandKind.BranchReset(_)         => Left(buildBranchResetCommand(node))
    case CommandKind.Conditional(_, _, _)   => Left(buildConditionalCommand(node))
    case CommandKind.BacktrackControl(_, _) => Left(buildBacktrackControlCommand(node))
  }

  def buildRCallCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildIndexedCallCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildRelativeCallCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildNamedCallCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildPNamedCallCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildCommentCommand(): IRNodeData = IRNodeData.Empty

  def buildInlineCodeCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildEmbedCodeCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildCalloutCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildCalloutIntCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildCalloutStringCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildBranchResetCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildConditionalCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildBacktrackControlCommand(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildGroup(node: Node, kind: GroupKind, child: Node): Either[IRNodeData, IRNode] = kind match {
    case GroupKind.IndexedCapture                 => Left(buildIndexedCapture(child))
    case GroupKind.NonCapture                     => Right(buildNonCapture(child))
    case GroupKind.NamedCapture(_, name)          => Left(buildNamedCapture(name, child))
    case GroupKind.Balance(_, _, _)               => Left(buildBalanceGroup(node))
    case GroupKind.PNamedCapture(name)            => Left(buildPNamedCapture(name, child))
    case _: GroupKind.LookAround                  => ???
    case GroupKind.Atomic(_)                      => Left(buildAtomicGroup(node))
    case GroupKind.NonAtomicPositiveLookAhead(_)  => Left(buildNonAtomicPositiveLookAheadGroup(node))
    case GroupKind.NonAtomicPositiveLookBehind(_) => Left(buildNonAtomicPositiveLookBehindGroup(node))
    case GroupKind.ScriptRun(_)                   => Left(buildScriptRunGroup(node))
    case GroupKind.AtomicScriptRun(_)             => Left(buildAtomicScriptRunGroup(node))
    case GroupKind.InlineFlag(diff)               => Right(buildInlineFlagGroup(diff, child))
    case GroupKind.ResetFlag(flagSet)             => Right(buildResetFlagGroup(flagSet, child))
    case GroupKind.Absence                        => Left(buildAbsenceGroup(node))
  }

  def buildIndexedCapture(child: Node): IRNodeData = {
    val index = nextIndex
    nextIndex += 1
    IRNodeData.Capture(index, None, build(child))
  }

  def buildNonCapture(child: Node): IRNode =
    build(child)

  def buildNamedCapture(name: String, child: Node): IRNodeData = {
    val index = nextIndex
    nextIndex += 1
    IRNodeData.Capture(index, Some(name), build(child))
  }

  def buildPNamedCapture(name: String, child: Node): IRNodeData = {
    val index = nextIndex
    nextIndex += 1
    IRNodeData.Capture(index, Some(name), build(child))
  }

  def buildBalanceGroup(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildAtomicGroup(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildNonAtomicPositiveLookAheadGroup(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildNonAtomicPositiveLookBehindGroup(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildScriptRunGroup(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildAtomicScriptRunGroup(node: Node): IRNodeData =
    IRNodeData.Unsupported(node.data)

  def buildInlineFlagGroup(diff: FlagSetDiff, child: Node): IRNode =
    withContext(_.update(diff))(build(child))

  def buildResetFlagGroup(flagSet: FlagSet, child: Node): IRNode =
    withContext(_.reset(flagSet))(build(child))

  def buildAbsenceGroup(node: Node): IRNodeData =
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
