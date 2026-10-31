package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.CaseCommandKind
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.Reference

/** IRBuilder is a builder from AST node to IR. */
object IRBuilder:

  /** Returns an IR pattern built from the given AST.
    *
    * It throws [[IRBuildingException]] for an error that needs the whole pattern (e.g. a reference to a non-existent
    * group, or a look-behind that the dialect does not allow).
    */
  def build(node: Node, flagSet: FlagSet, dialect: Dialect): IRPattern =
    build(node, flagSet, dialect, resolveCharacter)

  /** Resolves a character-level node with the flags at the node, which the character resolution step implements. */
  def resolveCharacter(node: Node, flags: IRCharFlags): IRNodeData =
    throw new NotImplementedError(s"Character resolution is not implemented: ${node.data} with $flags")

  /** Returns an IR pattern built from the given AST, with the given resolution of character-level nodes. */
  private[ir] def build(
      node: Node,
      flagSet: FlagSet,
      dialect: Dialect,
      resolveCharacter: (Node, IRCharFlags) => IRNodeData
  ): IRPattern =
    val featureSet = IRFeatureSet.from(flagSet, dialect, startOptions(node))
    val context = BuildingContext.from(flagSet, dialect, featureSet)
    val table = CaptureAnalyzer.analyze(node, context)
    val resolver = new ReferenceResolver(table, dialect, featureSet)
    val checker = new LookBehindChecker(table, resolver, featureSet)
    val backReferences = new IRValidator(table, resolver, checker, featureSet).validate(node)
    val builder = new IRBuilder(context, table, backReferences, resolveCharacter)
    IRPattern(
      builder.build(node),
      table.count,
      table.names,
      IRBackReferenceSemantics(featureSet.unsetBackReferenceMatchesEmpty, featureSet.backReferenceChoice),
      featureSet.anchored
    )

  /** Returns the names of PCRE's start options (e.g. `(*CRLF)`), which can appear only at the start. */
  private def startOptions(node: Node): Seq[String] =
    val nodes = node.data match
      case NodeData.Sequence(nodes) => nodes
      case _                        => Seq(node)
    nodes.iterator
      .map(_.data)
      .takeWhile:
        case NodeData.Command(_: CommandKind.StartOption) => true
        case _                                            => false
      .collect { case NodeData.Command(CommandKind.StartOption(name, _)) => name }
      .toSeq

  /** The start options that change whether a match is accepted, which the checkers do not handle. */
  private val UnsupportedStartOptions: Set[String] = Set("NOTEMPTY", "NOTEMPTY_ATSTART")

private[ir] class IRBuilder(
    initial: BuildingContext,
    table: CaptureTable,
    backReferences: Map[NodeKey, Seq[Int]],
    resolveCharacter: (Node, IRCharFlags) => IRNodeData
):
  private val dialect = initial.dialect

  private val featureSet = initial.featureSet

  var context: BuildingContext = initial

  /** Perl's case commands (`\L`, `\U`, `\F` and `\Q`), innermost first.
    *
    * Perl's lexer processes them on the source text, so they do not follow the group structure.
    */
  private var caseCommands: List[CaseCommandKind] = Nil

  /** Perl's `\l` or `\u` waiting for the next character. */
  private var pendingCaseCommand: Option[CaseCommandKind] = None

  def build(node: Node): IRNode =
    val result = node.data match
      case NodeData.Disjunction(children)                            => buildDisjunction(children)
      case NodeData.Sequence(children)                               => buildSequence(children)
      case NodeData.Repeat(child, quantifier)                        => Left(buildRepeat(node, child, quantifier))
      case NodeData.Group(kind, child)                               => buildGroup(node, kind, child)
      case NodeData.Command(kind)                                    => buildCommand(node, kind)
      case NodeData.Caret                                            => Left(buildCaret())
      case NodeData.Dollar                                           => Left(buildDollar())
      case NodeData.Backslash(kind)                                  => Left(buildBackslash(node, kind))
      case NodeData.Dot | NodeData.Class(_, _) | NodeData.Literal(_) => Left(buildCharacter(node))
    result match
      case Left(data) => IRNode(data, node.loc)
      case Right(ir)  => ir

  /** The flags that a character-level node at the current position needs. */
  def charFlags: IRCharFlags =
    context.charFlags(pendingCaseCommand.toList ++ caseCommands.filter(_ != CaseCommandKind.QuoteCommand))

  /** Builds a character-level node: `.`, a class, a literal, and a backslash producing characters (e.g. `\d`). */
  def buildCharacter(node: Node): IRNodeData =
    val flags = charFlags
    pendingCaseCommand = None
    // Java's CANON_EQ matches canonically equivalent sequences, which the checkers do not handle.
    if flags.canonicalEquivalence then IRNodeData.Unsupported(node.data)
    else resolveCharacter(node, flags)

  def buildDisjunction(children: Seq[Node]): Either[IRNodeData, IRNode] =
    Left(IRNodeData.Disjunction(children.map(build)))

  def buildSequence(children: Seq[Node]): Either[IRNodeData, IRNode] =
    children match
      case Seq()    => Left(IRNodeData.Empty)
      case children => Left(IRNodeData.Sequence(children.map(build)))

  def buildRepeat(node: Node, child: Node, quantifier: Quantifier): IRNodeData =
    val ir = build(child)
    val strategy = quantifier.strategy match
      case BacktrackStrategy.Greedy if context.ungreedy => BacktrackStrategy.Lazy
      case BacktrackStrategy.Lazy if context.ungreedy   => BacktrackStrategy.Greedy
      case BacktrackStrategy.Possessive                 => BacktrackStrategy.Greedy
      case strategy                                     => strategy
    val q = quantifier match
      case Quantifier.Star(_)                            => IRQuantifier.Unbounded(0, strategy)
      case Quantifier.Plus(_)                            => IRQuantifier.Unbounded(1, strategy)
      case Quantifier.Question(_)                        => IRQuantifier.Bounded(0, 1, strategy)
      case Quantifier.Exact(n, _)                        => IRQuantifier.Exact(n)
      case Quantifier.Bounded(min, max, _) if min == max => IRQuantifier.Exact(min)
      case Quantifier.Bounded(min, max, _)               => IRQuantifier.Bounded(min, max, strategy)
      case Quantifier.MaxBounded(0, _)                   => IRQuantifier.Exact(0)
      case Quantifier.MaxBounded(max, _)                 => IRQuantifier.Bounded(0, max, strategy)
      case Quantifier.Unbounded(min, _)                  => IRQuantifier.Unbounded(min, strategy)
    val repeat = IRNodeData.Repeat(ir, q)
    // A possessive quantifier is an atomic group of the greedy one, even for an exact count (e.g. `(?:a|ab){1}+`).
    if quantifier.strategy == BacktrackStrategy.Possessive then IRNodeData.Atomic(IRNode(repeat, node.loc))
    else repeat

  def buildCommand(node: Node, kind: CommandKind): Either[IRNodeData, IRNode] = kind match
    case CommandKind.InlineFlag(diff) =>
      context = context.update(diff)
      Left(IRNodeData.Empty)
    case CommandKind.ResetFlag(flagSet) =>
      context = context.reset(flagSet)
      Left(IRNodeData.Empty)
    case CommandKind.PBackReference(_)  => Left(buildBackReference(node))
    case CommandKind.BranchReset(nodes) =>
      scoped:
        nodes match
          case Seq(node) => Right(build(node))
          case nodes     => Left(IRNodeData.Disjunction(nodes.map(build)))
    case CommandKind.Comment(_)              => Left(IRNodeData.Empty)
    case CommandKind.StartOption(name, _)    => Left(buildStartOptionCommand(node, name))
    case CommandKind.RCall                   => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.IndexedCall(_)          => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.RelativeCall(_)         => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.NamedCall(_)            => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.PNamedCall(_)           => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.InlineCode(_)           => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.EmbedCode(_)            => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.Callout                 => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.CalloutInt(_)           => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.CalloutString(_, _, _)  => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.Conditional(_, _, _)    => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.BacktrackControl(_, _)  => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.AbsenceExpression(_, _) => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.AbsenceStopper(_)       => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.AbsenceRangeClear       => Left(IRNodeData.Unsupported(node.data))

  def buildStartOptionCommand(node: Node, name: String): IRNodeData =
    if IRBuilder.UnsupportedStartOptions.contains(name) then IRNodeData.Unsupported(node.data)
    else IRNodeData.Empty

  def buildGroup(node: Node, kind: GroupKind, child: Node): Either[IRNodeData, IRNode] = kind match
    case GroupKind.IndexedCapture =>
      table.indexOf(node) match
        case Some(index) => Left(IRNodeData.Capture(index, None, scoped(build(child))))
        case None        => Right(scoped(build(child)))
    case GroupKind.NonCapture            => Right(scoped(build(child)))
    case GroupKind.NamedCapture(_, name) => Left(buildNamedCapture(node, name, child))
    case GroupKind.PNamedCapture(name)   => Left(buildNamedCapture(node, name, child))
    case GroupKind.PositiveLookAhead(_)  => Left(IRNodeData.LookAhead(false, scoped(build(child))))
    case GroupKind.NegativeLookAhead(_)  => Left(IRNodeData.LookAhead(true, scoped(build(child))))
    case GroupKind.PositiveLookBehind(_) => Left(IRNodeData.LookBehind(false, scoped(build(child))))
    case GroupKind.NegativeLookBehind(_) => Left(IRNodeData.LookBehind(true, scoped(build(child))))
    case GroupKind.Atomic(_)             => Left(IRNodeData.Atomic(scoped(build(child))))
    case GroupKind.InlineFlag(diff)      =>
      Right(scoped:
        context = context.update(diff)
        build(child))
    case GroupKind.ResetFlag(flagSet) =>
      Right(scoped:
        context = context.reset(flagSet)
        build(child))
    case GroupKind.Balance(_, _, _)               => Left(IRNodeData.Unsupported(node.data))
    case GroupKind.NonAtomicPositiveLookAhead(_)  => Left(IRNodeData.Unsupported(node.data))
    case GroupKind.NonAtomicPositiveLookBehind(_) => Left(IRNodeData.Unsupported(node.data))
    case GroupKind.ScriptRun(_)                   => Left(IRNodeData.Unsupported(node.data))
    case GroupKind.AtomicScriptRun(_)             => Left(IRNodeData.Unsupported(node.data))
    case GroupKind.ScanSubstring(_, _)            => Left(IRNodeData.Unsupported(node.data))
    case GroupKind.Absence                        => Left(IRNodeData.Unsupported(node.data))

  def buildNamedCapture(node: Node, name: String, child: Node): IRNodeData =
    val index = table.indexOf(node).getOrElse(throw new IllegalStateException(s"No capture index: $node"))
    val captureName = if CaptureAnalyzer.isNumericName(dialect, name) then None else Some(name)
    IRNodeData.Capture(index, captureName, scoped(build(child)))

  def buildBackslash(node: Node, kind: BackslashKind): IRNodeData = kind match
    case _: BackslashKind.BackslashValue | _: BackslashKind.EscapeClass      => buildCharacter(node)
    case BackslashKind.EscapeBackReference(_, _: Reference.LeveledReference) => IRNodeData.Unsupported(node.data)
    case BackslashKind.EscapeBackReference(_, _)                             => buildBackReference(node)
    case BackslashKind.EscapeCall(_, _)                                      => IRNodeData.Unsupported(node.data)
    case BackslashKind.Assert(kind)                                          => buildAssert(node, kind)
    case BackslashKind.CaseCommand(kind)                                     => buildCaseCommand(kind)

  def buildBackReference(node: Node): IRNodeData =
    val indices = backReferences.getOrElse(
      NodeKey(node),
      throw new IllegalStateException(s"No resolved back-reference: $node")
    )
    IRNodeData.BackReference(indices, charFlags)

  def buildAssert(node: Node, kind: AssertKind): IRNodeData = kind match
    case AssertKind.Boundary(None)    => IRNodeData.Assert(IRAssertKind.WordBoundary(charFlags))
    case AssertKind.NonBoundary(None) => IRNodeData.Assert(IRAssertKind.NotWordBoundary(charFlags))
    case AssertKind.Begin             => IRNodeData.Assert(IRAssertKind.TextBegin)
    case AssertKind.LowerEnd          => IRNodeData.Assert(IRAssertKind.TextEnd)
    case AssertKind.UpperEnd          =>
      if featureSet.upperEndIsTextEnd then IRNodeData.Assert(IRAssertKind.TextEnd)
      else IRNodeData.Assert(IRAssertKind.ChompTextEnd(context.lineTerminators))
    case AssertKind.Boundary(Some(_)) | AssertKind.NonBoundary(Some(_)) | AssertKind.Cut | AssertKind.Sticky =>
      IRNodeData.Unsupported(node.data)

  def buildCaseCommand(kind: CaseCommandKind): IRNodeData =
    kind match
      case CaseCommandKind.SingleLowerCaseCommand | CaseCommandKind.SingleUpperCaseCommand =>
        pendingCaseCommand = Some(kind)
      case CaseCommandKind.EndCaseCommand => caseCommands = caseCommands.drop(1)
      case kind                           => caseCommands = kind :: caseCommands
    IRNodeData.Empty

  def buildCaret(): IRNodeData =
    if featureSet.caretIsLineBegin || context.multiline then
      IRNodeData.Assert(
        IRAssertKind.LineBegin(context.lineTerminators, featureSet.lineBeginMatchesAfterFinalTerminator)
      )
    else IRNodeData.Assert(IRAssertKind.TextBegin)

  def buildDollar(): IRNodeData =
    if featureSet.dollarIsLineEnd || context.multiline then
      IRNodeData.Assert(IRAssertKind.LineEnd(context.lineTerminators))
    else if featureSet.dollarIsChompTextEnd then IRNodeData.Assert(IRAssertKind.ChompTextEnd(context.lineTerminators))
    else IRNodeData.Assert(IRAssertKind.TextEnd)

  /** Builds within a group, where the inline flags inside do not leak out. */
  def scoped[A](x: => A): A =
    val saved = context
    try x
    finally context = saved
