package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.DialectOptions
import codes.quine.labs.resyntax.ast.Encoding
import codes.quine.labs.resyntax.ast.EscapeClassKind
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.QuoteLiteral
import codes.quine.labs.resyntax.ast.Reference
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.FullCaseFolding
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** IRBuilder is a builder from AST node to IR. */
object IRBuilder:

  /** Returns an IR pattern built from the given AST.
    *
    * It throws [[IRBuildingException]] for an error that needs the whole pattern (e.g. a reference to a non-existent
    * group, or a look-behind that the dialect does not allow).
    *
    * @param options
    *   the dialect's options, where IR uses the encoding
    */
  def build(
      node: Node,
      flagSet: FlagSet,
      dialect: Dialect,
      options: DialectOptions = DialectOptions.Default
  ): IRPattern =
    build(node, flagSet, dialect, options, None)

  /** Returns an IR pattern built from the given AST, where `resolveCharacter` replaces the character resolution. */
  private[ir] def build(
      node: Node,
      flagSet: FlagSet,
      dialect: Dialect,
      options: DialectOptions,
      resolveCharacter: Option[(Node, IRCharFlags) => IRNodeData]
  ): IRPattern =
    val starts = startOptions(node)
    val featureSet = IRFeatureSet.from(flagSet, dialect, starts)
    options.validate(dialect)
    val mode = unitMode(dialect, options.encoding, flagSet, featureSet.utf)
    if dialect == Dialect.Python && mode == UnitMode.CodePoint && flagSet.localeUpper then
      throw new IRBuildingException("Cannot use LOCALE flag with a str pattern", SourceLocation.Invalid)
    val context = BuildingContext.from(flagSet, dialect, featureSet)
    val table = CaptureAnalyzer.analyze(node, context)
    val resolver = new ReferenceResolver(table, dialect, featureSet)
    val perlUnicodeRules = dialect == Dialect.Perl && usesPerlUnicodeRules(node, options.perlUtf8)
    val checker =
      new LookBehindChecker(table, resolver, featureSet, literalWidth(dialect, mode, flagSet, perlUnicodeRules))
    val backReferences = new IRValidator(table, resolver, checker, featureSet).validate(node)
    val chars = new IRCharResolver(dialect, mode, perlUnicodeRules)
    val builder = new IRBuilder(context, table, backReferences, chars, resolveCharacter)
    IRPattern(
      builder.build(node),
      table.count,
      table.names,
      IRBackReferenceSemantics(featureSet.unsetBackReferenceMatchesEmpty, featureSet.backReferenceChoice),
      featureSet.anchored,
      mode
    )

  /** Returns the unit mode of a pattern from the dialect, the encoding and the flags.
    *
    * @param utf
    *   PCRE's `(*UTF)` start option
    */
  private def unitMode(dialect: Dialect, encoding: Option[Encoding], flagSet: FlagSet, utf: Boolean): UnitMode =
    dialect match
      case Dialect.JavaScript =>
        if flagSet.unicode || flagSet.unicodeSets then UnitMode.CodePoint else UnitMode.Utf16
      // Java matches code points, and reads a lone surrogate as a code point too.
      case Dialect.Java   => UnitMode.CodePoint
      case Dialect.DotNet => UnitMode.Utf16
      case Dialect.Python =>
        if flagSet.bytes || encoding.contains(Encoding.PythonBytes) then UnitMode.Byte else UnitMode.CodePoint
      // Ruby's `n` is the encoding option (NOENCODING), which `FlagSet` keeps as `explicitCapture`.
      case Dialect.Ruby =>
        if flagSet.explicitCapture || encoding.contains(Encoding.RubyAscii8Bit) then UnitMode.Byte
        else UnitMode.CodePoint
      case Dialect.Perl => if encoding.contains(Encoding.PerlBytes) then UnitMode.Byte else UnitMode.CodePoint
      case Dialect.PCRE => if flagSet.unicode || utf then UnitMode.CodePoint else UnitMode.Byte

  /** Returns the case equivalence of a back-reference with the flags in the dialect and the mode. */
  def caseEquivalence(dialect: Dialect, mode: UnitMode, flags: IRCharFlags): IRCaseEquivalence =
    IRCaseEquivalence.of(dialect, mode, flags, perlNative = false)

  /** Returns the width of a literal in a look-behind, where the dialects counting it differently from one care.
    *
    * PCRE's byte mode counts the UTF-8 bytes of a non-ASCII literal (a surrogate of a pair is half of four bytes), and
    * Perl counts the folded length under `i` (e.g. `(?<=ß{128})` is too long). The flag `i` is the pattern's one here.
    */
  private def literalWidth(
      dialect: Dialect,
      mode: UnitMode,
      flagSet: FlagSet,
      perlUnicodeRules: Boolean
  ): Int => LookBehindChecker.Width =
    import LookBehindChecker.Width
    dialect match
      case Dialect.PCRE if mode == UnitMode.Byte =>
        value =>
          if value < 0x80 then Width(1, 1)
          else if 0xd800 <= value && value <= 0xdfff then Width(2, 2)
          else
            val n = UString(value).encode(UnitMode.Byte).length.toLong
            Width(n, n)
      case Dialect.Perl if flagSet.ignoreCase && perlUnicodeRules =>
        value => Width(1, FullCaseFolding(value).length.max(1).toLong)
      case _ => _ => Width(1, 1)

  /** Checks whether a Perl pattern uses Unicode rules under `/d` (perlre "Which character set modifier is in effect?").
    *
    * A UTF-8 pattern does, and a code point above `U+00FF`, `\p{...}` and `\N{...}` make the pattern UTF-8. Without
    * `utf8`, a non-ASCII literal is taken as a sign of a UTF-8 pattern (with `use utf8`).
    */
  private def usesPerlUnicodeRules(node: Node, utf8: Option[Boolean]): Boolean =
    utf8.contains(true) || usesPerlUnicodeSyntax(node, literals = utf8.isEmpty)

  private def usesPerlUnicodeSyntax(node: Node, literals: Boolean): Boolean =
    def check(x: Any): Boolean = x match
      case NodeData.Literal(value)           => literals && value >= 0x80
      case ClassItemData.ClassLiteral(value) => literals && value >= 0x80
      case QuoteLiteral.QuoteValue(value)    => literals && value >= 0x80
      case BackslashKind.Escape(_, value)    => value > 0xff
      case _: EscapeClassKind.CharName       => true
      case _: EscapeClassKind.UnicodeProperty | _: EscapeClassKind.UnicodeBareProperty |
          _: EscapeClassKind.UnicodePropertyValue | _: EscapeClassKind.NonUnicodeProperty |
          _: EscapeClassKind.NonUnicodeBareProperty | _: EscapeClassKind.NonUnicodePropertyValue =>
        true
      case xs: Iterable[?] => xs.exists(check)
      case x: Option[?]    => x.exists(check)
      case _: String       => false
      case x: Product      => x.productIterator.exists(check)
      case _               => false
    check(node)

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
    chars: IRCharResolver,
    resolveCharacter: Option[(Node, IRCharFlags) => IRNodeData]
):
  private val dialect = initial.dialect

  private val featureSet = initial.featureSet

  var context: BuildingContext = initial

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
  def charFlags: IRCharFlags = context.charFlags

  /** Builds a character-level node: `.`, a class, a literal, and a backslash producing characters (e.g. `\d`). */
  def buildCharacter(node: Node): IRNodeData =
    val flags = charFlags
    // Java's CANON_EQ matches canonically equivalent sequences, which the checkers do not handle.
    if flags.canonicalEquivalence then IRNodeData.Unsupported(node.data)
    else resolveCharacter.fold(chars.resolve(node, flags))(_(node, flags))

  /** Builds consecutive literal characters together (e.g. for multi-character case folding). */
  def buildLiterals(nodes: Seq[Node]): Seq[IRNode] =
    val run = nodes.map(node => (node, charFlags))
    // Java's CANON_EQ matches canonically equivalent sequences, which the checkers do not handle.
    if run.exists(_._2.canonicalEquivalence) then
      run.map((node, _) => IRNode(IRNodeData.Unsupported(node.data), node.loc))
    else
      resolveCharacter match
        case Some(f) => run.map((node, flags) => IRNode(f(node, flags), node.loc))
        case None    => chars.literals(run)

  def buildDisjunction(children: Seq[Node]): Either[IRNodeData, IRNode] =
    Left(IRNodeData.Disjunction(children.map(build)))

  def buildSequence(children: Seq[Node]): Either[IRNodeData, IRNode] =
    children match
      case Seq()     => Left(IRNodeData.Empty)
      case children0 =>
        val children = chars.joinRubyByteRepeats(children0)
        val nodes = Seq.newBuilder[IRNode]
        var i = 0
        while i < children.length do
          if chars.isLiteral(children(i)) then
            var j = i
            while j < children.length && chars.isLiteral(children(j)) do j += 1
            nodes ++= buildLiterals(children.slice(i, j))
            i = j
          else
            nodes += build(children(i))
            i += 1
        // Literals resolved together may become one node (e.g. `ss` under Ruby's case folding).
        nodes.result() match
          case Seq(node) => Right(node)
          case nodes     => Left(IRNodeData.Sequence(nodes))

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
    case BackslashKind.CaseCommand(_)                                        => IRNodeData.Empty

  def buildBackReference(node: Node): IRNodeData =
    val indices = backReferences.getOrElse(
      NodeKey(node),
      throw new IllegalStateException(s"No resolved back-reference: $node")
    )
    IRNodeData.BackReference(indices, charFlags)

  def buildAssert(node: Node, kind: AssertKind): IRNodeData = kind match
    case AssertKind.Boundary(None)    => IRNodeData.Assert(IRAssertKind.WordBoundary(chars.wordSet(charFlags)))
    case AssertKind.NonBoundary(None) => IRNodeData.Assert(IRAssertKind.NotWordBoundary(chars.wordSet(charFlags)))
    case AssertKind.Begin             => IRNodeData.Assert(IRAssertKind.TextBegin)
    case AssertKind.LowerEnd          => IRNodeData.Assert(IRAssertKind.TextEnd)
    case AssertKind.UpperEnd          =>
      if featureSet.upperEndIsTextEnd then IRNodeData.Assert(IRAssertKind.TextEnd)
      else IRNodeData.Assert(IRAssertKind.ChompTextEnd(context.lineTerminators))
    case AssertKind.Boundary(Some(_)) | AssertKind.NonBoundary(Some(_)) | AssertKind.Cut | AssertKind.Sticky =>
      IRNodeData.Unsupported(node.data)

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
