package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackControlKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
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
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.FullCaseFolding
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
    // A Perl byte string has no `use utf8`, so its non-ASCII literals are bytes.
    val perlUtf8 = if mode == UnitMode.Byte then options.perlUtf8.orElse(Some(false)) else options.perlUtf8
    val perlUnicodeRules = dialect == Dialect.Perl && usesPerlUnicodeRules(node, perlUtf8)
    val supplementary = if dialect == Dialect.Java then lastSupplementary(node) else -1
    val checker =
      new LookBehindChecker(
        table,
        resolver,
        featureSet,
        literalWidth(flagSet, dialect, perlUnicodeRules),
        supplementary
      )
    val backReferences = new IRValidator(table, resolver, checker, featureSet).validate(node)
    val chars = new IRCharResolver(dialect, mode, perlUnicodeRules)
    val builder =
      new IRBuilder(
        context,
        table,
        backReferences,
        chars,
        checker,
        resolveCharacter,
        hasCall(node),
        hasBackReference(node)
      )
    IRPattern(
      builder.build(node),
      table.count,
      table.names,
      IRBackReferenceSemantics(featureSet.unsetBackReferenceMatchesEmpty, featureSet.backReferenceChoice),
      featureSet.anchored,
      mode,
      notEmpty(starts)
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
    * Perl counts the folded length under `i` (e.g. `(?<=ß{128})` is too long). The flag `i` is the pattern's one here.
    */
  private def literalWidth(
      flagSet: FlagSet,
      dialect: Dialect,
      perlUnicodeRules: Boolean
  ): Int => LookBehindChecker.Width =
    import LookBehindChecker.Width
    if dialect == Dialect.Perl && flagSet.ignoreCase && perlUnicodeRules then
      value => Width(1, FullCaseFolding(value).length.max(1).toLong)
    else _ => Width(1, 1)

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

  /** Returns the start of the last node with a supplementary character or a surrogate in the pattern text, or `-1`. */
  private def lastSupplementary(node: Node): Int =
    def isSupplementary(c: Int): Boolean = c >= 0x10000 || Character.isSurrogate(c.toChar) && c <= 0xffff
    def hasSupplementary(x: Any): Boolean = x match
      case ClassItemData.ClassLiteral(value) => isSupplementary(value)
      case xs: Iterable[?]                   => xs.exists(hasSupplementary)
      case x: Option[?]                      => x.exists(hasSupplementary)
      case _: String                         => false
      case x: Product                        => x.productIterator.exists(hasSupplementary)
      case _                                 => false
    val self = node.data match
      case NodeData.Literal(value)                     => isSupplementary(value)
      case NodeData.Class(_, item)                     => hasSupplementary(item)
      case NodeData.Command(CommandKind.Comment(text)) => text.codePoints.anyMatch(isSupplementary)
      case _                                           => false
    NodeKey.children(node).map(lastSupplementary).foldLeft(if self then node.loc.start else -1)(_.max(_))

  /** Returns the names of PCRE's start options (e.g. `(*CRLF)`), which can appear only at the start. */
  private def startOptions(node: Node): Seq[String] =
    // They are in the first alternative of the pattern (e.g. `(*NOTEMPTY)a|b`).
    def leading(node: Node): Seq[Node] = node.data match
      case NodeData.Sequence(nodes)         => nodes
      case NodeData.Disjunction(first +: _) => leading(first)
      case _                                => Seq(node)
    leading(node).iterator
      .map(_.data)
      .takeWhile:
        case NodeData.Command(_: CommandKind.StartOption) => true
        case _                                            => false
      .collect { case NodeData.Command(CommandKind.StartOption(name, _)) => name }
      .toSeq

  /** Returns where an empty match is rejected by the start options, where `(*NOTEMPTY)` includes the other. */
  private def notEmpty(startOptions: Seq[String]): IRNotEmpty =
    if startOptions.contains("NOTEMPTY") then IRNotEmpty.Anywhere
    else if startOptions.contains("NOTEMPTY_ATSTART") then IRNotEmpty.AtStart
    else IRNotEmpty.Never

  /** Checks whether the pattern has a back-reference. */
  private def hasBackReference(node: Node): Boolean =
    NodeKey.exists(node):
      _.data match
        case NodeData.Backslash(_: BackslashKind.EscapeBackReference) => true
        case NodeData.Command(_: CommandKind.PBackReference)          => true
        case _                                                        => false

  /** Checks whether the pattern calls a group or itself (e.g. `(?1)` and `\g<1>`). */
  private def hasCall(node: Node): Boolean =
    NodeKey.exists(node):
      _.data match
        case NodeData.Command(
              CommandKind.RCall | _: CommandKind.IndexedCall | _: CommandKind.RelativeCall | _: CommandKind.NamedCall |
              _: CommandKind.PNamedCall
            ) =>
          true
        case NodeData.Backslash(_: BackslashKind.EscapeCall) => true
        case _                                               => false

  /** The version of PCRE2 that the PCRE dialect follows, which `(?(VERSION>=10.0)...)` compares with. */
  val PCREVersion: (Int, Int) = (10, 49)

private[ir] class IRBuilder(
    initial: BuildingContext,
    table: CaptureTable,
    backReferences: Map[NodeKey, Seq[Int]],
    chars: IRCharResolver,
    checker: LookBehindChecker,
    resolveCharacter: Option[(Node, IRCharFlags) => IRNodeData],
    hasCall: Boolean,
    hasBackReference: Boolean
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
    val ir = child.data match
      // Java's `\R` under a quantifier matches each iteration in its first way, though `\R` alone backtracks.
      case NodeData.Backslash(BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline)) if dialect == Dialect.Java =>
        IRNode(IRNodeData.Atomic(build(child)), child.loc)
      case _ if isJavaGroupCurly(child, quantifier) => javaGroupCurly(build(child))
      case _                                        => build(child)
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

  /** Checks whether Java repeats the group with `GroupCurly`, which matches each iteration in its first way, and keeps
    * the captures in it over backtracking.
    *
    * It matters only for a group with `\R` (which has two ways), or with captures in a pattern with back-references.
    */
  def isJavaGroupCurly(child: Node, quantifier: Quantifier): Boolean =
    def isGroup = child.data match
      case NodeData.Group(GroupKind.IndexedCapture | GroupKind.NonCapture | _: GroupKind.NamedCapture, _) => true
      case NodeData.Group(_: GroupKind.InlineFlag, _)                                                     => true
      case _                                                                                              => false
    def matters = NodeKey.exists(child):
      case Node(NodeData.Backslash(BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline)), _) => true
      case node @ Node(NodeData.Group(_, _), _) => hasBackReference && node != child && table.indexOf(node).isDefined
      case _                                    => false
    val counted = quantifier match
      case Quantifier.Question(_) => false
      case _                      => quantifier.strategy != BacktrackStrategy.Possessive
    dialect == Dialect.Java && counted && isGroup && checker.javaDeterministic(child) && matters

  /** Makes the body of Java's `GroupCurly` atomic, which keeps the captures in it as Java's atomic group does. */
  def javaGroupCurly(ir: IRNode): IRNode = ir.data match
    case IRNodeData.Capture(index, name, body) =>
      IRNode(IRNodeData.Capture(index, name, IRNode(IRNodeData.Atomic(body), body.loc)), ir.loc)
    case _ => IRNode(IRNodeData.Atomic(ir), ir.loc)

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
    case CommandKind.Comment(_)                   => Left(IRNodeData.Empty)
    case CommandKind.StartOption(name, _)         => Left(buildStartOptionCommand(node, name))
    case CommandKind.RCall                        => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.IndexedCall(_)               => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.RelativeCall(_)              => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.NamedCall(_)                 => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.PNamedCall(_)                => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.InlineCode(_)                => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.EmbedCode(_)                 => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.Callout                      => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.CalloutInt(_)                => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.CalloutString(_, _, _)       => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.Conditional(test, yes, no)   => buildConditional(node, test, yes, no)
    case CommandKind.BacktrackControl(kind, name) => Left(buildBacktrackControl(kind, name))
    case CommandKind.AbsenceExpression(_, _)      => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.AbsenceStopper(_)            => Left(IRNodeData.Unsupported(node.data))
    case CommandKind.AbsenceRangeClear            => Left(IRNodeData.Unsupported(node.data))

  // The start options are of the whole pattern, which `IRBuilder.build` reads.
  def buildStartOptionCommand(node: Node, name: String): IRNodeData = IRNodeData.Empty

  def buildConditional(node: Node, test: ConditionalTest, yes: Node, no: Option[Node]): Either[IRNodeData, IRNode] =
    def branch(node: Option[Node]): IRNode = node.fold(IRNode(IRNodeData.Empty))(n => scoped(build(n)))
    def captured(indices: Seq[Int]): Either[Boolean, IRCondition] =
      // .NET's group `0` is the whole match, which is not set yet.
      val set = indices.filter(_ != 0)
      if set.isEmpty then Left(false)
      // Ruby tests only the first group of a name shared by groups.
      else if dialect == Dialect.Ruby then Right(IRCondition.Captured(set.take(1)))
      else Right(IRCondition.Captured(set))
    def resolved: Seq[Int] =
      backReferences.getOrElse(NodeKey(node), throw new IllegalStateException(s"No resolved test: $node"))
    // A recursion test is false out of a recursion, so it is constant without calls.
    // PCRE reads `(R)` and `(R1)` as capture tests when a group has the name.
    def recursion(name: Option[String]): Option[Either[Boolean, IRCondition]] =
      name.filter(dialect == Dialect.PCRE && table.names.contains(_)) match
        case Some(name) => Some(captured(table.names(name)))
        case None       => Option.when(!hasCall)(Left(false))
    val condition: Option[Either[Boolean, IRCondition]] = test match
      case ConditionalTest.Indexed(_) | ConditionalTest.Relative(_) | ConditionalTest.Named(_, _) =>
        Some(captured(resolved))
      case ConditionalTest.Referenced(_, _: Reference.LeveledReference) => None
      case ConditionalTest.Referenced(_, _)                             => Some(captured(resolved))
      case ConditionalTest.RRecursion                                   => recursion(Some("R"))
      case ConditionalTest.IndexedRecursion(n)                          => recursion(Some(s"R$n"))
      case ConditionalTest.NamedRecursion(_)                            => recursion(None)
      // `(DEFINE)` holds the groups for calls, and it is skipped otherwise.
      case ConditionalTest.Define                    => Option.when(!hasCall)(Left(false))
      case ConditionalTest.Version(ge, major, minor) =>
        import scala.math.Ordering.Implicits.*
        val version = IRBuilder.PCREVersion
        Some(Left(if ge then version >= (major, minor) else version == (major, minor)))
      case ConditionalTest.LookAround(kind, body)  => Some(Right(buildLookAroundCondition(kind, body)))
      case ConditionalTest.CalloutLookAround(_, _) => None
    condition match
      case Some(Left(true))       => Right(branch(Some(yes)))
      case Some(Left(false))      => Right(branch(no))
      case Some(Right(condition)) => Left(IRNodeData.Conditional(condition, branch(Some(yes)), branch(no)))
      case None                   => Left(IRNodeData.Unsupported(node.data))

  /** Builds a look-around condition, where .NET reads a bare group name as a capture test (e.g. `(?(x)a|b)`). */
  def buildLookAroundCondition(kind: Option[GroupKind.LookAround], body: Node): IRCondition =
    def dotNetName: Option[String] = body.data match
      case NodeData.Literal(c)      => Some(String(Character.toChars(c)))
      case NodeData.Sequence(nodes) =>
        val cs = nodes.collect { case Node(NodeData.Literal(c), _) => String(Character.toChars(c)) }
        Option.when(cs.size == nodes.size)(cs.mkString)
      case _ => None
    kind match
      case None =>
        dotNetName.flatMap(table.names.get) match
          case Some(indices) => IRCondition.Captured(indices)
          case None          => IRCondition.LookAhead(false, scoped(build(body)))
      case Some(_: GroupKind.PositiveLookAhead)  => IRCondition.LookAhead(false, scoped(build(body)))
      case Some(_: GroupKind.NegativeLookAhead)  => IRCondition.LookAhead(true, scoped(build(body)))
      case Some(_: GroupKind.PositiveLookBehind) =>
        IRCondition.LookBehind(false, scoped(build(body)), checker.matching(body))
      case Some(_: GroupKind.NegativeLookBehind) =>
        IRCondition.LookBehind(true, scoped(build(body)), checker.matching(body))

  def buildBacktrackControl(kind: Option[BacktrackControlKind], name: Option[String]): IRNodeData =
    // An empty name is the same as no name.
    val named = name.filter(_.nonEmpty)
    kind match
      case Some(BacktrackControlKind.Fail | BacktrackControlKind.F) => IRNodeData.Chars(CharSet.empty)
      case Some(BacktrackControlKind.Accept)      => IRNodeData.BacktrackControl(IRBacktrackControl.Accept)
      case Some(BacktrackControlKind.Commit)      => IRNodeData.BacktrackControl(IRBacktrackControl.Commit)
      case Some(BacktrackControlKind.Prune)       => IRNodeData.BacktrackControl(IRBacktrackControl.Prune)
      case Some(BacktrackControlKind.Skip)        => IRNodeData.BacktrackControl(IRBacktrackControl.Skip(named))
      case Some(BacktrackControlKind.Then)        => IRNodeData.BacktrackControl(IRBacktrackControl.Then)
      case Some(BacktrackControlKind.Mark) | None =>
        IRNodeData.BacktrackControl(IRBacktrackControl.Mark(name.getOrElse("")))

  /** Builds a look-behind, which never matches if the engine tries no start for it.
    *
    * Java's `TreeInfo` lengths can make it so (e.g. `(?<=\X)`, where `\X` counts one for the minimum and zero for the
    * maximum).
    */
  def buildLookBehind(negative: Boolean, child: Node): IRNodeData =
    checker.matching(child) match
      case IRLookBehindMatch.Forward(Seq(IRLookBehindStarts.Java(min, max, _))) if 0 <= max && max < min =>
        if negative then IRNodeData.Empty else IRNodeData.Chars(CharSet.empty)
      case matching => IRNodeData.LookBehind(negative, scoped(build(child)), matching)

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
    case GroupKind.PositiveLookBehind(_) => Left(buildLookBehind(negative = false, child))
    case GroupKind.NegativeLookBehind(_) => Left(buildLookBehind(negative = true, child))
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
    case GroupKind.NonAtomicPositiveLookAhead(_)  => Left(IRNodeData.NonAtomicLookAhead(scoped(build(child))))
    case GroupKind.NonAtomicPositiveLookBehind(_) =>
      Left(IRNodeData.NonAtomicLookBehind(scoped(build(child)), checker.matching(child)))
    case GroupKind.ScriptRun(_)        => Left(IRNodeData.Unsupported(node.data))
    case GroupKind.AtomicScriptRun(_)  => Left(IRNodeData.Unsupported(node.data))
    case GroupKind.ScanSubstring(_, _) => Left(IRNodeData.Unsupported(node.data))
    case GroupKind.Absence             => Left(IRNodeData.Unsupported(node.data))

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
    // `\G` is the start of the search, and the checkers search from the start of the input.
    case AssertKind.Sticky                                              => IRNodeData.Assert(IRAssertKind.TextBegin)
    case AssertKind.Cut                                                 => IRNodeData.ResetMatchStart
    case AssertKind.Boundary(Some(_)) | AssertKind.NonBoundary(Some(_)) =>
      IRNodeData.Unsupported(node.data)

  def buildCaret(): IRNodeData =
    if featureSet.caretIsLineBegin || context.multiline then
      IRNodeData.Assert(IRAssertKind.LineBegin(context.lineTerminators, featureSet.lineBeginAtEnd))
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
