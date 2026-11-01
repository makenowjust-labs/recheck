package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackControlKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.EscapeClassKind
import codes.quine.labs.resyntax.ast.EscapeStyle
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.QuoteLiteral
import codes.quine.labs.resyntax.ast.Reference
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.FullCaseFolding
import codes.quine.labs.resyntax.charset.UnitMode

object IRBuilder:

  /** Throws [[IRBuildingException]] for an error that needs the whole pattern (e.g. a reference to a non-existent
    * group, or a look-behind that the dialect does not allow).
    */
  def build(node: Node, flagSet: FlagSet, dialect: Dialect): IRPattern =
    build(node, flagSet, dialect, None)

  private[build] def build(
      node: Node,
      flagSet: FlagSet,
      dialect: Dialect,
      resolveCharacter: Option[(Node, IRCharFlags) => IRNodeData]
  ): IRPattern =
    val starts = startOptions(node)
    val featureSet = IRFeatureSet.from(flagSet, dialect, starts)
    val mode = unitMode(dialect, flagSet, featureSet.utf)
    if dialect.isPython && mode == UnitMode.CodePoint && flagSet.localeUpper then
      throw new IRBuildingException("Cannot use LOCALE flag with a str pattern", SourceLocation.Invalid)
    if dialect.isPython && mode == UnitMode.Byte && flagSet.unicode then
      throw new IRBuildingException("Cannot use UNICODE flag with a bytes pattern", SourceLocation.Invalid)
    if dialect.isPython then checkPythonGlobalFlags(node, flagSet)
    val context = BuildingContext(flagSet, dialect, featureSet)
    val table = CaptureAnalyzer.analyze(node, context)
    val resolver = new ReferenceResolver(table, dialect, featureSet)
    val perlUnicodeRules = dialect match
      case Dialect.Perl(utf8, _) => utf8 || usesPerlUnicodeSyntax(node)
      case _                     => false
    // Perl decides the widths in a look-behind on compiling the pattern, by whether the pattern itself is UTF-8.
    val perlUTF8Pattern = dialect match
      case Dialect.Perl(true, PerlSource.String) => true
      case Dialect.Perl(utf8, _)                 => usesPerlUnicodeSyntax(node) || utf8 && hasNonASCIILiteral(node)
      case _                                     => false
    val chars = new IRCharResolver(dialect, mode, perlUnicodeRules)
    val lookBehindAnalyzer =
      new LookBehindAnalyzer(
        table,
        resolver,
        featureSet,
        charWidth(table, chars, dialect, mode, perlUTF8Pattern),
        node => table.charFlagsOf(node).flatMap(chars.rubyClassWidths(node, _))
      )
    val backReferences = new SemanticAnalyzer(table, resolver, lookBehindAnalyzer, featureSet).analyze(node)
    val builder =
      new IRBuilder(
        context,
        table,
        backReferences,
        chars,
        lookBehindAnalyzer,
        resolveCharacter,
        hasCall(node)
      )
    val root = builder.build(node)
    IRPattern(
      root,
      table.count,
      table.names,
      IRBackReferenceSemantics(featureSet.unsetBackReferenceMatchesEmpty, featureSet.backReferenceChoice),
      featureSet.anchored,
      mode,
      notEmpty(starts)
    )

  /** Returns the unit of the input, where `utf` is PCRE's `(*UTF)` start option. */
  private def unitMode(dialect: Dialect, flagSet: FlagSet, utf: Boolean): UnitMode =
    dialect match
      case Dialect.JavaScript =>
        if flagSet.unicode || flagSet.unicodeSets then UnitMode.CodePoint else UnitMode.UTF16
      // Java matches code points, and reads a lone surrogate as a code point too.
      case Dialect.Java        => UnitMode.CodePoint
      case Dialect.DotNet      => UnitMode.UTF16
      case Dialect.Python(tpe) =>
        if tpe == Dialect.Python.Type.Bytes then UnitMode.Byte else UnitMode.CodePoint
      // Ruby's `n` is the encoding option (NOENCODING), which `FlagSet` keeps as `explicitCapture`.
      case Dialect.Ruby(encoding) =>
        if flagSet.explicitCapture || encoding == Dialect.Ruby.Encoding.ASCII8Bit then UnitMode.Byte
        else UnitMode.CodePoint
      case Dialect.Perl(utf8, _) => if utf8 then UnitMode.CodePoint else UnitMode.Byte
      case Dialect.PCRE          => if flagSet.unicode || utf then UnitMode.CodePoint else UnitMode.Byte

  /** Checks whether Perl takes the full case folding of the character under the flags (e.g. `ß` as `ss`). */
  private def perlFoldsMulti(value: Int, flags: IRCharFlags, perlUnicodeRules: Boolean): Boolean =
    val hasASCII = FullCaseFolding(value).iterator.exists(_ < 0x80)
    val one = flags.charset match
      case IRCharset.Default  => !perlUnicodeRules
      case IRCharset.Unicode  => false
      case IRCharset.Locale   => value < 0x100 || hasASCII && value != 0x1e9e
      case _: IRCharset.ASCII => flags.caselessRestrict && hasASCII && value != 0xdf && value != 0x1e9e
    flags.ignoreCase && !one

  /** Returns the width of a literal, an escape or a class in a look-behind, where the dialects counting it differently
    * from one care.
    *
    * Perl counts the folded length of a character under `i` (e.g. `(?<=ß{128})` is too long), and a class counts its
    * longest character but not its ranges (e.g. `[ßa]` is two, and `[ß-ẞ]` is one). The folding is one for a Latin-1
    * character under `l`, and for one with ASCII characters under `aa` and `l`, except `ẞ` and, under `aa`, `ß`.
    *
    * Ruby (Onigmo) counts a character folded to several characters under `i` by the folding (e.g. `ß` as two), and a
    * class having such a character and another has a variable width (e.g. `[ßa]`, but `[ß]` is two). The nodes are
    * Ruby's escapes of the bytes of a character (e.g. `\xC3\x9F`), or one node otherwise.
    */
  private def charWidth(
      table: CaptureTable,
      chars: IRCharResolver,
      dialect: Dialect,
      mode: UnitMode,
      perlUTF8Pattern: Boolean
  ): Seq[Node] => LookBehindAnalyzer.Width =
    import LookBehindAnalyzer.Width
    def single(n: Int): Width = Width(n, n)
    // Perl reads the bytes of a byte string as Latin-1 characters.
    if mode != UnitMode.CodePoint && !dialect.isPerl then return nodes => single(nodes.size)
    dialect match
      case _: Dialect.Perl =>
        def fold(value: Int, flags: IRCharFlags): Int =
          if perlFoldsMulti(value, flags, perlUTF8Pattern) then FullCaseFolding(value).length else 1
        def classWidth(data: ClassItemData, flags: IRCharFlags): Int = data match
          case ClassItemData.ClassUnion(items) => items.map(i => classWidth(i.data, flags)).maxOption.getOrElse(1)
          case value: ClassItemData.ClassValue => fold(value.value, flags)
          case ClassItemData.ClassRange(b, e) if b.data.value == e.data.value => fold(b.data.value, flags)
          case _                                                              => 1
        nodes =>
          val node = nodes.head
          val width = table
            .charFlagsOf(node)
            .fold(1): flags =>
              node.data match
                case NodeData.Literal(value)                                 => fold(value, flags)
                case NodeData.Backslash(value: BackslashKind.BackslashValue) => fold(value.value, flags)
                case NodeData.Class(false, item)                             => classWidth(item.data, flags)
                case _                                                       => 1
          Width(1, width.toLong)
      case _: Dialect.Ruby =>
        def fold(value: Int): Int = FullCaseFolding(value).length.max(1)
        nodes =>
          val node = nodes.head
          table
            .charFlagsOf(node)
            .filter(_.ignoreCase)
            .fold(single(1)): flags =>
              node.data match
                case _ if nodes.size > 1 =>
                  single(fold(IRCharResolver.decodeUTF8(nodes.collect {
                    case Node(NodeData.Backslash(BackslashKind.Escape(_, b)), _) => b
                  })))
                case NodeData.Literal(value)                                 => single(fold(value))
                case NodeData.Backslash(value: BackslashKind.BackslashValue) => single(fold(value.value))
                case NodeData.Class(false, _)                                =>
                  chars.resolve(node, flags.copy(ignoreCase = false)) match
                    case IRNodeData.Chars(set) if set.size == 1 => single(fold(set.head))
                    case _                                      =>
                      chars.rubyClassWidths(node, flags).fold(single(1))(ws => Width(ws.min, ws.max.toLong))
                case _ => single(1)
      case _ => nodes => single(nodes.size)

  /** Checks Python's flags with the global inline flags, which `re` checks together. */
  private def checkPythonGlobalFlags(node: Node, flagSet: FlagSet): Unit =
    def inline(node: Node): Seq[FlagSet] = node.data match
      case NodeData.Command(CommandKind.InlineFlag(diff)) => Seq(diff.added)
      case _                                              => NodeKey.children(node).flatMap(inline)
    val flagSets = flagSet +: inline(node)
    def has(p: FlagSet => Boolean): Boolean = flagSets.exists(p)
    if has(_.ascii) && has(_.unicode) then
      throw new IRBuildingException("ASCII and UNICODE flags are incompatible", SourceLocation.Invalid)
    if has(_.ascii) && has(_.localeUpper) then
      throw new IRBuildingException("ASCII and LOCALE flags are incompatible", SourceLocation.Invalid)

  /** Checks whether a Perl pattern switches `/d` to Unicode rules by itself (perlre "Which character set modifier is in
    * effect?").
    *
    * `/d` uses Unicode rules when the pattern or the string is UTF-8, so a pattern of byte strings uses them only by a
    * code point above `U+00FF`, `\p{...}`, `\N{...}` (including `\N{U+...}`) or `\b{...}`.
    */
  private def usesPerlUnicodeSyntax(node: Node): Boolean =
    deepExists(node):
      case AssertKind.Boundary(Some(_)) | AssertKind.NonBoundary(Some(_)) => true
      case BackslashKind.Escape(EscapeStyle.NamedHex, _)                  => true
      case BackslashKind.Escape(_, value)                                 => value > 0xff
      case _: EscapeClassKind.CharName                                    => true
      case _: EscapeClassKind.UnicodeProperty | _: EscapeClassKind.UnicodeBareProperty |
          _: EscapeClassKind.UnicodePropertyValue | _: EscapeClassKind.NonUnicodeProperty |
          _: EscapeClassKind.NonUnicodeBareProperty | _: EscapeClassKind.NonUnicodePropertyValue =>
        true

  /** Checks whether a Perl pattern has a non-ASCII literal, which makes a regex literal UTF-8 with `use utf8`. */
  private def hasNonASCIILiteral(node: Node): Boolean =
    deepExists(node):
      case NodeData.Literal(value)           => value >= 0x80
      case ClassItemData.ClassLiteral(value) => value >= 0x80
      case QuoteLiteral.QuoteValue(value)    => value >= 0x80

  private def deepExists(x: Any)(p: PartialFunction[Any, Boolean]): Boolean =
    p.applyOrElse(
      x,
      {
        case xs: Iterable[?] => xs.exists(deepExists(_)(p))
        case x: Option[?]    => x.exists(deepExists(_)(p))
        case _: String       => false
        case x: Product      => x.productIterator.exists(deepExists(_)(p))
        case _               => false
      }
    )

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

  private def notEmpty(startOptions: Seq[String]): IRNotEmpty =
    if startOptions.contains("NOTEMPTY") then IRNotEmpty.Anywhere
    else if startOptions.contains("NOTEMPTY_ATSTART") then IRNotEmpty.AtStart
    else IRNotEmpty.Never

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

private[build] class IRBuilder(
    initial: BuildingContext,
    table: CaptureTable,
    backReferences: Map[NodeKey, Seq[Int]],
    chars: IRCharResolver,
    lookBehindAnalyzer: LookBehindAnalyzer,
    resolveCharacter: Option[(Node, IRCharFlags) => IRNodeData],
    hasCall: Boolean
):
  private val dialect = initial.dialect

  private val featureSet = initial.featureSet

  var context: BuildingContext = initial

  def build(node: Node): IRNode =
    val result = node.data match
      case NodeData.Disjunction(children)                            => Left(buildDisjunction(children))
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

  def charFlags: IRCharFlags = context.charFlags

  def buildCharacter(node: Node): IRNodeData =
    val flags = charFlags
    // Java's CANON_EQ matches canonically equivalent sequences, which the checkers do not handle.
    if flags.canonicalEquivalence then IRNodeData.Unsupported(node.data)
    else resolveCharacter.fold(chars.resolve(node, flags))(_(node, flags))

  /** Builds a part, which is literal characters in a row, together (e.g. for multi-character case folding). */
  def buildPart(nodes: Seq[Node]): Seq[IRNode] =
    val part = nodes.map(node => (node, charFlags))
    if part.exists(_._2.canonicalEquivalence) then
      part.map((node, _) => IRNode(IRNodeData.Unsupported(node.data), node.loc))
    else
      resolveCharacter match
        case Some(f) => part.map((node, flags) => IRNode(f(node, flags), node.loc))
        case None    => chars.literals(part)

  def buildDisjunction(children: Seq[Node]): IRNodeData =
    // The flags at an alternative are the ones after the previous alternatives, as `(?i)` reaches the next ones.
    IRNodeData.Disjunction(children.map(build))

  def buildSequence(children: Seq[Node]): Either[IRNodeData, IRNode] =
    children match
      case Seq()     => Left(IRNodeData.Empty)
      case children0 =>
        val children = chars.joinRubyByteRepeats(children0).toIndexedSeq
        val nodes = Seq.newBuilder[IRNode]
        var i = 0
        while i < children.length do
          if chars.isLiteral(children(i)) then
            var j = i
            while j < children.length && chars.isLiteral(children(j)) do j += 1
            nodes ++= buildPart(children.slice(i, j))
            i = j
          else
            nodes += build(children(i))
            i += 1
        // A part resolved together may become one node (e.g. `ss` under Ruby's case folding).
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
    case CommandKind.Comment(_) => Left(IRNodeData.Empty)
    // The start options are of the whole pattern, which `IRBuilder.build` reads.
    case CommandKind.StartOption(_, _)            => Left(IRNodeData.Empty)
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

  def buildConditional(node: Node, test: ConditionalTest, yes: Node, no: Option[Node]): Either[IRNodeData, IRNode] =
    def branch(node: Option[Node]): IRNode = node.fold(IRNode(IRNodeData.Empty))(n => scoped(build(n)))
    def captured(indices: Seq[Int]): Either[Boolean, IRCondition] =
      // .NET's group `0` is the whole match, which is not set yet.
      val set = indices.filter(_ != 0)
      if set.isEmpty then Left(false)
      // Ruby tests only the first group of a name shared by groups.
      else if dialect.isRuby then Right(IRCondition.Captured(set.take(1)))
      else Right(IRCondition.Captured(set))
    def resolved: Seq[Int] =
      backReferences.getOrElse(NodeKey(node), throw new IllegalStateException(s"No resolved test: $node"))
    // A recursion test is false out of a recursion, so it is constant without calls.
    // PCRE reads `(R)` and `(R1)` as capture tests when a group has the name.
    def recursion(name: Option[String]): Option[Either[Boolean, IRCondition]] =
      name.filter(dialect.isPCRE && table.names.contains(_)) match
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
        IRCondition.LookBehind(false, scoped(build(body)), lookBehindAnalyzer.matching(body))
      case Some(_: GroupKind.NegativeLookBehind) =>
        IRCondition.LookBehind(true, scoped(build(body)), lookBehindAnalyzer.matching(body))

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

  def buildLookBehind(negative: Boolean, child: Node): IRNodeData =
    IRNodeData.LookBehind(negative, scoped(build(child)), lookBehindAnalyzer.matching(child))

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
      Left(IRNodeData.NonAtomicLookBehind(scoped(build(child)), lookBehindAnalyzer.matching(child)))
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
    IRNodeData.BackReference(indices, chars.referenceFlags(charFlags))

  def buildAssert(node: Node, kind: AssertKind): IRNodeData = kind match
    case AssertKind.Boundary(None) =>
      val word = chars.wordSet(charFlags)
      IRNodeData.Assert(
        chars
          .wordMarks(charFlags)
          .fold(IRAssertKind.WordBoundary(word)):
            IRAssertKind.MarkWordBoundary(word, _)
      )
    case AssertKind.NonBoundary(None) =>
      val word = chars.wordSet(charFlags)
      IRNodeData.Assert(
        chars
          .wordMarks(charFlags)
          .fold(IRAssertKind.NotWordBoundary(word)):
            IRAssertKind.NotMarkWordBoundary(word, _)
      )
    case AssertKind.Begin    => IRNodeData.Assert(IRAssertKind.TextBegin)
    case AssertKind.LowerEnd => IRNodeData.Assert(IRAssertKind.TextEnd)
    case AssertKind.UpperEnd =>
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
