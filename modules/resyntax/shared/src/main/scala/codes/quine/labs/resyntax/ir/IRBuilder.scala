package codes.quine.labs.resyntax.ir

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackControlKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.ClassItem
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
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

object IRBuilder:

  /** Throws [[IRBuildingException]] for an error that needs the whole pattern (e.g. a reference to a non-existent
    * group, or a look-behind that the dialect does not allow).
    */
  def build(node: Node, flagSet: FlagSet, dialect: Dialect): IRPattern =
    build(node, flagSet, dialect, None)

  private[ir] def build(
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
    val perlUtf8Pattern = dialect match
      case Dialect.Perl(true, PerlSource.String) => true
      case Dialect.Perl(utf8, _)                 => usesPerlUnicodeSyntax(node) || utf8 && hasNonAsciiLiteral(node)
      case _                                     => false
    val chars = new IRCharResolver(dialect, mode, perlUnicodeRules)
    val supplementary = if dialect.isJava then lastSupplementary(node) else -1
    val checker =
      new LookBehindChecker(
        table,
        resolver,
        featureSet,
        charWidth(table, chars, dialect, mode, perlUtf8Pattern),
        supplementary,
        if dialect.isPerl then perlMinSubtract(table, perlUnicodeRules) else _ => 0L,
        node => table.charFlagsOf(node).flatMap(chars.rubyClassWidths(node, _))
      )
    val backReferences = new IRValidator(table, resolver, checker, featureSet).validate(node)
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
    builder.findsTries = dialect.isPerl && hasThen(node)
    val root = builder.build(node)
    IRPattern(
      root,
      table.count,
      table.names,
      IRBackReferenceSemantics(featureSet.unsetBackReferenceMatchesEmpty, featureSet.backReferenceChoice),
      featureSet.anchored,
      mode,
      notEmpty(starts),
      builder.tries.toMap
    )

  /** @param utf
    *   PCRE's `(*UTF)` start option
    */
  private def unitMode(dialect: Dialect, flagSet: FlagSet, utf: Boolean): UnitMode =
    dialect match
      case Dialect.JavaScript =>
        if flagSet.unicode || flagSet.unicodeSets then UnitMode.CodePoint else UnitMode.Utf16
      // Java matches code points, and reads a lone surrogate as a code point too.
      case Dialect.Java        => UnitMode.CodePoint
      case Dialect.DotNet      => UnitMode.Utf16
      case Dialect.Python(tpe) =>
        if tpe == Dialect.Python.Type.Bytes then UnitMode.Byte else UnitMode.CodePoint
      // Ruby's `n` is the encoding option (NOENCODING), which `FlagSet` keeps as `explicitCapture`.
      case Dialect.Ruby(encoding) =>
        if flagSet.explicitCapture || encoding == Dialect.Ruby.Encoding.Ascii8Bit then UnitMode.Byte
        else UnitMode.CodePoint
      case Dialect.Perl(utf8, _) => if utf8 then UnitMode.CodePoint else UnitMode.Byte
      case Dialect.PCRE          => if flagSet.unicode || utf then UnitMode.CodePoint else UnitMode.Byte

  /** Perl's `/d` rules at a back-reference are the rules of a native string (see [[IRCharResolver.referenceFlags]]).
    */
  def caseEquivalence(dialect: Dialect, mode: UnitMode, flags: IRCharFlags): IRCaseEquivalence =
    IRCaseEquivalence.of(dialect, mode, flags, perlNative = dialect.isPerl && flags.charset == IRCharset.Default)

  def backReferenceFolding(dialect: Dialect, mode: UnitMode, flags: IRCharFlags): IRBackReferenceFolding =
    if !flags.ignoreCase then IRBackReferenceFolding.Simple
    else
      dialect match
        case _: Dialect.Ruby if mode == UnitMode.CodePoint => IRBackReferenceFolding.Full(0)
        case _: Dialect.Perl                               =>
          flags.charset match
            case IRCharset.Default                            => IRBackReferenceFolding.Simple
            case IRCharset.Locale                             => IRBackReferenceFolding.Full(0x100)
            case _: IRCharset.Ascii if flags.caselessRestrict => IRBackReferenceFolding.Full(0x80)
            case _                                            => IRBackReferenceFolding.Full(0)
        case _ => IRBackReferenceFolding.Simple

  /** Returns the width of a literal, an escape or a class in a look-behind, where the dialects counting it differently
    * from one care.
    *
    * Perl counts the folded length of a character under `i` (e.g. `(?<=ß{128})` is too long), and a class counts its
    * longest character but not its ranges (e.g. `[ßa]` is two, and `[ß-ẞ]` is one). The folding is one for a Latin-1
    * character under `l`, and for one with ASCII characters under `aa` and `l`, except `ẞ` and, under `aa`, `ß`.
    *
    * Ruby (Onigmo) matches a character folded to several characters under `i` as the folding (e.g. `ß` as `ss`), and a
    * class having such a character and another has a variable width (e.g. `[ßa]`, but `[ß]` is two). The nodes are
    * Ruby's escapes of the bytes of a character (e.g. `\xC3\x9F`), or one node otherwise.
    */
  /** Checks whether Perl folds the character to several characters (e.g. `ß` to `ss`) under the flags. */
  private def perlFoldsMulti(value: Int, flags: IRCharFlags, perlUnicodeRules: Boolean): Boolean =
    val hasAscii = FullCaseFolding(value).iterator.exists(_ < 0x80)
    val one = flags.charset match
      case IRCharset.Default  => !perlUnicodeRules
      case IRCharset.Unicode  => false
      case IRCharset.Locale   => value < 0x100 || hasAscii && value != 0x1e9e
      case _: IRCharset.Ascii => flags.caselessRestrict && hasAscii && value != 0xdf && value != 0x1e9e
    flags.ignoreCase && !one

  /** Returns how much Perl lessens the minimum length of the nodes of a sequence, where literals may match one
    * character folded to several ones (e.g. `ss` matches `ß`).
    *
    * Perl finds such literals from the left, the longest first, as `join_exact` does. It lessens the minimum also in a
    * pattern that is not UTF-8, where the string decides the rules of `/d`, so it follows the rules at run time.
    */
  private def perlMinSubtract(table: CaptureTable, perlUnicodeRules: Boolean): Seq[Node] => Long =
    lazy val folds = FullCaseFolding.multiCodePointFoldings().keySet
    lazy val maxLength = folds.map(_.length).max
    nodes =>
      val chars = nodes.map: node =>
        val value = node.data match
          case NodeData.Literal(value)                                 => Some(value)
          case NodeData.Backslash(value: BackslashKind.BackslashValue) => Some(value.value)
          case _                                                       => None
        (value, table.charFlagsOf(node)) match
          case (Some(value), Some(flags)) if perlFoldsMulti(value, flags, perlUnicodeRules) =>
            Some(FullCaseFolding(value)).filter(_.length == 1).map(_(0))
          case _ => None
      var subtract = 0L
      var i = 0
      while i < chars.length do
        val found = (maxLength to 2 by -1).find: n =>
          i + n <= chars.length && chars.slice(i, i + n).forall(_.isDefined) &&
            folds.contains(UString(chars.slice(i, i + n).flatten*))
        found match
          case Some(n) =>
            subtract += n - 1
            i += n
          case None => i += 1
      subtract

  private def charWidth(
      table: CaptureTable,
      chars: IRCharResolver,
      dialect: Dialect,
      mode: UnitMode,
      perlUtf8Pattern: Boolean
  ): Seq[Node] => LookBehindChecker.Width =
    import LookBehindChecker.Width
    def single(n: Int): Width = Width(n, n)
    // Perl reads the bytes of a byte string as Latin-1 characters.
    if mode != UnitMode.CodePoint && !dialect.isPerl then return nodes => single(nodes.size)
    dialect match
      case _: Dialect.Perl =>
        def fold(value: Int, flags: IRCharFlags): Int =
          if perlFoldsMulti(value, flags, perlUtf8Pattern) then FullCaseFolding(value).length else 1
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
                  single(fold(IRCharResolver.decodeUtf8(nodes.collect {
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
  private def hasNonAsciiLiteral(node: Node): Boolean =
    deepExists(node):
      case NodeData.Literal(value)           => value >= 0x80
      case ClassItemData.ClassLiteral(value) => value >= 0x80
      case QuoteLiteral.QuoteValue(value)    => value >= 0x80

  /** Returns the start of the last node with a supplementary character or a surrogate in the pattern text, or `-1`. */
  private def lastSupplementary(node: Node): Int =
    def isSupplementary(c: Int): Boolean = c >= 0x10000 || Character.isSurrogate(c.toChar) && c <= 0xffff
    val self = node.data match
      case NodeData.Literal(value) => isSupplementary(value)
      case NodeData.Class(_, item) =>
        deepExists(item) { case ClassItemData.ClassLiteral(value) => isSupplementary(value) }
      case NodeData.Command(CommandKind.Comment(text)) => UString.fromString(text).iterator.exists(isSupplementary)
      case _                                           => false
    NodeKey.children(node).map(lastSupplementary).foldLeft(if self then node.loc.start else -1)(_.max(_))

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

  private def hasThen(node: Node): Boolean =
    NodeKey.exists(node):
      _.data match
        case NodeData.Command(CommandKind.BacktrackControl(Some(BacktrackControlKind.Then), _)) => true
        case _                                                                                  => false

  private def hasBackReference(node: Node): Boolean =
    NodeKey.exists(node):
      _.data match
        case NodeData.Backslash(_: BackslashKind.EscapeBackReference) => true
        case NodeData.Command(_: CommandKind.PBackReference)          => true
        case _                                                        => false

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

/** PerlTrieWord is the kind of the literal starting an alternative, which Perl puts in a trie with the same kind. */
private[ir] enum PerlTrieWord:
  case Empty, Exact, Fold

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
      case NodeData.Disjunction(children)                            => buildDisjunction(node, children)
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

  /** Builds consecutive literal characters together (e.g. for multi-character case folding). */
  def buildLiterals(nodes: Seq[Node]): Seq[IRNode] =
    val run = nodes.map(node => (node, charFlags))
    if run.exists(_._2.canonicalEquivalence) then
      run.map((node, _) => IRNode(IRNodeData.Unsupported(node.data), node.loc))
    else
      resolveCharacter match
        case Some(f) => run.map((node, flags) => IRNode(f(node, flags), node.loc))
        case None    => chars.literals(run)

  /** Whether it finds the alternatives that Perl compiles into a trie, which matter only with `(*THEN)`. */
  var findsTries: Boolean = false

  /** The alternatives that Perl compiles into a trie, by the location of the alternation. */
  val tries: mutable.Map[SourceLocation, Seq[(Int, Int)]] = mutable.Map.empty

  def buildDisjunction(node: Node, children: Seq[Node]): Either[IRNodeData, IRNode] =
    // The flags at an alternative are the ones after the previous alternatives, as `(?i)` reaches the next ones.
    val nodes = children.map(child => (if findsTries then trieWord(child, context) else None, build(child)))
    if findsTries then
      val runs = trieRuns(nodes.map(_._1))
      if runs.nonEmpty then tries(node.loc) = runs
    Left(IRNodeData.Disjunction(nodes.map(_._2)))

  /** Returns the kind of the literal that Perl starts the trie word of the alternative with, if it can be in a trie.
    *
    * A literal is `EXACT`, or `EXACTFU` for a case-insensitive one of two or more characters with case variants, and an
    * empty alternative is a word of any kind.
    */
  def trieWord(node: Node, context: BuildingContext): Option[PerlTrieWord] =
    def literal(node: Node): Boolean = node.data match
      case NodeData.Literal(_) | NodeData.Backslash(_: BackslashKind.Escape)        => true
      case NodeData.Class(false, ClassItem.Value(ClassItemData.ClassLiteral(_), _)) => true
      case _                                                                        => false
    def hasCases(node: Node, context: BuildingContext): Boolean =
      chars.resolve(node, context.charFlags) match
        case IRNodeData.Chars(set) => set.size > 1
        case _                     => true
    def sequence(nodes: List[Node], context: BuildingContext): Option[PerlTrieWord] = nodes match
      case Nil                                                             => Some(PerlTrieWord.Empty)
      case Node(NodeData.Command(_: CommandKind.Comment), _) :: rest       => sequence(rest, context)
      case Node(NodeData.Command(CommandKind.InlineFlag(diff)), _) :: rest => sequence(rest, context.update(diff))
      case Node(NodeData.Group(GroupKind.NonCapture, child), _) :: rest    => sequence(child :: rest, context)
      case Node(NodeData.Sequence(children), _) :: rest                    => sequence(children.toList ++ rest, context)
      case first :: _ if literal(first)                                    =>
        val run = nodes.takeWhile(literal)
        if !context.charFlags.ignoreCase || !run.exists(hasCases(_, context)) then Some(PerlTrieWord.Exact)
        else Option.when(run.size >= 2)(PerlTrieWord.Fold)
      case _ => None
    sequence(List(node), context)

  /** Returns the runs of two or more alternatives in a trie, which start with a literal of the same kind. */
  def trieRuns(words: Seq[Option[PerlTrieWord]]): Seq[(Int, Int)] =
    val runs = Seq.newBuilder[(Int, Int)]
    var i = 0
    while i < words.size do
      words(i) match
        case Some(kind) if kind != PerlTrieWord.Empty =>
          var j = i + 1
          while j < words.size && words(j).exists(w => w == kind || w == PerlTrieWord.Empty) do j += 1
          if j - i >= 2 then runs.addOne((i, j))
          i = j
        case _ => i += 1
    runs.result()

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
      case NodeData.Backslash(BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline)) if dialect.isJava =>
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
    * It matters only for a group with `\R` (which has two ways), with captures in a pattern with back-references, or
    * matching the empty string (whose iteration `GroupCurly` rejects).
    */
  def isJavaGroupCurly(child: Node, quantifier: Quantifier): Boolean =
    def isGroup = child.data match
      case NodeData.Group(GroupKind.IndexedCapture | GroupKind.NonCapture | _: GroupKind.NamedCapture, _) => true
      case NodeData.Group(_: GroupKind.InlineFlag, _)                                                     => true
      case _                                                                                              => false
    def matters = checker.javaMinLength(child) == 0 || NodeKey.exists(child):
      case Node(NodeData.Backslash(BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline)), _) => true
      case node @ Node(NodeData.Group(_, _), _) => hasBackReference && node != child && table.indexOf(node).isDefined
      case _                                    => false
    val counted = quantifier match
      case Quantifier.Question(_) => false
      case _                      => quantifier.strategy != BacktrackStrategy.Possessive
    dialect.isJava && counted && isGroup && checker.javaDeterministic(child) && matters

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
