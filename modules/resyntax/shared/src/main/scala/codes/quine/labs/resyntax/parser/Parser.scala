package codes.quine.labs.resyntax.parser

import scala.annotation.switch
import scala.collection.mutable
import scala.util.boundary

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BackReferenceStyle
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackControlKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.BoundaryModifier
import codes.quine.labs.resyntax.ast.CaseCommandKind
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.EscapeClassKind
import codes.quine.labs.resyntax.ast.EscapeStyle
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.NameStyle
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.QuoteLiteral
import codes.quine.labs.resyntax.ast.Reference
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.parser.Parser.ParsingContext

object Parser:
  private[parser] final case class ParsingContext(
      inClass: Boolean,
      skipsComment: Boolean
  )

  private[parser] object ParsingContext:
    def from(featureSet: FeatureSet): ParsingContext = ParsingContext(
      inClass = false,
      skipsComment = featureSet.skipsComment
    )

  def parse(source: String, flagSet: FlagSet, dialect: Dialect): Node =
    val analysis = QuickAnalysis.from(source)
    val featureSet = FeatureSet.from(analysis, dialect, flagSet)
    val parser = new Parser(source, analysis, featureSet)
    parser.parse()

private[parser] final class Parser(
    private val source: String,
    private val analysis: QuickAnalysis,
    private val featureSet: FeatureSet
):
  private var offset: Int = 0

  private var currentChar: Char =
    // Note that `\u0000` does not indicate the end of source.
    // If we need to check the end, use the `isEnd` method.
    if source.isEmpty then '\u0000' else source.charAt(offset)

  private var context: ParsingContext = ParsingContext.from(featureSet)

  // Ruby's `\u{X Y}` is read as the first code point, and the rest are kept here until the caller takes them.
  private var pendingCodePoints: List[Int] = Nil

  def parse(): Node =
    val node = parseDisjunction()
    if !isEnd then
      assert(currentChar == ')', "Unexpected")
      fail("Unmatched ')'")
    node

  private def parseDisjunction(): Node =
    val start = offset
    val builder = Seq.newBuilder[Node]
    builder.addOne(parseSequence())

    while !isEnd && currentChar == '|' do
      next()
      builder.addOne(parseSequence())

    val nodes = builder.result()
    if nodes.size == 1 then return nodes.head

    val end = offset
    Node(NodeData.Disjunction(nodes), SourceLocation(start, end))

  private def parseSequence(): Node =
    val start = offset
    val stack = mutable.Stack.empty[Node]

    while !isSequenceStop do parseSequenceItem(stack)

    val nodes = stack.toSeq
    if nodes.size == 1 then return nodes.head

    val end = offset
    Node(NodeData.Sequence(nodes.reverse), SourceLocation(start, end))

  private def parseSequenceItem(stack: mutable.Stack[Node]): Unit =
    if context.skipsComment && skipComment() then return

    (currentChar: @switch) match
      case '*' =>
        parseSimpleRepeat(stack, Quantifier.Star(_))
      case '+' =>
        parseSimpleRepeat(stack, Quantifier.Plus(_))
      case '?' =>
        parseSimpleRepeat(stack, Quantifier.Question(_))
      case '{' =>
        parseCurlyQuantifier(stack)
      case '}' =>
        parseCloseCurly(stack)
      case '(' =>
        parseGroup(stack)
      case '^' =>
        parseCaret(stack)
      case '$' =>
        parseDollar(stack)
      case '.' =>
        parseDot(stack)
      case '\\' =>
        parseBackslash(stack)
      case '[' =>
        parseClass(stack)
      case ']' =>
        parseCloseBracket(stack)
      case _ =>
        parseLiteral(stack)

  private def parseSimpleRepeat(stack: mutable.Stack[Node], buildQuantifier: BacktrackStrategy => Quantifier): Unit =
    val node = popRepeatable(stack)
    next()
    val strategy = parseBacktrackStrategy(exact = false)
    val quantifier = buildQuantifier(strategy)
    pushRepeat(stack, node, quantifier)

  private def parseCurlyQuantifier(stack: mutable.Stack[Node]): Unit =
    val save = offset
    next()
    skipBraceBlank()
    val minOption = parseRepeatCountOption()
    skipBraceBlank()

    if minOption.isDefined && currentChar == '}' then
      next()
      val strategy = parseBacktrackStrategy(exact = true)
      pushCurlyRepeat(stack, Quantifier.Exact(minOption.get, strategy))
      return

    if isEnd || currentChar != ',' || minOption.isEmpty && !featureSet.hasMaxBounded then
      resetBracket(stack, save)
      return

    next()
    skipBraceBlank()
    val maxOption = parseRepeatCountOption()
    skipBraceBlank()

    if isEnd || currentChar != '}' || minOption.isEmpty && maxOption.isEmpty then
      resetBracket(stack, save)
      return

    next()
    val strategy = parseBacktrackStrategy(exact = false)
    val quantifier = (minOption, maxOption) match
      case (Some(min), Some(max)) =>
        if max < min && !featureSet.allowsReversedBoundedRepeat then fail("Numbers out of order in {} quantifier", save)
        Quantifier.Bounded(min, max, strategy)
      case (Some(min), None) => Quantifier.Unbounded(min, strategy)
      case (None, Some(max)) => Quantifier.MaxBounded(max, strategy)
      case (None, None)      => throw new IllegalStateException("Unreachable")
    pushCurlyRepeat(stack, quantifier)

  private def pushCurlyRepeat(stack: mutable.Stack[Node], quantifier: Quantifier): Unit =
    stack.headOption match
      // Java ignores a curly quantifier following another quantifier: `a{2}{3}` matches `aa`.
      case Some(node @ Node(NodeData.Repeat(_, _), _)) if featureSet.ignoresCurlyAfterRepeat =>
        stack.pop()
        stack.push(node.copy(loc = node.loc.copy(end = offset)))
      case _ =>
        pushRepeat(stack, popRepeatable(stack), quantifier)

  private def parseRepeatCountOption(): Option[Int] =
    val start = offset
    parseSaturatedIntOption(featureSet.maxRepeatCount, "Number too big in {} quantifier", start)

  private def skipBraceBlank(force: Boolean = false): Unit =
    if force || featureSet.allowsBlankInBrace then while !isEnd && (currentChar == ' ' || currentChar == '\t') do next()

  private def popRepeatable(stack: mutable.Stack[Node]): Node =
    assert(stack.nonEmpty, "Nothing to repeat")
    val node = stack.pop()
    assertRepeatable(node)
    node

  private def assertRepeatable(node: Node): Unit =
    node.data match
      case NodeData.Group(_: GroupKind.PositiveLookAhead | _: GroupKind.NegativeLookAhead, _) =>
        assert(featureSet.allowsLookAheadRepeat, "Nothing to repeat")
      case NodeData.Group(_: GroupKind.PositiveLookBehind | _: GroupKind.NegativeLookBehind, _) =>
        assert(featureSet.allowsLookBehindRepeat, "Nothing to repeat")
      case NodeData.Caret | NodeData.Dollar | NodeData.Backslash(_: BackslashKind.Assert) =>
        assert(featureSet.allowsZeroWidthAssertRepeat, "Nothing to repeat")
      case NodeData.Repeat(_, _) =>
        assert(featureSet.allowsNestedRepeat, "Nested quantifier")
      case _ =>
        ()

  private def pushRepeat(stack: mutable.Stack[Node], node: Node, quantifier: Quantifier): Unit =
    val end = offset
    stack.push(Node(NodeData.Repeat(node, quantifier), node.loc.copy(end = end)))

  private def parseBacktrackStrategy(exact: Boolean): BacktrackStrategy =
    if isEnd || exact && !featureSet.allowsMeaninglessBacktrackStrategy then return BacktrackStrategy.Greedy

    if currentChar == '?' then
      next()
      return BacktrackStrategy.Lazy

    if featureSet.hasPossessiveBacktrackStrategy && currentChar == '+' then
      next()
      return BacktrackStrategy.Possessive

    BacktrackStrategy.Greedy

  private def resetBracket(stack: mutable.Stack[Node], save: Int): Unit =
    if !featureSet.allowsBrokenCurly then fail("Incomplete quantifier", save)
    if !featureSet.allowsBrokenCurlyAfterAlphabeticBackslash && isAfterAlphabeticBackslash(stack, save) then
      fail("Unescaped left brace", save)
    reset(save)
    stack.push(Node(NodeData.Literal('{'), SourceLocation(save, save + 1)))
    next()

  private def isAfterAlphabeticBackslash(stack: mutable.Stack[Node], pos: Int): Boolean =
    stack.headOption.exists: node =>
      node.data.isInstanceOf[NodeData.Backslash] && node.loc.end == pos && node.loc.start + 2 == pos &&
        CodePointUtil.isAlphabetic(source.charAt(pos - 1).toInt)

  private def parseGroup(stack: mutable.Stack[Node]): Unit =
    val start = offset
    next()
    (currentChar: @switch) match
      case '?' =>
        parseExtendedGroup(stack, start)
      case '*' =>
        parseAlphabeticGroup(stack, start)
      case _ =>
        parseGroupBody(stack, GroupKind.IndexedCapture, start)

  private def parseExtendedGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    (currentChar: @switch) match
      case ':' =>
        parseNonCaptureGroup(stack, start)
      case '|' =>
        parseBranchResetGroup(stack, start)
      case '=' =>
        parsePositiveLookAheadGroup(stack, start)
      case '!' =>
        parseNegativeLookAheadGroup(stack, start)
      case '<' =>
        parseAngleGroup(stack, start)
      case '>' =>
        parseAtomicGroup(stack, start)
      case '*' =>
        parseNonAtomicPositiveLookAheadGroup(stack, start)
      case '~' =>
        parseAbsenceGroup(stack, start)
      case '(' =>
        parseConditionalGroup(stack, start)
      case '\'' =>
        parseQuoteNamedCaptureGroup(stack, start)
      case 'P' =>
        parsePGroup(stack, start)
      case 'R' =>
        parseRGroup(stack, start)
      case '&' =>
        parseNamedCallGroup(stack, start)
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        parseIndexedCallGroup(stack, start)
      case '+' =>
        parsePlusGroup(stack, start)
      case '-' =>
        parseMinusGroup(stack, start)
      case '^' =>
        parseResetFlagGroup(stack, start)
      case '#' =>
        parseCommentGroup(stack, start)
      case '{' =>
        parseInlineCodeGroup(stack, start)
      case '?' =>
        parseEmbedCodeGroup(stack, start)
      case 'C' =>
        parseCalloutGroup(stack, start)
      case _ =>
        parseInlineFlagGroup(stack, start)

  private def parseNonCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    parseGroupBody(stack, GroupKind.NonCapture, start)

  private def parseBranchResetGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasBranchReset, "Invalid group")
    next()
    protectContext:
      val node = parseDisjunction()
      closeGroup()
      val nodes = node.data match
        case NodeData.Disjunction(nodes) => nodes
        case _                           => Seq(node)
      stack.push(Node(NodeData.Command(CommandKind.BranchReset(nodes)), SourceLocation(start, offset)))

  private def parsePositiveLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    parseGroupBody(stack, GroupKind.PositiveLookAhead(AssertNameStyle.Symbolic), start)

  private def parseNegativeLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    parseGroupBody(stack, GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), start)

  private def parseAngleGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    (currentChar: @switch) match
      case '=' =>
        parsePositiveLookBehindGroup(stack, start)
      case '!' =>
        parseNegativeLookBehindGroup(stack, start)
      case '*' =>
        parseNonAtomicPositiveLookBehindGroup(stack, start)
      case _ =>
        parseAngleNamedCaptureGroup(stack, start)

  private def parsePositiveLookBehindGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    parseGroupBody(stack, GroupKind.PositiveLookBehind(AssertNameStyle.Symbolic), start)

  private def parseNegativeLookBehindGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    parseGroupBody(stack, GroupKind.NegativeLookBehind(AssertNameStyle.Symbolic), start)

  private def parseNonAtomicPositiveLookBehindGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasNonAtomicLookAround, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.NonAtomicPositiveLookBehind(AssertNameStyle.Symbolic), start)

  private def parseAtomicGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasAtomicGroup, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.Atomic(AssertNameStyle.Symbolic), start)

  private def parseNonAtomicPositiveLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasNonAtomicLookAround, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.NonAtomicPositiveLookAhead(AssertNameStyle.Symbolic), start)

  private def parseAbsenceGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasAbsenceOperator, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.Absence, start)

  private val LookAroundTests: Seq[(String, String, String, AssertNameStyle => GroupKind.LookAround)] = Seq(
    ("?=", "*positive_lookahead:", "*pla:", GroupKind.PositiveLookAhead(_)),
    ("?!", "*negative_lookahead:", "*nla:", GroupKind.NegativeLookAhead(_)),
    ("?<=", "*positive_lookbehind:", "*plb:", GroupKind.PositiveLookBehind(_)),
    ("?<!", "*negative_lookbehind:", "*nlb:", GroupKind.NegativeLookBehind(_))
  )

  private def parseConditionalGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasConditional, "Invalid group")
    next()

    boundary:
      for n <- parseIntOption() do
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Indexed(n), start)
        boundary.break()

      if featureSet.hasNamedCaptureTest && currentChar == '<' then
        next()
        val name = parseName(end = '>')
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Angle, name), start)
        boundary.break()

      if featureSet.hasNamedCaptureTest && currentChar == '\'' then
        next()
        val name = parseName(end = '\'')
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Quote, name), start)
        boundary.break()

      if featureSet.hasDefineTest && startWith("DEFINE") then
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Define, start)
        boundary.break()

      if featureSet.hasVersionTest && startWith("VERSION") then
        val lt = !isEnd && currentChar == '>'
        if lt then next()
        expect('=', "Invalid condition")
        val major = parseIntOption().getOrElse(fail("Invalid condition"))
        expect('.', "Invalid condition")
        val minor = parseIntOption().getOrElse(fail("Invalid condition"))
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Version(lt, major, minor), start)
        boundary.break()

      if currentChar == 'R' && featureSet.hasRecursionTest then
        next()
        (currentChar: @switch) match
          case ')' =>
            closeCondition()
            parseConditionalGroupBody(stack, ConditionalTest.RRecursion, start)
          case '&' =>
            next()
            val name = parseID()
            closeCondition()
            parseConditionalGroupBody(stack, ConditionalTest.NamedRecursion(name), start)
          case _ =>
            val n = parseIntOption().getOrElse(fail("Invalid group"))
            closeCondition()
            parseConditionalGroupBody(stack, ConditionalTest.IndexedRecursion(n), start)
        boundary.break()

      if featureSet.hasBareNamedCaptureTest && isIDStart then
        val name = parseID()
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Bare, name), start)
        boundary.break()

      val isRelative = !isEnd && (currentChar == '+' || currentChar == '-')
      if featureSet.hasRelativeIndexedCaptureTest && isRelative then
        val minus = currentChar == '-'
        next()
        parseIntOption() match
          case Some(n) =>
            closeCondition()
            parseConditionalGroupBody(stack, ConditionalTest.Relative(if minus then -n else n), start)
            boundary.break()
          case None =>
            fail("Invalid condition")

      if featureSet.hasLookAroundTest then
        for (symbolic, alphabetic, abbrev, la) <- LookAroundTests do
          if parseLookAroundTest(stack, start, symbolic, alphabetic, abbrev, la) then boundary.break()

        parseLookAroundConditional(stack, None, start)
        boundary.break()

      fail("Invalid condition")

  private def closeCondition(): Unit =
    expect(')', "Invalid condition")

  private def parseConditionalGroupBody(stack: mutable.Stack[Node], test: ConditionalTest, start: Int): Unit =
    protectContext:
      val yes = protectContext(parseSequence())
      val no = Option.when(!isEnd && currentChar == '|'):
        next()
        protectContext(parseSequence())
      closeGroup()
      val end = offset
      stack.push(Node(NodeData.Command(CommandKind.Conditional(test, yes, no)), SourceLocation(start, end)))

  private def parseLookAroundTest(
      stack: mutable.Stack[Node],
      start: Int,
      symbolic: String,
      alphabetic: String,
      abbrev: String,
      la: AssertNameStyle => GroupKind.LookAround
  ): Boolean =
    if startWith(symbolic) then
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Symbolic)), start)
      return true

    if featureSet.hasAlphabeticGroup && startWith(alphabetic) then
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Alphabetic)), start)
      return true

    if featureSet.hasAlphabeticGroup && startWith(abbrev) then
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Abbrev)), start)
      return true

    false

  private def parseLookAroundConditional(
      stack: mutable.Stack[Node],
      kind: Option[GroupKind.LookAround],
      start: Int
  ): Unit =
    val node = protectContext(parseDisjunction())
    closeCondition()
    parseConditionalGroupBody(stack, ConditionalTest.LookAround(kind, node), start)

  private def parseAngleNamedCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasAngleNamedCapture, "Invalid group")
    parseNamedCaptureGroup(stack, '>', NameStyle.Angle, start)

  private def parseQuoteNamedCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasQuoteNamedCapture, "Invalid group")
    next()
    parseNamedCaptureGroup(stack, '\'', NameStyle.Quote, start)

  private def parseNamedCaptureGroup(stack: mutable.Stack[Node], end: Char, style: NameStyle, start: Int): Unit =
    if featureSet.hasBalanceGroup then
      parseBalanceGroup(stack, end, style, start)
      return

    val name = parseName(end = end)
    parseGroupBody(stack, GroupKind.NamedCapture(style, name), start)

  private def parseBalanceGroup(stack: mutable.Stack[Node], end: Char, style: NameStyle, start: Int): Unit =
    if currentChar == '-' then
      next()
      val test = parseName(end = end)
      parseGroupBody(stack, GroupKind.Balance(style, None, test), start)
      return

    val name = parseID()
    if currentChar == end then
      next()
      parseGroupBody(stack, GroupKind.NamedCapture(style, name), start)
      return

    if currentChar == '-' then
      next()
      val test = parseName(end = end)
      parseGroupBody(stack, GroupKind.Balance(style, Some(name), test), start)
      return

    fail("Invalid identifier")

  private def parsePGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasPGroup, "Invalid group")
    next()
    (currentChar: @switch) match
      case '<' =>
        next()
        val name = parseName(end = '>')
        parseGroupBody(stack, GroupKind.PNamedCapture(name), start)
      case '=' =>
        next()
        val name = parseID()
        pushCommand(stack, CommandKind.PBackReference(name), start)
      case '>' =>
        assert(featureSet.hasPCall, "Invalid group")
        next()
        val name = parseID()
        pushCommand(stack, CommandKind.PNamedCall(name), start)
      case _ =>
        fail("Invalid group")

  private def parseRGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasRCall, "Invalid group")
    next()
    pushCommand(stack, CommandKind.RCall, start)

  private def parseNamedCallGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasCallCommand, "Invalid group")
    next()
    val name = parseID()
    pushCommand(stack, CommandKind.NamedCall(name), start)

  private def parseIndexedCallGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasCallCommand, "Invalid group")
    val n = parseIntOption().getOrElse(fail("Invalid group"))
    pushCommand(stack, CommandKind.IndexedCall(n), start)

  private def parsePlusGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasCallCommand, "Invalid group")
    next()
    val n = parseIntOption().getOrElse(fail("Invalid group"))
    pushCommand(stack, CommandKind.RelativeCall(n), start)

  private def parseMinusGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    parseIntOption() match
      case Some(n) =>
        assert(featureSet.hasCallCommand, "Invalid group")
        pushCommand(stack, CommandKind.RelativeCall(-n), start)
      case None =>
        assert(featureSet.hasInlineFlag, "Invalid group")
        val added = FlagSet()
        val removed = parseFlagSet()
        val diff = FlagSetDiff(added, Some(removed))
        (currentChar: @switch) match
          case ')' =>
            pushCommand(stack, CommandKind.InlineFlag(FlagSetDiff(added, Some(removed))), start)
            applyFlagSetDiff(diff)
          case ':' =>
            next()
            applyFlagSetDiffWith(diff):
              parseGroupBody(stack, GroupKind.InlineFlag(FlagSetDiff(added, Some(removed))), start)
          case _ =>
            fail("Invalid group")

  private def parseResetFlagGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasResetFlag, "Invalid group")
    next()
    val flagSet = parseFlagSet()
    (currentChar: @switch) match
      case ')' =>
        pushCommand(stack, CommandKind.ResetFlag(flagSet), start)
        applyResetFlag(flagSet)
      case ':' =>
        next()
        applyResetFlagWith(flagSet):
          parseGroupBody(stack, GroupKind.ResetFlag(flagSet), start)
      case _ =>
        fail("Invalid group")

  private def parseCommentGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasInlineComment, "Invalid group")
    next()
    val textStart = offset
    while !isEnd && currentChar != ')' do
      if featureSet.processBackslashInCommentGroup && currentChar == '\\' then
        next()
        if !isEnd then next()
      else next()
    assert(!isEnd, "Invalid group")
    val end = offset
    val text = source.slice(textStart, end)
    pushCommand(stack, CommandKind.Comment(text), start)

  private def parseInlineCodeGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasInlineCode, "Invalid group")
    next()
    val code = parseCode(end = '}')
    pushCommand(stack, CommandKind.InlineCode(code), start)

  private def parseEmbedCodeGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasInlineCode, "Invalid group")
    next()
    expect('{', "Invalid group")
    val code = parseCode(end = '}')
    pushCommand(stack, CommandKind.EmbedCode(code), start)

  private def parseCalloutGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasCallout, "Invalid group")
    next()
    (currentChar: @switch) match
      case '`' | '\'' | '"' | '^' | '%' | '#' | '$' =>
        val delim = currentChar
        next()
        val value = parseCode(end = delim, escapeDouble = true)
        pushCommand(stack, CommandKind.CalloutString(delim, delim, value), start)
      case '{' =>
        next()
        val value = parseCode(end = '}', escapeDouble = true)
        pushCommand(stack, CommandKind.CalloutString('{', '}', value), start)
      case _ =>
        parseIntOption() match
          case Some(value) =>
            pushCommand(stack, CommandKind.CalloutInt(value), start)
          case None =>
            pushCommand(stack, CommandKind.Callout, start)

  private def parseCode(end: Char, escapeDouble: Boolean = false): String =
    val start = offset
    while !isEnd &&
      !(currentChar == end && !(escapeDouble && offset + 1 < source.length && source.charAt(offset + 1) == end))
    do
      if currentChar == end && escapeDouble && offset + 1 < source.length && source.charAt(offset + 1) == end then
        next()
      next()
    assert(!isEnd, "Invalid group")
    val code = source.slice(start, offset)
    next()
    code

  private def parseInlineFlagGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasInlineFlag, "Invalid group")

    val added = parseFlagSet()

    (currentChar: @switch) match
      case '-' =>
        next()
        val removed = parseFlagSet()
        val diff = FlagSetDiff(added, Some(removed))
        (currentChar: @switch) match
          case ':' =>
            next()
            applyFlagSetDiffWith(diff):
              parseGroupBody(stack, GroupKind.InlineFlag(diff), start)
          case ')' =>
            pushInlineFlagCommand(stack, diff, start)
          case _ =>
            fail("Invalid flag")
      case ':' =>
        next()
        val diff = FlagSetDiff(added, None)
        applyFlagSetDiffWith(diff):
          parseGroupBody(stack, GroupKind.InlineFlag(diff), start)
      case ')' =>
        val diff = FlagSetDiff(added, None)
        pushInlineFlagCommand(stack, diff, start)
      case _ =>
        fail("Invalid flag")

  private def pushInlineFlagCommand(stack: mutable.Stack[Node], diff: FlagSetDiff, start: Int): Unit =
    pushCommand(stack, CommandKind.InlineFlag(diff), start)
    if featureSet.hasIncomprehensiveInlineFlag then parseOpaqueDisjunction(stack, diff)
    else applyFlagSetDiff(diff)

  private def parseOpaqueDisjunction(stack: mutable.Stack[Node], diff: FlagSetDiff): Unit =
    boundary:
      applyFlagSetDiffWith(diff):
        val node = parseDisjunction()
        node.data match
          case s: NodeData.Sequence =>
            if s.nodes.isEmpty then boundary.break()
          case _ => ()
        stack.push(node)

  private val BacktrackControls = Seq(
    ("ACCEPT", BacktrackControlKind.Accept),
    ("FAIL", BacktrackControlKind.Fail),
    ("MARK", BacktrackControlKind.Mark),
    ("COMMIT", BacktrackControlKind.Commit),
    ("PRUNE", BacktrackControlKind.Prune),
    ("SKIP", BacktrackControlKind.Skip),
    ("THEN", BacktrackControlKind.Then)
  )

  private val AlphabeticGroups: Seq[(String, String, AssertNameStyle => GroupKind)] = Seq(
    ("positive_lookahead:", "pla:", GroupKind.PositiveLookAhead(_)),
    ("negative_lookahead:", "nla:", GroupKind.NegativeLookAhead(_)),
    ("positive_lookbehind:", "plb:", GroupKind.PositiveLookBehind(_)),
    ("negative_lookbehind:", "nlb:", GroupKind.NegativeLookBehind(_)),
    ("atomic:", "", GroupKind.Atomic(_)),
    ("script_run:", "sr:", GroupKind.ScriptRun(_)),
    ("atomic_script_run:", "asr:", GroupKind.AtomicScriptRun(_))
  ) ++ {
    if featureSet.hasNonAtomicLookAround then
      Seq(
        ("non_atomic_positive_lookahead:", "napla:", GroupKind.NonAtomicPositiveLookAhead(_)),
        ("non_atomic_positive_lookbehind:", "naplb:", GroupKind.NonAtomicPositiveLookBehind(_))
      )
    else Seq.empty
  }

  private def parseAlphabeticGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasAlphabeticGroup, "Invalid group")

    next()

    boundary:
      for (command, kind) <- BacktrackControls do
        if parseBacktrackControl(stack, start, command, kind) then boundary.break()

      for (alphabetic, abbrev, kind) <- AlphabeticGroups do
        if parseAlphabetic(stack, start, alphabetic, abbrev, kind) then boundary.break()

      expect(':', "Invalid group")
      val name = parseID()
      pushCommand(stack, CommandKind.BacktrackControl(None, Some(name)), start)

  private def parseBacktrackControl(
      stack: mutable.Stack[Node],
      start: Int,
      command: String,
      kind: BacktrackControlKind
  ): Boolean =
    if !startWith(command) then return false

    (currentChar: @switch) match
      case ':' =>
        next()
        val name = parseID()
        pushCommand(stack, CommandKind.BacktrackControl(Some(kind), Some(name)), start)
        true
      case ')' =>
        pushCommand(stack, CommandKind.BacktrackControl(Some(kind), None), start)
        true
      case _ =>
        fail("Invalid group")

  private def parseAlphabetic(
      stack: mutable.Stack[Node],
      start: Int,
      alphabetic: String,
      abbrev: String,
      k: AssertNameStyle => GroupKind
  ): Boolean =
    if startWith(alphabetic) then
      parseGroupBody(stack, k(AssertNameStyle.Alphabetic), start)
      return true

    if abbrev.nonEmpty && startWith(abbrev) then
      parseGroupBody(stack, k(AssertNameStyle.Abbrev), start)
      return true

    false

  private def parseGroupBody(stack: mutable.Stack[Node], kind: GroupKind, start: Int): Unit =
    val node = protectContext(parseDisjunction())
    closeGroup()
    val end = offset
    stack.push(Node(NodeData.Group(kind, node), SourceLocation(start, end)))

  private def pushCommand(stack: mutable.Stack[Node], kind: CommandKind, start: Int): Unit =
    closeGroup()
    val end = offset
    stack.push(Node(NodeData.Command(kind), SourceLocation(start, end)))

  private def closeGroup(): Unit =
    expect(')', "Unclosed group")

  private def parseFlagSet(): FlagSet =
    var flagSet = FlagSet()
    while !isEnd do
      (currentChar: @switch) match
        case 'i' =>
          flagSet = flagSet.copy(ignoreCase = true)
        case 'm' =>
          flagSet = flagSet.copy(multiline = true)
        case 'x' =>
          flagSet = flagSet.copy(verbose = true)
        case 'J' =>
          if !featureSet.hasFlagUpperJ then return flagSet
          flagSet = flagSet.copy(dupNames = true)
        case 'L' =>
          if !featureSet.hasFlagUpperL then return flagSet
          flagSet = flagSet.copy(localeUpper = true)
        case 'U' =>
          if !featureSet.hasFlagUpperU then return flagSet
          flagSet = flagSet.copy(ungreedy = true)
        case 'a' =>
          if !featureSet.hasFlagA then return flagSet
          flagSet = flagSet.copy(ascii = true)
        case 'd' =>
          if !featureSet.hasFlagD then return flagSet
          flagSet = flagSet.copy(hasIndices = true)
        case 'l' =>
          if !featureSet.hasFlagL then return flagSet
          flagSet = flagSet.copy(localeLower = true)
        case 'n' =>
          if !featureSet.hasFlagN then return flagSet
          flagSet = flagSet.copy(explicitCapture = true)
        case 'p' =>
          if !featureSet.hasFlagP then return flagSet
          flagSet = flagSet.copy(preserve = true)
        case 's' =>
          if !featureSet.hasFlagS then return flagSet
          flagSet = flagSet.copy(dotAll = true)
        case 'u' =>
          if !featureSet.hasFlagU then return flagSet
          flagSet = flagSet.copy(unicode = true)
        case _ =>
          return flagSet
      next()
    flagSet

  private def parseCaret(stack: mutable.Stack[Node]): Unit =
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Caret, SourceLocation(start, end)))

  private def parseDollar(stack: mutable.Stack[Node]): Unit =
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Dollar, SourceLocation(start, end)))

  private def parseDot(stack: mutable.Stack[Node]): Unit =
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Dot, SourceLocation(start, end)))

  private def parseBackslash(stack: mutable.Stack[Node]): Unit =
    val start = offset
    next()
    val backslash = parseBackslashKind()
    val end = offset
    stack.push(Node(NodeData.Backslash(backslash), SourceLocation(start, end)))

    for value <- pendingCodePoints do
      stack.push(
        Node(NodeData.Backslash(BackslashKind.Escape(EscapeStyle.UnicodeBracket, value)), SourceLocation(start, end))
      )
    pendingCodePoints = Nil

    if backslash != BackslashKind.CaseCommand(CaseCommandKind.QuoteCommand) then return

    while !isEnd && !source.startsWith("\\E", offset) do parseLiteral(stack)

    val eStart = offset
    if startWith("\\E") then
      val backslash = NodeData.Backslash(BackslashKind.CaseCommand(CaseCommandKind.EndCaseCommand))
      val eEnd = offset
      stack.push(Node(backslash, SourceLocation(eStart, eEnd)))

  private def parseBackslashKind(): BackslashKind =
    (currentChar: @switch) match
      case 'a'                                                 => parseBackslashLowerA()
      case 'b'                                                 => parseBackslashLowerB()
      case 'c'                                                 => parseBackslashLowerC()
      case 'd'                                                 => parseBackslashLowerD()
      case 'e'                                                 => parseBackslashLowerE()
      case 'f'                                                 => parseBackslashLowerF()
      case 'g'                                                 => parseBackslashLowerG()
      case 'h'                                                 => parseBackslashLowerH()
      case 'k'                                                 => parseBackslashLowerK()
      case 'l'                                                 => parseBackslashLowerL()
      case 'n'                                                 => parseBackslashLowerN()
      case 'o'                                                 => parseBackslashLowerO()
      case 'p'                                                 => parseBackslashLowerP()
      case 'q'                                                 => parseBackslashLowerQ()
      case 'r'                                                 => parseBackslashLowerR()
      case 's'                                                 => parseBackslashLowerS()
      case 't'                                                 => parseBackslashLowerT()
      case 'u'                                                 => parseBackslashLowerU()
      case 'v'                                                 => parseBackslashLowerV()
      case 'w'                                                 => parseBackslashLowerW()
      case 'x'                                                 => parseBackslashLowerX()
      case 'z'                                                 => parseBackslashLowerZ()
      case 'A'                                                 => parseBackslashUpperA()
      case 'B'                                                 => parseBackslashUpperB()
      case 'C'                                                 => parseBackslashUpperC()
      case 'D'                                                 => parseBackslashUpperD()
      case 'E'                                                 => parseBackslashUpperE()
      case 'F'                                                 => parseBackslashUpperF()
      case 'G'                                                 => parseBackslashUpperG()
      case 'H'                                                 => parseBackslashUpperH()
      case 'K'                                                 => parseBackslashUpperK()
      case 'L'                                                 => parseBackslashUpperL()
      case 'M'                                                 => parseBackslashUpperM()
      case 'N'                                                 => parseBackslashUpperN()
      case 'P'                                                 => parseBackslashUpperP()
      case 'Q'                                                 => parseBackslashUpperQ()
      case 'R'                                                 => parseBackslashUpperR()
      case 'S'                                                 => parseBackslashUpperS()
      case 'U'                                                 => parseBackslashUpperU()
      case 'V'                                                 => parseBackslashUpperV()
      case 'W'                                                 => parseBackslashUpperW()
      case 'X'                                                 => parseBackslashUpperX()
      case 'Z'                                                 => parseBackslashUpperZ()
      case '0'                                                 => parseBackslashOctal()
      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        parseBackslashBackReference()
      case '/' | '|' | '+' | '*' | '?' | '{' | '}' | '(' | ')' | '^' | '$' | '.' | '\\' | '[' | ']' | '&' =>
        parseBackslashEscape()
      case _ => parseBackslashUnknown()

  private def parseBackslashLowerA(): BackslashKind =
    if !featureSet.hasBackslashBell then return parseBackslashUnknown()

    next()
    BackslashKind.Escape(EscapeStyle.Single('a'), 0x0007)

  private def parseBackslashLowerB(): BackslashKind =
    if context.inClass then
      assert(featureSet.hasBackslashBackspace, "Invalid backslash escape")
      return parseBackslashBackslash()

    parseBackslashBoundary()

  private def parseBackslashBackslash(): BackslashKind =
    next()
    BackslashKind.Escape(EscapeStyle.Single('b'), 0x0008)

  private val BoundaryModifiers = Seq(
    ("{gcb}", BoundaryModifier.GcbModifier),
    ("{lb}", BoundaryModifier.LbModifier),
    ("{sb}", BoundaryModifier.SbModifier),
    ("{wb}", BoundaryModifier.WbModifier)
  )

  private def parseBackslashBoundary(): BackslashKind =
    next()

    boundary:
      if featureSet.hasBoundaryGModifier && startWith("{g}") then
        boundary.break(BackslashKind.Assert(AssertKind.Boundary(Some(BoundaryModifier.GModifier))))

      if featureSet.hasBoundaryModifier then
        for (name, modifier) <- BoundaryModifiers do
          if startWith(name) then boundary.break(BackslashKind.Assert(AssertKind.Boundary(Some(modifier))))

      BackslashKind.Assert(AssertKind.Boundary(None))

  private def parseBackslashLowerC(): BackslashKind =
    if !featureSet.hasBackslashControl then return parseBackslashUnknown()

    next()
    if featureSet.hasBackslashMetaControl && !isEnd && currentChar == '\\' then
      val value = parseMetaControlTarget()
      return BackslashKind.Escape(EscapeStyle.Control('\\'), metaControlValue(value))

    if !isControl then
      assert(featureSet.allowsBrokenBackslashControl, "Invalid backslash escape")
      return BackslashKind.Unknown('c')

    val char = currentChar
    next()

    val value =
      if featureSet.hasBackslashControlAny then char ^ 0x40
      else if featureSet.hasBackslashMetaControl then metaControlValue(char)
      else if CodePointUtil.isAlphabetic(char) || featureSet.hasBackslashControlDigitInClass then char % 32
      else char ^ 0x40
    BackslashKind.Escape(EscapeStyle.Control(char), value)

  private def metaControlValue(value: Int): Int =
    if value == '?' then 0x7f else value & 0x9f

  private def parseMetaControlTarget(): Int =
    assert(!isEnd, "Invalid backslash escape")
    if currentChar == '\\' then
      val save = offset
      next()
      parseBackslashKind() match
        case kind: BackslashKind.BackslashValue => kind.value
        case _                                  => fail("Invalid backslash escape", save)
    else
      assert(currentChar < 0x80, "Invalid backslash escape")
      val char = currentChar
      next()
      char.toInt

  private def parseBackslashLowerD(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.Digit)

  private def parseBackslashLowerE(): BackslashKind =
    if !featureSet.hasBackslashEscape then return parseBackslashUnknown()

    next()
    BackslashKind.Escape(EscapeStyle.Single('e'), 0x1b)

  private def parseBackslashLowerF(): BackslashKind =
    next()
    BackslashKind.Escape(EscapeStyle.Single('f'), 0x0c)

  private def parseBackslashLowerG(): BackslashKind =
    if !(featureSet.hasBackslashGBackReference || featureSet.hasBackslashGCall) then return parseBackslashUnknown()

    next()
    (currentChar: @switch) match
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        assert(featureSet.hasBackslashGBackReference, "Invalid backslash escape")
        val index = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        BackslashKind.EscapeBackReference(
          BackReferenceStyle.GBackReference(NameStyle.Bare),
          Reference.IndexedReference(index)
        )
      case '+' | '-' =>
        assert(featureSet.hasBackslashGBackReference, "Invalid backslash escape")
        val sign = if currentChar == '-' then -1 else 1
        next()
        val n = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        BackslashKind.EscapeBackReference(
          BackReferenceStyle.GBackReference(NameStyle.Bare),
          Reference.RelativeReference(sign * n)
        )
      case '{' =>
        assert(featureSet.hasBackslashGBackReference, "Invalid backslash escape")
        next()
        val ref = parseBaseRef()
        expect('}', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.GBackReference(NameStyle.Curly), ref)
      case '<' | '\'' =>
        assert(featureSet.hasBackslashGCall, "Invalid backslash escape")
        val style = if currentChar == '<' then NameStyle.Angle else NameStyle.Quote
        next()
        val ref = parseBaseRef()
        expect(if style == NameStyle.Angle then '>' else '\'', "Invalid backslash escape")
        BackslashKind.EscapeCall(style, ref)
      case _ =>
        fail("Invalid backslash escape")

  private def parseBackslashLowerH(): BackslashKind =
    if !(featureSet.hasBackslashHorizontalSpace || featureSet.hasBackslashHexDigit) then return parseBackslashUnknown()

    next()
    val klass = if featureSet.hasBackslashHorizontalSpace then EscapeClassKind.Horizontal else EscapeClassKind.HexDigit
    BackslashKind.EscapeClass(klass)

  private def parseBackslashLowerK(): BackslashKind =
    if !featureSet.hasBackslashKBackReference || context.inClass then return parseBackslashUnknown()

    next()
    (currentChar: @switch) match
      case '<' =>
        next()
        val ref = parseRef()
        expect('>', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.KBackReference(NameStyle.Angle), ref)
      case '\'' =>
        assert(featureSet.hasBackslashKBackReference, "Invalid backslash escape")
        next()
        val ref = parseRef()
        expect('\'', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.KBackReference(NameStyle.Quote), ref)
      case _ =>
        fail("Invalid backslash escape")

  private def parseRef(): Reference =
    val ref = parseBaseRef()

    if !featureSet.hasLeveledBackReference then return ref

    (currentChar: @switch) match
      case '+' | '-' =>
        val sign = if currentChar == '-' then -1 else 1
        next()
        val level = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        Reference.LeveledReference(ref, sign * level)
      case _ => ref

  private def parseBaseRef(): Reference.BaseReference =
    if !featureSet.allowsNonNameBaseRef then
      val name = parseID()
      return Reference.NamedReference(name)

    (currentChar: @switch) match
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        val index = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        Reference.IndexedReference(index)
      case '+' | '-' =>
        val sign = if currentChar == '-' then -1 else 1
        next()
        val n = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        Reference.RelativeReference(sign * n)
      case _ =>
        val name = parseID()
        Reference.NamedReference(name)

  private def parseBackslashLowerL(): BackslashKind =
    if !featureSet.hasBackslashCaseCommand then return parseBackslashUnknown()

    next()
    BackslashKind.CaseCommand(CaseCommandKind.SingleLowerCaseCommand)

  private def parseBackslashLowerN(): BackslashKind =
    next()
    BackslashKind.Escape(EscapeStyle.Single('n'), 0x0a)

  private def parseBackslashLowerO(): BackslashKind =
    if !featureSet.hasBackslashOctal then return parseBackslashUnknown()

    next()
    expect('{', "Invalid backslash escape")
    val value = parseBracedCodePoint(8)
    BackslashKind.Escape(EscapeStyle.Octal, value)
  private def parseBackslashLowerP(): BackslashKind = parseBackslashProperty(invert = false)
  private def parseBackslashProperty(invert: Boolean): BackslashKind =
    if !featureSet.hasBackslashUnicodeProperty then return parseBackslashUnknown()

    if source.lift(offset + 1).forall(_ != '{') && !featureSet.hasBackslashUnicodePropertyBare &&
      featureSet.allowsAlphabeticUnknownBackslash
    then return parseBackslashUnknown()

    next()
    if isEnd || currentChar != '{' then
      assert(featureSet.hasBackslashUnicodePropertyBare, "Invalid backslash escape")
      assert(!isEnd, "Invalid backslash escape")
      val name = currentChar.toString
      next()
      val klass =
        if invert then EscapeClassKind.NonUnicodeBareProperty(name) else EscapeClassKind.UnicodeBareProperty(name)
      return BackslashKind.EscapeClass(klass)

    next()
    val (negated, name, value) =
      if featureSet.allowsLoosePropertyName then parseLooseProperty() else parseStrictProperty()
    expect('}', "Invalid backslash escape")

    val klass = (invert != negated, value) match
      case (false, None)        => EscapeClassKind.UnicodeProperty(name)
      case (false, Some(value)) => EscapeClassKind.UnicodePropertyValue(name, value)
      case (true, None)         => EscapeClassKind.NonUnicodeProperty(name)
      case (true, Some(value))  => EscapeClassKind.NonUnicodePropertyValue(name, value)
    BackslashKind.EscapeClass(klass)

  private def parseStrictProperty(): (Boolean, String, Option[String]) =
    val name = parsePropertyName()
    val value = Option.when(!isEnd && currentChar == '='):
      next()
      parsePropertyName()
    (false, name, value)

  private def parsePropertyName(): String =
    if !featureSet.allowsRawPropertyName then return parseID()
    val start = offset
    while !isEnd && currentChar != '}' && currentChar != '=' do next()
    assert(start < offset, "Invalid backslash escape")
    source.slice(start, offset)

  // Perl, PCRE and Ruby ignore blanks, hyphens, underscores and cases in property names, so the name is kept as written.
  private def parseLooseProperty(): (Boolean, String, Option[String]) =
    skipBraceBlank(force = true)
    val negated = !isEnd && currentChar == '^'
    if negated then next()
    val start = offset
    while !isEnd && currentChar != '}' do next()
    val body = source.slice(start, offset).trim
    val separator = body.indexWhere(c => c == '=' || c == ':')
    val (name, value) =
      if separator < 0 then (body, None)
      else (body.slice(0, separator).trim, Some(body.slice(separator + 1, body.length).trim))
    assert(name.nonEmpty && value.forall(_.nonEmpty), "Invalid backslash escape")
    (negated, name, value)

  private def parseBackslashLowerQ(): BackslashKind =
    if !(featureSet.hasBackslashQuoteSet && context.inClass) then return parseBackslashUnknown()

    next()
    expect('{', "Invalid backslash escape")
    val strings = Seq.newBuilder[Seq[QuoteLiteral]]
    strings.addOne(parseQuoteString())
    while !isEnd && currentChar == '|' do
      next()
      strings.addOne(parseQuoteString())
    expect('}', "Invalid backslash escape")

    BackslashKind.EscapeClass(EscapeClassKind.QuoteSet(strings.result()))

  private def parseQuoteString(): Seq[QuoteLiteral] =
    val string = Seq.newBuilder[QuoteLiteral]

    while !(isEnd || currentChar == '|' || currentChar == '}') do
      if currentChar == '\\' then
        val save = offset
        next()
        val backslash = parseBackslashKind()
        backslash match
          case kind: BackslashKind.BackslashValue =>
            string.addOne(QuoteLiteral.QuoteBackslash(kind))
          case _ =>
            fail("Invalid backslash escape", save)
      else
        val value = if featureSet.readsAsUnicode then currentCodePoint else currentChar.toInt
        nextCodePoint(value)
        string.addOne(QuoteLiteral.QuoteValue(value))

    string.result()

  private def parseBackslashLowerR(): BackslashKind =
    next()
    BackslashKind.Escape(EscapeStyle.Single('r'), 0x0d)

  private def parseBackslashLowerS(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.Space)

  private def parseBackslashLowerT(): BackslashKind =
    next()
    BackslashKind.Escape(EscapeStyle.Single('t'), 0x09)

  private def parseBackslashLowerU(): BackslashKind =
    if !(featureSet.hasBackslashUnicodeHex || featureSet.hasBackslashCaseCommand) then return parseBackslashUnknown()

    val save = offset
    next()

    if featureSet.hasBackslashUnicodeBracket && currentChar == '{' then
      next()
      if featureSet.hasBackslashUnicodeList then return parseUnicodeList()
      val value = parseBracedCodePoint(16)
      return BackslashKind.Escape(EscapeStyle.UnicodeBracket, value)

    if featureSet.hasBackslashCaseCommand then return BackslashKind.CaseCommand(CaseCommandKind.SingleUpperCaseCommand)

    if !source.slice(offset, offset + 4).forall(CodePointUtil.isHexDigit(_)) || offset + 4 > source.length then
      assert(featureSet.allowsBrokenBackslashHex, "Invalid backslash escape")
      reset(save)
      return parseBackslashUnknown()

    val value = checkCodePoint(parseHexDigits(4), save)

    val isHighSurrogate = CodePointUtil.isHighSurrogate(value)
    if featureSet.combinesSurrogatePairEscape && isHighSurrogate && source.startsWith("\\u", offset) then
      val low = source.slice(offset + 2, offset + 6)
      if low.length == 4 && low.forall(CodePointUtil.isHexDigit(_)) then
        val lowValue = Integer.parseInt(low, 16)
        if CodePointUtil.isLowSurrogate(lowValue) then
          reset(offset + 6)
          val pairValue = CodePointUtil.decodeSurrogatePair(value, lowValue)
          return BackslashKind.Escape(EscapeStyle.UnicodeHex4Pair, pairValue)

    BackslashKind.Escape(EscapeStyle.UnicodeHex4, value)

  private def parseUnicodeList(): BackslashKind =
    skipBraceBlank(force = true)
    val values = Seq.newBuilder[Int]
    while !isEnd && isHexDigit do
      val start = offset
      values.addOne(checkCodePoint(parseHexDigits(Int.MaxValue), start))
      skipBraceBlank(force = true)
    expect('}', "Invalid backslash escape")
    values.result() match
      case Seq()         => fail("Invalid backslash escape")
      case value +: rest =>
        pendingCodePoints = rest.toList
        BackslashKind.Escape(EscapeStyle.UnicodeBracket, value)
  private def parseBackslashLowerV(): BackslashKind =
    if !(featureSet.hasBackslashVerticalSpace || featureSet.hasBackslashVerticalTab) then return parseBackslashUnknown()

    next()

    if featureSet.hasBackslashVerticalSpace then return BackslashKind.EscapeClass(EscapeClassKind.Vertical)

    BackslashKind.Escape(EscapeStyle.Single('v'), 0x0b)

  private def parseBackslashLowerW(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.Word)

  private def parseBackslashLowerX(): BackslashKind =
    val save = offset
    next()

    if featureSet.hasBackslashXBracket && currentChar == '{' then
      next()
      val value = parseBracedCodePoint(16)
      return BackslashKind.Escape(EscapeStyle.HexBracket, value)

    if !isHexDigit then
      if featureSet.allowsBackslashXWithoutDigit then return BackslashKind.Escape(EscapeStyle.Hex0, 0x00)
      assert(featureSet.allowsBrokenBackslashHex, "Invalid backslash escape")
      reset(save)
      return parseBackslashUnknown()

    val isHex2 = offset + 1 < source.length && CodePointUtil.isHexDigit(source.charAt(offset + 1).toInt)
    if isHex2 then return BackslashKind.Escape(EscapeStyle.Hex2, parseHexDigits(2).toInt)

    if featureSet.hasBackslashXHex1 then return BackslashKind.Escape(EscapeStyle.Hex1, parseHexDigits(1).toInt)

    assert(featureSet.allowsBrokenBackslashHex, "Invalid backslash escape")
    reset(save)
    parseBackslashUnknown()
  private def parseBackslashLowerZ(): BackslashKind =
    if !featureSet.hasBackslashEnd then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.LowerEnd)

  private def parseBackslashUpperA(): BackslashKind =
    if !featureSet.hasBackslashBegin then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.Begin)

  private def parseBackslashUpperB(): BackslashKind =
    if context.inClass then return parseBackslashUnknown()

    next()

    boundary:
      if featureSet.hasBoundaryModifier then
        if startWith("{g}") then
          boundary.break(BackslashKind.Assert(AssertKind.NonBoundary(Some(BoundaryModifier.GModifier))))

        for (name, modifier) <- BoundaryModifiers do
          if startWith(name) then boundary.break(BackslashKind.Assert(AssertKind.NonBoundary(Some(modifier))))

      BackslashKind.Assert(AssertKind.NonBoundary(None))

  private def parseBackslashUpperC(): BackslashKind =
    if featureSet.hasBackslashCodeUnit && !context.inClass then
      next()
      return BackslashKind.EscapeClass(EscapeClassKind.CodeUnit)

    if !featureSet.hasBackslashMetaControl then
      assert(featureSet.allowsUnknownBackslashUpperC, "Invalid backslash escape")
      return parseBackslashUnknown()

    next()
    expect('-', "Invalid backslash escape")
    val value = parseMetaControlTarget()
    BackslashKind.Escape(EscapeStyle.ControlDash, metaControlValue(value))

  private def parseBackslashUpperD(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonDigit)

  private def parseBackslashUpperE(): BackslashKind =
    if !(featureSet.hasBackslashCaseCommand || featureSet.hasBackslashQuoteCommand) then return parseBackslashUnknown()
    assert(featureSet.allowsUnpairedQuoteEnd, "Invalid backslash escape")

    next()
    BackslashKind.CaseCommand(CaseCommandKind.EndCaseCommand)

  private def parseBackslashUpperF(): BackslashKind =
    if !featureSet.hasBackslashCaseCommand then return parseBackslashUnknown()

    next()
    BackslashKind.CaseCommand(CaseCommandKind.FoldCaseCommand)

  private def parseBackslashUpperG(): BackslashKind =
    if !featureSet.hasBackslashStickyAssert then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.Sticky)

  private def parseBackslashUpperH(): BackslashKind =
    if !(featureSet.hasBackslashHorizontalSpace || featureSet.hasBackslashHexDigit) then return parseBackslashUnknown()

    next()
    val klass =
      if featureSet.hasBackslashHorizontalSpace then EscapeClassKind.NonHorizontal else EscapeClassKind.NonHexDigit
    BackslashKind.EscapeClass(klass)

  private def parseBackslashUpperK(): BackslashKind =
    if !featureSet.hasBackslashCut then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.Cut)

  private def parseBackslashUpperL(): BackslashKind =
    if !featureSet.hasBackslashCaseCommand then return parseBackslashUnknown()

    next()
    BackslashKind.CaseCommand(CaseCommandKind.LowerCaseCommand)

  private def parseBackslashUpperM(): BackslashKind =
    if !featureSet.hasBackslashMetaControl then return parseBackslashUnknown()

    next()
    expect('-', "Invalid backslash escape")
    val value = parseMetaControlTarget()
    BackslashKind.Escape(EscapeStyle.Meta, value | 0x80)

  private def parseBackslashUpperN(): BackslashKind =
    if !(featureSet.hasBackslashNonNewline || featureSet.hasBackslashCharName || featureSet.hasBackslashCharNameHex)
    then return parseBackslashUnknown()

    next()

    val isCharName = !isEnd && currentChar == '{' && !(featureSet.hasBackslashNonNewline && isCurlyQuantifierAhead)
    if !isCharName then
      assert(featureSet.hasBackslashNonNewline, "Invalid backslash escape")
      return BackslashKind.EscapeClass(EscapeClassKind.NonNewline)

    next()
    if featureSet.hasBackslashCharNameHex && startWith("U+") then
      val start = offset
      assert(isHexDigit, "Invalid backslash escape")
      val value = checkCodePoint(parseHexDigits(Int.MaxValue), start)
      expect('}', "Invalid backslash escape")
      return BackslashKind.Escape(EscapeStyle.NamedHex, value)

    assert(featureSet.hasBackslashCharName, "Invalid backslash escape")
    val start = offset
    while !isEnd && currentChar != '}' do next()
    val name = source.slice(start, offset)
    assert(name.nonEmpty, "Invalid backslash escape")
    expect('}', "Invalid backslash escape")
    BackslashKind.EscapeClass(EscapeClassKind.CharName(name))

  private def isCurlyQuantifierAhead: Boolean =
    val end = source.indexOf('}', offset)
    end > 0 && {
      val body = source.slice(offset + 1, end)
      body.exists(_.isDigit) && body.forall(c => c.isDigit || c == ',' || c == ' ' || c == '\t')
    }

  private def parseBackslashUpperP(): BackslashKind = parseBackslashProperty(invert = true)
  private def parseBackslashUpperQ(): BackslashKind =
    if !featureSet.hasBackslashQuoteCommand then return parseBackslashUnknown()

    next()
    BackslashKind.CaseCommand(CaseCommandKind.QuoteCommand)

  private def parseBackslashUpperR(): BackslashKind =
    if !featureSet.hasBackslashGeneralNewline then return parseBackslashUnknown()

    next()
    BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline)

  private def parseBackslashUpperS(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonSpace)

  private def parseBackslashUpperU(): BackslashKind =
    if !(featureSet.hasBackslashUnicodeHex8 || featureSet.hasBackslashCaseCommand) then return parseBackslashUnknown()

    val save = offset
    next()

    if featureSet.hasBackslashCaseCommand then return BackslashKind.CaseCommand(CaseCommandKind.UpperCaseCommand)

    for i <- 0 until 8 do
      assert(
        offset + i < source.length && CodePointUtil.isHexDigit(source.charAt(offset + i).toInt),
        "Invalid backslash escape"
      )

    BackslashKind.Escape(EscapeStyle.UnicodeHex8, checkCodePoint(parseHexDigits(8), save))
  private def parseBackslashUpperV(): BackslashKind =
    if !featureSet.hasBackslashVerticalSpace then return parseBackslashUnknown()

    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonVertical)

  private def parseBackslashUpperW(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonWord)

  private def parseBackslashUpperX(): BackslashKind =
    if !featureSet.hasBackslashGraphemeCluster then return parseBackslashUnknown()

    next()
    BackslashKind.EscapeClass(EscapeClassKind.GraphemeCluster)

  private def parseBackslashUpperZ(): BackslashKind =
    if !featureSet.hasBackslashUpperEnd then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.UpperEnd)

  private def parseBackslashOctal(): BackslashKind =
    if !featureSet.hasBackslashBareOctal then
      if featureSet.hasBackslashZeroOctal && currentChar == '0' then return parseBackslashZeroOctal()
      if featureSet.hasBackslashNull && currentChar == '0' && !source.lift(offset + 1).exists(_.isDigit) then
        next()
        return BackslashKind.Escape(EscapeStyle.Single('0'), 0x00)
      fail("Invalid backslash escape")

    if !isOctalDigit then
      assert(featureSet.allowsNonOctalDigitBackslash, "Invalid backslash escape")
      return parseBackslashUnknown()

    val size = if currentChar <= '3' || !featureSet.limitsOctalTo0377ByDigits then 3 else 2
    val start = offset
    val value = parseOctalDigits(size)
    if value > 0xff && !featureSet.allowsOctalAbove0377 then fail("Octal escape value too large", start)
    BackslashKind.Escape(EscapeStyle.BareOctal, value)

  private def parseBackslashZeroOctal(): BackslashKind =
    next()
    assert(isOctalDigit, "Invalid octal escape")
    val size = if currentChar <= '3' then 3 else 2
    BackslashKind.Escape(EscapeStyle.BareOctal, parseOctalDigits(size))

  private def parseOctalDigits(size: Int): Int =
    var value = 0
    for _ <- 1 to size; if isOctalDigit do
      value = value * 8 + (currentChar - '0')
      next()
    value

  private def parseBackslashBackReference(): BackslashKind =
    if context.inClass then return parseBackslashOctal()

    val save = offset

    if featureSet.readsThreeOctalDigitsAsOctal && source
        .slice(offset, offset + 3)
        .count(CodePointUtil.isOctalDigit(_)) == 3
    then return parseBackslashOctal()

    val n =
      if featureSet.readsThreeOctalDigitsAsOctal then parseBoundedDigits(_ < 100)
      else if featureSet.readsBackReferenceDigitsWhileGroupExists then parseBoundedDigits(_ <= analysis.captureSize)
      else parseSaturatedIntOption(Long.MaxValue, "Invalid backslash escape", save).get

    if featureSet.checksValidBackReference && !(0 < n && n <= analysis.captureSize) then
      reset(save)
      if isOctalDigit then return parseBackslashOctal()

      return parseBackslashUnknown()

    val isOctalStart = CodePointUtil.isOctalDigit(source.charAt(save).toInt)
    if featureSet.fallsBackMultiDigitBackReferenceToOctal && n >= 10 && n > analysis.captureSize && isOctalStart then
      reset(save)
      return parseBackslashOctal()

    if featureSet.rejectsInvalidBackReference && n > analysis.captureSize then fail("Invalid back-reference", save)

    BackslashKind.EscapeBackReference(BackReferenceStyle.BareBackReference, Reference.IndexedReference(n))

  private def parseBoundedDigits(continues: Int => Boolean): Int =
    var value = currentChar - '0'
    next()
    while isDigit && continues(value * 10 + (currentChar - '0')) do
      value = value * 10 + (currentChar - '0')
      next()
    value

  private def parseBackslashEscape(): BackslashKind =
    val value = currentChar
    if value == '&' then assertSymbolUnknownBackslash()
    next()
    BackslashKind.Escape(EscapeStyle.Single(value), value.toInt)

  private def parseBackslashUnknown(): BackslashKind =
    assert(!isEnd, "Invalid backslash escape")
    val value = currentCodePoint

    if isAlphabetic then assert(featureSet.allowsAlphabeticUnknownBackslash, "Invalid backslash escape")
    else if CodePointUtil.isWord(value) then assert(featureSet.allowsWordUnknownBackslash, "Invalid backslash escape")
    else assertSymbolUnknownBackslash()

    nextCodePoint(value)
    BackslashKind.Unknown(value)

  private def assertSymbolUnknownBackslash(): Unit =
    if featureSet.allowsSymbolUnknownBackslash then return
    if context.inClass && currentChar == '-' then return
    if featureSet.hasBackslashClassSetReservedPunctuator && context.inClass && "&-!#%,:;<=>@`~".contains(currentChar)
    then return
    fail("Invalid backslash escape")

  private def parseClass(stack: mutable.Stack[Node]): Unit = protectContext:
    context = context.copy(inClass = true)

    val start = offset

    next()
    val invert = currentChar == '^'
    if invert then next()

    val item = parseClassItem(!featureSet.allowsClassEmpty)
    expect(']', "Unclosed ']'")

    val end = offset
    stack.push(Node(NodeData.Class(invert, item), SourceLocation(start, end)))

  private def parseClassItem(first: Boolean): ClassItem =
    val start = offset

    var item = parseClassItemUnion(first)

    var continue = true
    while continue do
      continue = false

      if featureSet.allowsClassIntersection && startWith("&&") then
        continue = true
        val right = parseClassItemUnion(false)
        val end = offset
        item = ClassItem(ClassItemData.ClassIntersection(item, right), SourceLocation(start, end))

      if featureSet.allowsClassDiff && startWith("--") then
        continue = true
        val right = parseClassItemUnion(false)
        val end = offset
        item = ClassItem(ClassItemData.ClassDiff(item, right), SourceLocation(start, end))

    item

  private def parseClassItemUnion(first: Boolean): ClassItem =
    val start = offset
    val seq = Seq.newBuilder[ClassItem]

    while (first && start == offset && !isEnd) || !isClassItemStop do seq.addOne(parseClassItemAtom())

    val end = offset
    val items = seq.result()
    if items.size == 1 then return items.head
    ClassItem(ClassItemData.ClassUnion(items), SourceLocation(start, end))

  private def isClassItemStop: Boolean =
    isEnd ||
      currentChar == ']' ||
      featureSet.allowsClassIntersection && source.startsWith("&&", offset) ||
      featureSet.allowsClassDiff && source.startsWith("--", offset)

  private def parseClassItemAtom(): ClassItem =
    val start = offset
    val begin = parseClassItemNonRange()

    if !isClassItemStop && currentChar == '-' && isValueClassItem(begin) then
      val save = offset
      next()

      if isClassItemStop then
        reset(save)
        return begin

      if (featureSet.allowsClassPosix || featureSet.allowsClassNest) && currentChar == '[' then
        reset(save)
        return begin

      val end = parseClassItemNonRange()
      val endOffset = offset
      if !isValueClassItem(end) then
        reset(save)
        return begin

      return ClassItem(
        ClassItemData.ClassRange(begin.asInstanceOf[ClassItem.Value], end.asInstanceOf[ClassItem.Value]),
        SourceLocation(start, endOffset)
      )

    begin

  private def parseClassItemNonRange(): ClassItem =
    val start = offset

    if featureSet.allowsClassPosix && startWith("[:") then
      val invert = currentChar == '^'
      if invert then next()
      val name = parseID()
      if startWith(":]") then
        val end = offset
        return ClassItem(ClassItemData.ClassPosix(invert, name), SourceLocation(start, end))
      reset(start)

    if featureSet.allowsClassNest && currentChar == '[' then
      next()
      val invert = currentChar == '^'
      if invert then next()
      val item = parseClassItem(!featureSet.allowsClassEmpty)
      if currentChar == ']' then
        next()
        val end = offset
        return ClassItem(ClassItemData.ClassNest(invert, item), SourceLocation(start, end))
      assert(featureSet.allowsBrokenCloseBracket, "Unclosed ']'")
      reset(start)

    if currentChar == '\\' then
      next()
      if isLiteralBackslashInClass then
        val kind = BackslashKind.Unknown(currentChar.toInt)
        next()
        return ClassItem.Value(ClassItemData.ClassBackslashValue(kind), SourceLocation(start, offset))
      val backslash = parseBackslashKind()
      val end = offset
      backslash match
        case kind: BackslashKind.BackslashValue if pendingCodePoints.nonEmpty =>
          val values = (kind.value :: pendingCodePoints).map: value =>
            ClassItem.Value(ClassItemData.ClassBackslashValue(BackslashKind.Escape(EscapeStyle.UnicodeBracket, value)))
          pendingCodePoints = Nil
          return ClassItem(ClassItemData.ClassUnion(values), SourceLocation(start, end))
        case kind: BackslashKind.BackslashValue =>
          return ClassItem.Value(ClassItemData.ClassBackslashValue(kind), SourceLocation(start, end))
        case BackslashKind.EscapeClass(
              EscapeClassKind.GeneralNewline | EscapeClassKind.GraphemeCluster | EscapeClassKind.NonNewline
            ) =>
          fail("Invalid escape sequence", start)
        case BackslashKind.EscapeClass(klass) =>
          return ClassItem(ClassItemData.ClassBackslashClass(klass), SourceLocation(start, end))
        case BackslashKind.CaseCommand(CaseCommandKind.QuoteCommand) =>
          return parseClassQuote(start)
        case _ =>
          fail("Invalid escape sequence", start)

    parseClassItemLiteral()

  private def isLiteralBackslashInClass: Boolean =
    !isEnd && ((currentChar: @switch) match
      case 'A' | 'z' | 'Z' | 'G' | 'K' | 'R' | 'X'       => featureSet.treatsNonClassBackslashInClassAsLiteral
      case 'g' | 'k'                                     => featureSet.treatsBackReferenceBackslashInClassAsLiteral
      case 'l' | 'u' | 'L' | 'U' | 'F' | 'E' | 'Q' | 'C' => featureSet.hasBackslashCaseCommand
      case _                                             => false)

  private def parseClassQuote(start: Int): ClassItem =
    val items = Seq.newBuilder[ClassItem]
    while !isEnd && !source.startsWith("\\E", offset) do items.addOne(parseClassItemLiteral())
    startWith("\\E")
    ClassItem(ClassItemData.ClassUnion(items.result()), SourceLocation(start, offset))

  private def parseClassItemLiteral(): ClassItem =
    val value =
      if featureSet.readsAsUnicode then currentCodePoint else currentChar.toInt
    val start = offset
    nextCodePoint(value)
    val end = offset
    ClassItem.Value(ClassItemData.ClassLiteral(value), SourceLocation(start, end))

  private def isValueClassItem(item: ClassItem): Boolean =
    item.isInstanceOf[ClassItem.Value]

  private def parseCloseCurly(stack: mutable.Stack[Node]): Unit =
    assert(featureSet.allowsBrokenCloseCurly, "Incomplete quantifier")
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Literal('}'), SourceLocation(start, end)))

  private def parseCloseBracket(stack: mutable.Stack[Node]): Unit =
    assert(featureSet.allowsBrokenCloseBracket, "Unclosed ']'")
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Literal(']'), SourceLocation(start, end)))

  private def parseLiteral(stack: mutable.Stack[Node]): Unit =
    val value =
      if featureSet.readsAsUnicode then currentCodePoint else currentChar.toInt
    val start = offset
    nextCodePoint(value)
    val end = offset
    stack.push(Node(NodeData.Literal(value), SourceLocation(start, end)))

  private def protectContext[A](body: => A): A =
    val savedContext = context
    val value = body
    context = savedContext
    value

  private def applyFlagSetDiff(diff: FlagSetDiff): Unit =
    if diff.added.verbose then context = context.copy(skipsComment = true)
    if diff.removed.exists(_.verbose) then context = context.copy(skipsComment = false)

  private def applyFlagSetDiffWith(diff: FlagSetDiff)(body: => Unit): Unit =
    protectContext:
      applyFlagSetDiff(diff)
      body

  private def applyResetFlag(flagSet: FlagSet): Unit =
    context = context.copy(skipsComment = false)
    if flagSet.verbose then context = context.copy(skipsComment = true)

  private def applyResetFlagWith(flagSet: FlagSet)(body: => Unit): Unit =
    protectContext:
      applyResetFlag(flagSet)
      body

  private def parseName(end: Char): String =
    val name = parseID()
    assert(!isEnd && currentChar == end, "Invalid identifier")
    next()
    name

  private def parseID(): String =
    val start = offset

    if !featureSet.allowsInvalidIdentifier then
      assert(isIDStart, "Invalid identifier")
      next()
    while isIDContinue do next()

    val end = offset
    assert(start < end, "Invalid identifier")

    source.slice(start, end)

  private def parseIntOption(): Option[Int] =
    if !isDigit then return None

    val start = offset
    while isDigit do next()
    val end = offset

    val str = source.slice(start, end)
    str.toIntOption.orElse(fail("Invalid integer", start))

  private def parseSaturatedIntOption(max: Long, message: String, pos: Int): Option[Int] =
    if !isDigit then return None

    var value = 0L
    while isDigit do
      value = if value > (Long.MaxValue - 9) / 10 then Long.MaxValue else value * 10 + (currentChar - '0')
      next()

    if value > max then fail(message, pos)
    Some(value.min(Int.MaxValue).toInt)

  private def parseHexDigits(size: Int): Long =
    var value = 0L
    var count = 0
    while count < size && isHexDigit do
      value = (value * 16 + Character.digit(currentChar, 16)).min(Long.MaxValue / 16)
      count += 1
      next()
    value

  private def parseBracedCodePoint(radix: Int): Int =
    skipBraceBlank()
    val start = offset
    var value = 0L
    var count = 0
    while !isEnd && (if radix == 8 then isOctalDigit else isHexDigit) do
      value = (value * radix + Character.digit(currentChar, radix)).min(Long.MaxValue / 16)
      count += 1
      next()

    // Perl reads `\x{}` as `\x{0}`, but rejects `\o{}`.
    if featureSet.allowsLooseBraceEscape && (count > 0 || radix == 16 || currentChar != '}') then
      while !isEnd && currentChar != '}' do next()
    else
      assert(count > 0, "Invalid backslash escape")
      skipBraceBlank()
    expect('}', "Invalid backslash escape")
    checkCodePoint(value, start)

  private def checkCodePoint(value: Long, pos: Int): Int =
    val max: Long =
      if featureSet.allowsCodePointAbove10FFFF then Long.MaxValue
      else if featureSet.limitsCodePointTo0xFF then 0xff
      else 0x10ffff
    if value > max then fail("Code point too large", pos)
    if !featureSet.allowsSurrogateEscape && 0xd800 <= value && value <= 0xdfff then fail("Invalid Unicode escape", pos)
    value.min(Int.MaxValue).toInt

  private def skipComment(): Boolean =
    var skipped = false

    var continue = true
    while continue do
      continue = false

      if isSpace then
        while isSpace do next()
        continue = true
        skipped = true

      if !isEnd && currentChar == '#' then
        while !isEnd && !isNewline do
          if featureSet.processBackslashInLineComment && !isEnd && currentChar == '\\' then
            next()
            if !isEnd then next()
          else next()
        continue = true
        skipped = true

    skipped

  private def next(): Unit = reset(offset + 1)

  private def nextCodePoint(codePoint: Int): Unit =
    reset(if codePoint >= 0x10000 then offset + 2 else offset + 1)

  private def reset(save: Int): Unit =
    offset = save
    currentChar = if offset >= source.length then '\u0000' else source.charAt(offset)

  private def currentCodePoint: Int =
    val codePoint1 = currentChar.toInt
    if CodePointUtil.isHighSurrogate(codePoint1) && offset + 1 < source.length then
      val codePoint2 = source.charAt(offset + 1).toInt
      if CodePointUtil.isLowSurrogate(codePoint2) then CodePointUtil.decodeSurrogatePair(codePoint1, codePoint2)
      else codePoint1
    else codePoint1

  private def expect(c: Char, message: String): Unit =
    if isEnd || currentChar != c then fail(message)
    next()

  private def startWith(s: String): Boolean =
    if source.startsWith(s, offset) then
      reset(offset + s.length)
      true
    else false

  private def isEnd: Boolean = offset >= source.length

  private def isSequenceStop: Boolean =
    isEnd || currentChar == '|' || currentChar == ')'

  private def isSpace: Boolean =
    !isEnd && CodePointUtil.isSpace(currentChar.toInt)

  private def isNewline: Boolean =
    !isEnd && CodePointUtil.isNewline(currentChar.toInt)

  private def isDigit: Boolean =
    !isEnd && CodePointUtil.isDigit(currentChar.toInt)

  private def isHexDigit: Boolean =
    !isEnd && CodePointUtil.isHexDigit(currentChar.toInt)

  private def isOctalDigit: Boolean =
    !isEnd && CodePointUtil.isOctalDigit(currentChar.toInt)

  private def isControl: Boolean =
    if isEnd then false
    else if featureSet.hasBackslashControlAny then true
    else if featureSet.hasBackslashControlPrintable then
      0x20 <= currentChar && currentChar <= 0x7e && (currentChar != '{' || featureSet.allowsBackslashControlCurly)
    else
      CodePointUtil.isAlphabetic(currentChar.toInt) ||
      featureSet.hasBackslashControlExtra && '@' <= currentChar && currentChar <= '_' ||
      featureSet.hasBackslashControlDigitInClass && context.inClass && (isDigit || currentChar == '_')

  private def isAlphabetic: Boolean =
    !isEnd && ('a' <= currentChar && currentChar <= 'z' || 'A' <= currentChar && currentChar <= 'Z')

  private def isIDStart: Boolean =
    !isEnd && CodePointUtil.isIDStart(currentChar.toInt)

  private def isIDContinue: Boolean =
    !isEnd && CodePointUtil.isIDContinue(currentChar.toInt)

  private def assert(condition: Boolean, message: String): Unit =
    if !condition then fail(message)

  private def fail(message: String, pos: Int = offset): Nothing =
    throw new ParsingException(message, Some(pos))
