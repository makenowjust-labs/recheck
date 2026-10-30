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
      skipsComment: Boolean,
      allowsDuplicatedGroupName: Boolean
  )

  private[parser] object ParsingContext:
    def from(featureSet: FeatureSet): ParsingContext = ParsingContext(
      inClass = false,
      skipsComment = featureSet.skipsComment,
      allowsDuplicatedGroupName = featureSet.allowsDuplicatedGroupName
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

  private var sequenceStart: Int = 0

  private var startOptionEnd: Int = 0

  private var disjunctionCount: Int = 0

  /** A list of (disjunction ID, alternative index, whether it is a branch reset) of enclosing alternatives. */
  private var alternativePath: List[(Int, Int, Boolean)] = Nil

  private val groupNames: mutable.Map[String, Seq[List[(Int, Int, Boolean)]]] = mutable.Map.empty

  /** The offset of the first reference by a group number, which Ruby rejects in a pattern with named captures. */
  private var numberedReferenceOffset: Option[Int] = None

  // Ruby's `\u{X Y}` is read as the first code point, and the rest are kept here until the caller takes them.
  private var pendingCodePoints: List[Int] = Nil

  def parse(): Node =
    val node = parseDisjunction()
    if !isEnd then
      assert(currentChar == ')', "Unexpected")
      fail("Unmatched ')'")
    if featureSet.rejectsNumberedReferenceWithNamedCapture && groupNames.nonEmpty then
      for pos <- numberedReferenceOffset do fail("Numbered reference with named capture", pos)
    node

  private def parseDisjunction(isBranchReset: Boolean = false): Node =
    val start = offset
    val builder = Seq.newBuilder[Node]
    val id = disjunctionCount
    disjunctionCount += 1
    val savedAlternativePath = alternativePath
    var index = 0
    alternativePath = (id, index, isBranchReset) :: savedAlternativePath
    builder.addOne(parseSequence())

    while !isEnd && currentChar == '|' do
      next()
      index += 1
      alternativePath = (id, index, isBranchReset) :: savedAlternativePath
      builder.addOne(parseSequence())
    alternativePath = savedAlternativePath

    val nodes = builder.result()
    if nodes.size == 1 then return nodes.head

    val end = offset
    Node(NodeData.Disjunction(nodes), SourceLocation(start, end))

  private def parseSequence(): Node =
    val start = offset
    val stack = mutable.Stack.empty[Node]

    val savedSequenceStart = sequenceStart
    sequenceStart = start
    while !isSequenceStop do parseSequenceItem(stack)
    sequenceStart = savedSequenceStart

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
    if stack.isEmpty && featureSet.readsLeadingCurlyAsLiteral then
      resetBracket(stack, save)
      return

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
      // Java ignores a curly quantifier following another quantifier or nothing: `a{2}{3}` matches `aa`.
      case Some(node @ Node(NodeData.Repeat(_, _), _)) if featureSet.ignoresCurlyAfterRepeat =>
        stack.pop()
        stack.push(node.copy(loc = node.loc.copy(end = offset)))
      case None if featureSet.ignoresCurlyAfterRepeat =>
        ()
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
      case NodeData.Command(
            _: CommandKind.InlineFlag | _: CommandKind.ResetFlag | _: CommandKind.StartOption | CommandKind.Callout |
            _: CommandKind.CalloutInt | _: CommandKind.CalloutString
          ) =>
        fail("Nothing to repeat")
      case NodeData.Command(CommandKind.BacktrackControl(kind, _)) =>
        assert(
          featureSet.allowsBacktrackControlRepeat || kind == Some(BacktrackControlKind.Accept),
          "Nothing to repeat"
        )
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
      case '+' if featureSet.allowsInlineFlagSign =>
        parseSignedInlineFlagGroup(stack, start)
      case '-' if featureSet.allowsInlineFlagSign =>
        parseSignedInlineFlagGroup(stack, start)
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
      case _ if featureSet.allowsInlineFlagSign =>
        parseSignedInlineFlagGroup(stack, start)
      case _ =>
        parseInlineFlagGroup(stack, start)

  private def parseNonCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    parseGroupBody(stack, GroupKind.NonCapture, start)

  private def parseBranchResetGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasBranchReset, "Invalid group")
    next()
    protectContext:
      val node = parseDisjunction(isBranchReset = true)
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
    if isEnd || currentChar != '|' then
      parseGroupBody(stack, GroupKind.Absence, start)
      return

    next()
    if !isEnd && currentChar == ')' then
      pushCommand(stack, CommandKind.AbsenceRangeClear, start)
      return

    protectContext:
      val absent = protectContext(parseSequence())
      if !isEnd && currentChar == '|' then
        next()
        val exp = protectContext(parseDisjunction())
        pushCommand(stack, CommandKind.AbsenceExpression(absent, exp), start)
      else pushCommand(stack, CommandKind.AbsenceStopper(absent), start)

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
      val indexStart = offset
      for n <- parseIntOption() do
        assert(n != 0 || featureSet.allowsZeroIndexedCaptureTest, "Invalid condition", indexStart)
        markNumberedReference(indexStart)
        assert(
          source.charAt(indexStart) != '0' || featureSet.allowsLeadingZeroInCaptureTest,
          "Invalid condition",
          indexStart
        )
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Indexed(n), start)
        boundary.break()

      if featureSet.hasNamedCaptureTest && currentChar == '<' then
        next()
        val test = parseNamedCaptureTest(NameStyle.Angle, '>')
        closeCondition()
        parseConditionalGroupBody(stack, test, start)
        boundary.break()

      if featureSet.hasNamedCaptureTest && currentChar == '\'' then
        next()
        val test = parseNamedCaptureTest(NameStyle.Quote, '\'')
        closeCondition()
        parseConditionalGroupBody(stack, test, start)
        boundary.break()

      if featureSet.hasDefineTest && startWith("DEFINE)") then
        reset(offset - 1)
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Define, start)
        boundary.break()

      if featureSet.hasVersionTest && startWith("VERSION") then
        val lt = !isEnd && currentChar == '>'
        if lt then next()
        expect('=', "Invalid condition")
        val major = parseIntOption().getOrElse(fail("Invalid condition"))
        val minor =
          if !isEnd && currentChar == '.' then
            next()
            parseIntOption().getOrElse(fail("Invalid condition"))
          else 0
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Version(lt, major, minor), start)
        boundary.break()

      val isRecursionTest =
        currentChar == 'R' && offset + 1 < source.length && (source.charAt(offset + 1) match
          case ')' | '&' => true
          case c         => CodePointUtil.isDigit(c))
      if featureSet.hasRecursionTest && isRecursionTest then
        next()
        (currentChar: @switch) match
          case ')' =>
            closeCondition()
            parseConditionalGroupBody(stack, ConditionalTest.RRecursion, start)
          case '&' =>
            next()
            val name = parseGroupNameBody()
            closeCondition()
            parseConditionalGroupBody(stack, ConditionalTest.NamedRecursion(name), start)
          case _ =>
            val n = parseIntOption().getOrElse(fail("Invalid group"))
            closeCondition()
            parseConditionalGroupBody(stack, ConditionalTest.IndexedRecursion(n), start)
        boundary.break()

      if featureSet.hasBareNamedCaptureTest && isGroupNameStart then
        val name = parseGroupNameBody()
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

      if featureSet.hasBareLookAroundTest then
        // .NET reads `(?(?:x)...)` and `(?(?>x)...)` as `(?(x)...)` because the condition is a look-ahead.
        if !startWith("?:") then startWith("?>")
        parseLookAroundConditional(stack, None, start)
        boundary.break()

      fail("Invalid condition")

  private def parseNamedCaptureTest(style: NameStyle, end: Char): ConditionalTest =
    if !featureSet.allowsNonNameCaptureTest then return ConditionalTest.Named(style, parseGroupName(end))

    // Ruby accepts `(?(<1>)...)` even in a pattern with named captures.
    val savedNumberedReferenceOffset = numberedReferenceOffset
    val ref = parseRef(end)
    numberedReferenceOffset = savedNumberedReferenceOffset
    expect(end, "Invalid identifier")
    ref match
      case Reference.NamedReference(name) => ConditionalTest.Named(style, name)
      case ref                            => ConditionalTest.Referenced(style, ref)

  private def closeCondition(): Unit =
    expect(')', "Invalid condition")

  private def parseConditionalGroupBody(stack: mutable.Stack[Node], test: ConditionalTest, start: Int): Unit =
    protectContext:
      val yes = protectContext(parseSequence())
      if test == ConditionalTest.Define then assert(isEnd || currentChar != '|', "Invalid condition")
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

    val name = parseGroupName(end = end)
    addGroupName(name, start)
    parseGroupBody(stack, GroupKind.NamedCapture(style, name), start)

  private def parseBalanceGroup(stack: mutable.Stack[Node], end: Char, style: NameStyle, start: Int): Unit =
    if currentChar == '-' then
      next()
      val test = parseGroupName(end = end)
      parseGroupBody(stack, GroupKind.Balance(style, None, test), start)
      return

    val name = parseGroupNameBody()
    if currentChar == end then
      next()
      parseGroupBody(stack, GroupKind.NamedCapture(style, name), start)
      return

    if currentChar == '-' then
      next()
      val test = parseGroupName(end = end)
      parseGroupBody(stack, GroupKind.Balance(style, Some(name), test), start)
      return

    fail("Invalid identifier")

  private def parsePGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasPGroup, "Invalid group")
    next()
    (currentChar: @switch) match
      case '<' =>
        next()
        val name = parseGroupName(end = '>')
        addGroupName(name, start)
        parseGroupBody(stack, GroupKind.PNamedCapture(name), start)
      case '=' =>
        next()
        val name = parseGroupNameBody()
        pushCommand(stack, CommandKind.PBackReference(name), start)
      case '>' =>
        assert(featureSet.hasPCall, "Invalid group")
        next()
        val name = parseGroupNameBody()
        pushCommand(stack, CommandKind.PNamedCall(name), start)
      case _ =>
        fail("Invalid group")

  private def addGroupName(name: String, start: Int): Unit =
    val paths = groupNames.getOrElse(name, Seq.empty)
    if !context.allowsDuplicatedGroupName then
      for path <- paths do
        val exclusive = path.exists: (id, index, isBranchReset) =>
          (isBranchReset || featureSet.allowsDuplicatedGroupNameInAlternatives) &&
            alternativePath.exists((id2, index2, _) => id == id2 && index != index2)
        assert(exclusive, "Duplicated group name", start)
    groupNames(name) = paths :+ alternativePath

  private def parseRGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasRCall, "Invalid group")
    next()
    pushCommand(stack, CommandKind.RCall, start)

  private def parseNamedCallGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasCallCommand, "Invalid group")
    next()
    val name = parseGroupNameBody()
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
        assert(featureSet.hasInlineFlagGroup, "Invalid group")
        val removedStart = offset
        val (removed, removedFlags) = parseRemovedFlagSet()
        checkInlineFlagRemoval("", removedFlags, removedStart)
        val diff = FlagSetDiff(FlagSet(), Some(removed))
        (currentChar: @switch) match
          case ')' =>
            pushInlineFlagCommand(stack, diff, start)
          case ':' =>
            next()
            applyFlagSetDiffWith(diff):
              parseGroupBody(stack, GroupKind.InlineFlag(diff), start)
          case _ =>
            fail("Invalid group")

  private def parseResetFlagGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.hasResetFlag, "Invalid group")
    next()
    val (flagSet, _) = parseFlagSet(resetting = true)
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
            assert(value <= 255, "Invalid group")
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
    assert(featureSet.hasInlineFlagGroup, "Invalid group")

    val (added, addedFlags) = parseFlagSet()

    (currentChar: @switch) match
      case '-' =>
        next()
        val removedStart = offset
        val (removed, removedFlags) = parseRemovedFlagSet()
        checkInlineFlagRemoval(addedFlags, removedFlags, removedStart)
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
        assert(addedFlags.nonEmpty || featureSet.allowsEmptyInlineFlag, "Invalid flag")
        val diff = FlagSetDiff(added, None)
        pushInlineFlagCommand(stack, diff, start)
      case _ =>
        fail("Invalid flag")

  /** Parses an inline flag group with any sequence of flags and signs (e.g. `(?i-m+s)`). */
  private def parseSignedInlineFlagGroup(stack: mutable.Stack[Node], start: Int): Unit =
    val flagsStart = offset
    var added = FlagSet()
    var removed: Option[FlagSet] = None
    var removing = false
    boundary:
      while !isEnd do
        if currentChar == '+' then
          removing = false
          next()
        else if currentChar == '-' then
          removing = true
          if removed.isEmpty then removed = Some(FlagSet())
          next()
        else
          val (flagSet, flags) = parseFlagSet(removing = removing, initial = if removing then removed.get else added)
          if removing then removed = Some(flagSet) else added = flagSet
          if flags.isEmpty then boundary.break()
    assert(offset > flagsStart || featureSet.allowsEmptyInlineFlag, "Invalid flag")

    val diff = FlagSetDiff(added, removed)
    (currentChar: @switch) match
      case ':' =>
        next()
        applyFlagSetDiffWith(diff):
          parseGroupBody(stack, GroupKind.InlineFlag(diff), start)
      case ')' =>
        pushInlineFlagCommand(stack, diff, start)
      case _ =>
        fail("Invalid flag")

  /** Parses inline flag letters to remove, and Ruby also accepts more `-` among them (e.g. `(?i-m-x)`). */
  private def parseRemovedFlagSet(): (FlagSet, String) =
    var (removed, removedFlags) = parseFlagSet(removing = true)
    while featureSet.allowsRepeatedInlineFlagMinus && !isEnd && currentChar == '-' do
      next()
      val (flagSet, flags) = parseFlagSet(removing = true, initial = removed)
      removed = flagSet
      removedFlags += flags
    (removed, removedFlags)

  private def checkInlineFlagRemoval(addedFlags: String, removedFlags: String, removedStart: Int): Unit =
    if removedFlags.isEmpty then
      if addedFlags.isEmpty then assert(featureSet.allowsEmptyInlineFlagDiff, "Invalid flag")
      else assert(featureSet.allowsEmptyInlineFlagRemoval, "Invalid flag")
    if !featureSet.allowsToggledInlineFlag then
      for (c, i) <- removedFlags.zipWithIndex do if addedFlags.contains(c) then fail("Invalid flag", removedStart + i)

  private def pushInlineFlagCommand(stack: mutable.Stack[Node], diff: FlagSetDiff, start: Int): Unit =
    assert(featureSet.hasInlineFlag, "Invalid group", start)
    if diff.removed.isDefined then assert(featureSet.hasInlineFlagRemovalCommand, "Invalid group", start)
    if featureSet.requiresInlineFlagAtStart then
      val atStart = sequenceStart == 0 && stack.forall:
        case Node(NodeData.Command(_: CommandKind.InlineFlag | _: CommandKind.Comment), _) => true
        case _                                                                             => false
      assert(atStart, "Inline flag not at the start", start)
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
    ("F", BacktrackControlKind.F),
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
      if featureSet.hasStartOption && start == startOptionEnd && parseStartOption(stack, start) then
        startOptionEnd = offset
        boundary.break()

      for (command, kind) <- BacktrackControls do
        if parseBacktrackControl(stack, start, command, kind) then boundary.break()

      for (alphabetic, abbrev, kind) <- AlphabeticGroups do
        if parseAlphabetic(stack, start, alphabetic, abbrev, kind) then boundary.break()

      if featureSet.hasScanSubstring then
        if startWith("scan_substring:") then
          parseScanSubstringGroup(stack, AssertNameStyle.Alphabetic, start)
          boundary.break()
        if startWith("scs:") then
          parseScanSubstringGroup(stack, AssertNameStyle.Abbrev, start)
          boundary.break()

      expect(':', "Invalid group")
      val name = parseBacktrackControlName()
      assert(name.nonEmpty, "Invalid group")
      pushCommand(stack, CommandKind.BacktrackControl(None, Some(name)), start)

  private val StartOptions = Seq(
    "UTF8",
    "UTF",
    "UCP",
    "NO_AUTO_POSSESS",
    "NO_DOTSTAR_ANCHOR",
    "NO_JIT",
    "NO_START_OPT",
    "NOTEMPTY_ATSTART",
    "NOTEMPTY",
    "CASELESS_RESTRICT",
    "TURKISH_CASING",
    "CR",
    "LF",
    "CRLF",
    "ANYCRLF",
    "ANY",
    "NUL",
    "BSR_ANYCRLF",
    "BSR_UNICODE"
  )

  private val LimitStartOptions = Seq("LIMIT_DEPTH", "LIMIT_HEAP", "LIMIT_MATCH", "LIMIT_RECURSION")

  private def parseStartOption(stack: mutable.Stack[Node], start: Int): Boolean =
    boundary:
      for name <- StartOptions do
        if source.startsWith(s"$name)", offset) then
          // PCRE2 accepts `(*TURKISH_CASING)` only with `(*UTF)` among the leading options, even with the `u` flag.
          if name == "TURKISH_CASING" then
            val options = "^(?:\\(\\*[A-Z0-9_=]+\\))*".r.findPrefixOf(source).getOrElse("")
            assert(options.contains("(*UTF)") || options.contains("(*UTF8)"), "Invalid group")
          reset(offset + name.length)
          pushCommand(stack, CommandKind.StartOption(name, None), start)
          boundary.break(true)

      for name <- LimitStartOptions do
        if startWith(s"$name=") then
          val value = parseIntOption().getOrElse(fail("Invalid group"))
          pushCommand(stack, CommandKind.StartOption(name, Some(value)), start)
          boundary.break(true)

      false

  private def parseScanSubstringGroup(stack: mutable.Stack[Node], style: AssertNameStyle, start: Int): Unit =
    expect('(', "Invalid group")
    val refs = Seq.newBuilder[Reference.BaseReference]
    refs.addOne(parseScanSubstringReference())
    while !isEnd && currentChar == ',' do
      next()
      refs.addOne(parseScanSubstringReference())
    expect(')', "Invalid group")
    parseGroupBody(stack, GroupKind.ScanSubstring(style, refs.result()), start)

  private def parseScanSubstringReference(): Reference.BaseReference =
    (currentChar: @switch) match
      case '<' | '\'' =>
        val end = if currentChar == '<' then '>' else '\''
        next()
        Reference.NamedReference(parseGroupName(end))
      case '+' | '-' =>
        val sign = if currentChar == '-' then -1 else 1
        next()
        Reference.RelativeReference(sign * parseIntOption().getOrElse(fail("Invalid group")))
      case _ =>
        Reference.IndexedReference(parseIntOption().getOrElse(fail("Invalid group")))

  private def parseBacktrackControlName(): String =
    val start = offset
    while !isEnd && currentChar != ')' do next()
    source.slice(start, offset)

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
        val name = parseBacktrackControlName()
        assert(name.nonEmpty || kind != BacktrackControlKind.Mark, "Invalid group")
        pushCommand(stack, CommandKind.BacktrackControl(Some(kind), Some(name)), start)
        true
      case ')' =>
        assert(kind != BacktrackControlKind.Mark, "Invalid group")
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

  /** Parses inline flag letters, and returns the flag set and the source string of the letters. */
  private def parseFlagSet(
      removing: Boolean = false,
      resetting: Boolean = false,
      initial: FlagSet = FlagSet()
  ): (FlagSet, String) =
    val start = offset
    var flagSet = initial
    def result: (FlagSet, String) = (flagSet, source.slice(start, offset))
    while !isEnd do
      val flagStart = offset
      val c = if featureSet.ignoresInlineFlagCase then currentChar.toLower else currentChar
      (c: @switch) match
        case 'i' =>
          flagSet = flagSet.copy(ignoreCase = true)
        case 'm' =>
          flagSet = flagSet.copy(multiline = true)
        case 'x' =>
          if !featureSet.hasFlagX then return result
          flagSet = flagSet.copy(verbose = true)
          if featureSet.hasFlagXX && source.slice(start, offset).contains('x') then
            flagSet = flagSet.copy(verboseMore = true)
        case 'J' =>
          if !featureSet.hasFlagUpperJ then return result
          flagSet = flagSet.copy(dupNames = true)
        case 'L' =>
          if !featureSet.hasFlagUpperL then return result
          flagSet = flagSet.copy(localeUpper = true)
        case 'U' =>
          if !featureSet.hasFlagUpperU then return result
          flagSet = flagSet.copy(ungreedy = true)
        case 'a' =>
          if !featureSet.hasFlagA then return result
          val subOption =
            if featureSet.hasFlagASubOption then source.lift(offset + 1).filter("DPSTW".contains(_)) else None
          flagSet = subOption match
            case Some('D') => flagSet.copy(asciiDigit = true)
            case Some('P') => flagSet.copy(asciiPosix = true)
            case Some('S') => flagSet.copy(asciiSpace = true)
            case Some('T') => flagSet.copy(asciiPosixDigit = true)
            case Some('W') => flagSet.copy(asciiWord = true)
            case _         => flagSet.copy(ascii = true)
          if subOption.isDefined then next()
        case 'c' =>
          if !featureSet.hasFlagC then return result
          flagSet = flagSet.copy(continue = true)
        case 'd' =>
          if !featureSet.hasFlagD then return result
          flagSet = flagSet.copy(hasIndices = true)
        case 'g' =>
          if !featureSet.hasFlagG then return result
          flagSet = flagSet.copy(global = true)
        case 'l' =>
          if !featureSet.hasFlagL then return result
          flagSet = flagSet.copy(localeLower = true)
        case 'n' =>
          if !featureSet.hasFlagN then return result
          flagSet = flagSet.copy(explicitCapture = true)
        case 'o' =>
          if !featureSet.hasFlagO then return result
          flagSet = flagSet.copy(once = true)
        case 'p' =>
          if !featureSet.hasFlagP then return result
          flagSet = flagSet.copy(preserve = true)
        case 'r' =>
          if !featureSet.hasFlagR then return result
          flagSet = flagSet.copy(nonDestructive = true)
        case 's' =>
          if !featureSet.hasFlagS then return result
          flagSet = flagSet.copy(dotAll = true)
        case 'u' =>
          if !featureSet.hasFlagU then return result
          flagSet = flagSet.copy(unicode = true)
        case _ =>
          return result
      checkFlag(c, source.slice(start, flagStart), removing, resetting)
      next()
    result

  private val CharsetFlags = "adluL"

  private def checkFlag(c: Char, previous: String, removing: Boolean, resetting: Boolean): Unit =
    if !featureSet.allowsDuplicatedInlineFlag && previous.contains(c) then fail("Duplicated flag")
    if !CharsetFlags.contains(c) then return
    if removing && featureSet.checksCharsetInlineFlagRemoval then fail("Invalid flag")
    // Perl rejects `(?^d)` because `d` is the default character set.
    if resetting && c == 'd' then fail("Invalid flag")
    if featureSet.checksCharsetInlineFlagConflict && previous.exists(p => CharsetFlags.contains(p) && p != c) then
      fail("Invalid flag")
    // Perl accepts `a` up to twice (`aa`), and the other character set flags only once.
    if featureSet.checksCharsetInlineFlagCount && previous.count(_ == c) >= (if c == 'a' then 2 else 1) then
      fail("Duplicated flag")

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
    if featureSet.allowsNamelessBackslashKG && !source.lift(offset + 1).exists(c => c == '<' || c == '\'') then
      return parseBackslashUnknown()

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
        BackslashKind.EscapeBackReference(BackReferenceStyle.GBackReference(NameStyle.Bare), parseRelativeReference())
      case '{' =>
        assert(featureSet.hasBackslashGBackReference, "Invalid backslash escape")
        next()
        skipBraceBlank()
        val ref = parseBaseRef('}')
        skipBraceBlank()
        expect('}', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.GBackReference(NameStyle.Curly), ref)
      case '<' | '\'' =>
        assert(featureSet.hasBackslashGCall, "Invalid backslash escape")
        val style = if currentChar == '<' then NameStyle.Angle else NameStyle.Quote
        val end = if style == NameStyle.Angle then '>' else '\''
        next()
        val ref = parseBaseRef(end)
        expect(end, "Invalid backslash escape")
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
    if featureSet.allowsNamelessBackslashKG && !source.lift(offset + 1).exists(c => c == '<' || c == '\'') then
      return parseBackslashUnknown()

    next()
    (currentChar: @switch) match
      case '<' =>
        next()
        val ref = parseKRef('>')
        expect('>', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.KBackReference(NameStyle.Angle), ref)
      case '\'' =>
        assert(featureSet.hasBackslashKBackReferenceQuote, "Invalid backslash escape")
        next()
        val ref = parseKRef('\'')
        expect('\'', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.KBackReference(NameStyle.Quote), ref)
      case '{' =>
        assert(featureSet.hasBackslashKBackReferenceCurly, "Invalid backslash escape")
        next()
        skipBraceBlank()
        val ref = parseKRef('}')
        skipBraceBlank()
        expect('}', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.KBackReference(NameStyle.Curly), ref)
      case _ =>
        fail("Invalid backslash escape")

  private def parseKRef(end: Char): Reference =
    if featureSet.allowsNonNameKBackReference then return parseRef(end)
    // .NET accepts `\k<0>`, though it rejects `(?<0>...)`.
    if featureSet.allowsNumberGroupName && isDigit then
      val start = offset
      parseIntOption()
      return Reference.NamedReference(source.slice(start, offset))
    Reference.NamedReference(parseGroupNameBody(end))

  /** Parses a Ruby's `\\k` reference, where a name ends before a level (e.g. `\\k<x+1>`) and a leading `+` is a part of
    * a name.
    */
  private def parseRef(end: Char): Reference =
    val ref = (currentChar: @switch) match
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        val start = offset
        val index = parseIntOption().get
        assert(index != 0, "Invalid backslash escape", start)
        markNumberedReference(start)
        Reference.IndexedReference(index)
      case '-' =>
        parseRelativeReference()
      case _ =>
        Reference.NamedReference(parseGroupNameBody(end, levelable = true))

    if !featureSet.hasLeveledBackReference then return ref

    (currentChar: @switch) match
      case '+' | '-' =>
        val sign = if currentChar == '-' then -1 else 1
        next()
        val level = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        Reference.LeveledReference(ref, sign * level)
      case _ => ref

  private def parseBaseRef(end: Char): Reference.BaseReference =
    if !featureSet.allowsNonNameBaseRef then return Reference.NamedReference(parseGroupNameBody(end))

    (currentChar: @switch) match
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        val start = offset
        val index = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        if index != 0 then markNumberedReference(start)
        Reference.IndexedReference(index)
      case '+' if featureSet.allowsForwardRelativeReference =>
        parseRelativeReference()
      case '-' =>
        parseRelativeReference()
      case _ =>
        Reference.NamedReference(parseGroupNameBody(end))

  private def markNumberedReference(pos: Int): Unit =
    if numberedReferenceOffset.isEmpty then numberedReferenceOffset = Some(pos)

  private def parseRelativeReference(): Reference.RelativeReference =
    assert(currentChar == '-' || featureSet.allowsForwardRelativeReference, "Invalid backslash escape")
    markNumberedReference(offset)
    val sign = if currentChar == '-' then -1 else 1
    next()
    val start = offset
    val n = parseIntOption().getOrElse(fail("Invalid backslash escape"))
    assert(n != 0, "Invalid backslash escape", start)
    Reference.RelativeReference(sign * n)

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
        assert(!"()[]{/-".contains(currentChar), "Invalid set operation")
        val isDouble =
          "&!#$%*+,.:;<=>?@^`~".contains(currentChar) && source.startsWith(s"$currentChar$currentChar", offset)
        assert(!isDouble, "Invalid set operation")
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

    markNumberedReference(save)
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

    if featureSet.hasClassPosixWordBoundary && (startWith("[[:<:]]") || startWith("[[:>:]]")) then
      val name = source.slice(start + 3, start + 4)
      val end = offset
      val item = ClassItem(ClassItemData.ClassPosix(false, name), SourceLocation(start + 1, end - 1))
      stack.push(Node(NodeData.Class(false, item), SourceLocation(start, end)))
      return

    next()

    if featureSet.checksClassPosixOutsideClass then
      findClassPosixEnd(start).foreach: _ =>
        if currentChar == ':' then fail("POSIX class outside class", start)
        fail("Unsupported POSIX collating element", start)

    val invert = currentChar == '^'
    if invert then next()

    val item = parseClassItem(!featureSet.allowsClassEmpty)
    expect(']', "Unclosed ']'")
    checkNegatedClassSet(invert, item, start)

    val end = offset
    stack.push(Node(NodeData.Class(invert, item), SourceLocation(start, end)))

  private def parseClassItem(first: Boolean): ClassItem =
    val start = offset

    var item = parseClassItemUnion(first)
    val operands = Seq.newBuilder[ClassItem]
    val operators = mutable.Set.empty[String]

    var continue = true
    while continue do
      continue = false

      if featureSet.allowsClassIntersection && startWith("&&") then
        continue = true
        if featureSet.checksClassSetSyntax then assert(isEnd || currentChar != '&', "Invalid class set character")
        val right = parseClassItemUnion(false)
        val end = offset
        if !featureSet.allowsEmptyClassIntersection then
          assert(!(isEmptyClassItem(item) && isEmptyClassItem(right)), "Invalid class set operation", start)
        operands.addOne(item).addOne(right)
        operators.add("&&")
        item = ClassItem(ClassItemData.ClassIntersection(item, right), SourceLocation(start, end))

      if featureSet.allowsClassDiff && startWith("--") then
        continue = true
        val right = parseClassItemUnion(false)
        val end = offset
        operands.addOne(item).addOne(right)
        operators.add("--")
        item = ClassItem(ClassItemData.ClassDiff(item, right), SourceLocation(start, end))

      if featureSet.hasClassSubtraction && startWith("-[") then
        val nestStart = offset - 1
        val invert = currentChar == '^'
        if invert then next()
        val inner = parseClassItem(true)
        expect(']', "Unclosed ']'")
        val nest = ClassItem(ClassItemData.ClassNest(invert, inner), SourceLocation(nestStart, offset))
        assert(isEnd || currentChar == ']', "Class subtraction must be last")
        item = ClassItem(ClassItemData.ClassDiff(item, nest), SourceLocation(start, offset))

    if featureSet.checksClassSetSyntax && operators.nonEmpty then
      assert(operators.size == 1, "Invalid class set operation", start)
      assert(operands.result().forall(isClassSetOperand), "Invalid class set operation", start)

    item

  // Java skips whitespace and comments in a class in the comments mode.
  private def skipClassComment(): Unit =
    if context.skipsComment && featureSet.skipsCommentInClass then skipComment()

  private def isEmptyClassItem(item: ClassItem): Boolean =
    item.data == ClassItemData.ClassUnion(Seq.empty)

  private def isClassSetOperand(item: ClassItem): Boolean =
    item.data match
      case ClassItemData.ClassUnion(_) | ClassItemData.ClassRange(_, _) => false
      case _                                                            => true

  /** Tests whether the class item may contain strings (the `MayContainStrings` operation in the ECMAScript spec).
    *
    * Properties of strings are checked on building IR, where Unicode properties are resolved.
    */
  private def mayContainStrings(item: ClassItem): Boolean =
    item.data match
      case ClassItemData.ClassUnion(items)              => items.exists(mayContainStrings)
      case ClassItemData.ClassIntersection(left, right) => mayContainStrings(left) && mayContainStrings(right)
      case ClassItemData.ClassDiff(left, _)             => mayContainStrings(left)
      case ClassItemData.ClassNest(invert, item)        => !invert && mayContainStrings(item)
      case ClassItemData.ClassBackslashClass(EscapeClassKind.QuoteSet(strings)) => strings.exists(_.size != 1)
      case _                                                                    => false

  private def checkNegatedClassSet(invert: Boolean, item: ClassItem, start: Int): Unit =
    if featureSet.checksClassSetSyntax && invert then
      assert(!mayContainStrings(item), "Negated class may contain strings", start)

  private def parseClassItemUnion(first: Boolean): ClassItem =
    val start = offset
    val seq = Seq.newBuilder[ClassItem]

    var count = 0

    skipClassComment()
    while (first && count == 0 && !isEnd && currentChar == ']') || !isClassItemStop(start) do
      parseClassItemAtom(seq, start)
      count += 1
      skipClassComment()

    val end = offset
    val items = seq.result()
    if items.size == 1 then return items.head
    ClassItem(ClassItemData.ClassUnion(items), SourceLocation(start, end))

  private def isClassItemStop(start: Int): Boolean =
    isEnd ||
      currentChar == ']' ||
      featureSet.allowsClassIntersection && source.startsWith("&&", offset) ||
      featureSet.allowsClassDiff && source.startsWith("--", offset) ||
      featureSet.hasClassSubtraction && start != offset && source.startsWith("-[", offset)

  private def parseClassItemAtom(seq: mutable.Builder[ClassItem, Seq[ClassItem]], unionStart: Int): Unit =
    val start = offset
    val item = parseClassItemNonRange()

    skipClassComment()
    if isClassItemStop(unionStart) || currentChar != '-' then
      seq.addOne(item)
      return

    val save = offset
    next()

    if isClassItemStop(unionStart) then
      reset(save)
      seq.addOne(item)
      return

    val begin = classQuoteLiterals(item) match
      case Some(literals) if literals.nonEmpty =>
        seq.addAll(literals.init)
        literals.last
      case _ => item

    skipClassComment()
    val endItem = parseClassItemNonRange()
    val (end, rest) = classQuoteLiterals(endItem) match
      case Some(literals) if literals.nonEmpty => (literals.head, literals.tail)
      case _                                   => (endItem, Seq.empty)

    (begin, end) match
      case (begin: ClassItem.Value, end: ClassItem.Value) =>
        assert(begin.data.value <= end.data.value, "Out of order class range", begin.loc.start)
        seq.addOne(ClassItem(ClassItemData.ClassRange(begin, end), SourceLocation(begin.loc.start, end.loc.end)))
        seq.addAll(rest)
      case _ =>
        if !isValueClassItem(begin) then
          if !isClassNestItem(begin) then assert(featureSet.allowsBrokenClassRangeBegin, "Invalid class range", start)
        else if !isClassNestItem(end) then assert(featureSet.allowsBrokenClassRangeEnd, "Invalid class range", start)

        seq.addOne(begin)
        if featureSet.consumesBrokenClassRange then
          seq.addOne(ClassItem.Value(ClassItemData.ClassLiteral('-'), SourceLocation(save, save + 1)))
          seq.addOne(end)
          seq.addAll(rest)
        else reset(save)

  /** Returns the literals of a `\\Q...\\E` class item, because Java and PCRE read them as unquoted for a range. */
  private def classQuoteLiterals(item: ClassItem): Option[Seq[ClassItem]] =
    item.data match
      case ClassItemData.ClassUnion(items) if source.startsWith("\\Q", item.loc.start) => Some(items)
      case _                                                                           => None

  private def parseClassItemNonRange(): ClassItem =
    val start = offset

    if featureSet.allowsClassPosix && currentChar == '[' then
      parseClassPosixOption() match
        case Some(item) => return item
        case None       => ()

    if featureSet.allowsClassNest && currentChar == '[' then
      next()
      val invert = currentChar == '^'
      if invert then next()
      val item = parseClassItem(!featureSet.allowsClassEmpty)
      expect(']', "Unclosed ']'")
      checkNegatedClassSet(invert, item, start)
      val end = offset
      return ClassItem(ClassItemData.ClassNest(invert, item), SourceLocation(start, end))

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

  private val ClassPosixNames: Set[String] = Set(
    "alnum",
    "alpha",
    "ascii",
    "blank",
    "cntrl",
    "digit",
    "graph",
    "lower",
    "print",
    "punct",
    "space",
    "upper",
    "word",
    "xdigit"
  )

  /** Parses a POSIX class like `[:alpha:]`.
    *
    * It returns `None` if the source does not look like a POSIX class, and it returns the `[` literal if the source
    * looks like a POSIX class but the dialect reads it as literals.
    */
  private def parseClassPosixOption(): Option[ClassItem] =
    val start = offset
    val bodyStart = offset + 2
    val bodyEnd = findClassPosixEnd(start) match
      case Some(end) => end
      case None      => return None

    if source.charAt(start + 1) != ':' then fail("Unsupported POSIX collating element", start)

    val body = source.slice(bodyStart, bodyEnd)
    val invert = body.startsWith("^")
    val name = if invert then body.drop(1) else body

    if ClassPosixNames.contains(name) then
      reset(bodyEnd + 2)
      return Some(ClassItem(ClassItemData.ClassPosix(invert, name), SourceLocation(start, offset)))

    if featureSet.hasClassPosixNameHeuristic && !isClassPosixLikeName(name) ||
      featureSet.hasClassPosixNameLimit && !ClassPosixNames.exists(name.startsWith) &&
      (name.length > 20 || name.exists(c => c == ':' || c == ']'))
    then return Some(parseClassItemLiteral())

    fail("Unknown POSIX class", start)

  /** Finds the end of a POSIX class (or a POSIX collating element if it is supported) beginning at the given offset. */
  private def findClassPosixEnd(start: Int): Option[Int] =
    if start + 1 >= source.length then return None
    val terminator = source.charAt(start + 1)
    if !(terminator == ':' || featureSet.checksClassPosixCollating && (terminator == '.' || terminator == '=')) then
      return None

    var i = start + 2
    while i + 1 < source.length do
      val c = source.charAt(i)
      val d = source.charAt(i + 1)
      if c == '\\' && (d == ']' || d == '\\') then i += 1
      else if c == '[' && d == terminator || c == ']' then return None
      else if c == terminator && d == ']' then return Some(i)
      i += 1
    None

  // Perl reads the name as literals if it is surely not meant to be a POSIX class name.
  private def isClassPosixLikeName(name: String): Boolean =
    3 <= name.length && name.length <= 14 &&
      !name.exists(c => c.isUpper || c.isWhitespace || c == '\\' || c == ']')

  private val ClassSetSyntaxCharacters: Set[Char] = "()[]{}/-\\|".toSet

  private val ClassSetReservedDoublePunctuators: Set[Char] = "&!#$%*+,.:;<=>?@^`~".toSet

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
    if featureSet.checksClassSetSyntax then
      assert(!ClassSetSyntaxCharacters.contains(currentChar), "Invalid class set character")
      assert(
        !(ClassSetReservedDoublePunctuators
          .contains(currentChar) && source.startsWith(s"$currentChar$currentChar", offset)),
        "Invalid class set character"
      )
    val value =
      if featureSet.readsAsUnicode then currentCodePoint else currentChar.toInt
    val start = offset
    nextCodePoint(value)
    val end = offset
    ClassItem.Value(ClassItemData.ClassLiteral(value), SourceLocation(start, end))

  private def isValueClassItem(item: ClassItem): Boolean =
    item.isInstanceOf[ClassItem.Value]

  private def isClassNestItem(item: ClassItem): Boolean =
    item.data.isInstanceOf[ClassItemData.ClassNest]

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
    if diff.added.dupNames then context = context.copy(allowsDuplicatedGroupName = true)
    if diff.removed.exists(_.dupNames) then context = context.copy(allowsDuplicatedGroupName = false)

  private def applyFlagSetDiffWith(diff: FlagSetDiff)(body: => Unit): Unit =
    protectContext:
      applyFlagSetDiff(diff)
      body

  private def applyResetFlag(flagSet: FlagSet): Unit =
    context = context.copy(skipsComment = false)
    if flagSet.verbose then context = context.copy(skipsComment = true)
    if flagSet.dupNames then context = context.copy(allowsDuplicatedGroupName = true)

  private def applyResetFlagWith(flagSet: FlagSet)(body: => Unit): Unit =
    protectContext:
      applyResetFlag(flagSet)
      body

  private def parseGroupName(end: Char): String =
    val name = parseGroupNameBody(end)
    assert(!isEnd && currentChar == end, "Invalid identifier")
    next()
    name

  /** Parses a group name, and returns the name with escape sequences decoded.
    *
    * If `levelable` is true, a loose name ends before `+` or `-` for a level.
    */
  private def parseGroupNameBody(end: Char = ')', levelable: Boolean = false): String =
    val start = offset

    if featureSet.allowsLooseGroupName then
      assert(!isEnd && currentChar != end && currentChar != '-' && !isDigit, "Invalid identifier")
      next()
      while !isEnd && currentChar != end && !(levelable && (currentChar == '+' || currentChar == '-')) do
        assert(currentChar != ')', "Invalid identifier")
        next()
      return source.slice(start, offset)

    if featureSet.allowsNumberGroupName && isDigit then
      val n = parseIntOption().get
      assert(n != 0, "Invalid identifier", start)
      return source.slice(start, offset)

    val name = new java.lang.StringBuilder
    boundary:
      while !isEnd do
        val save = offset
        val c =
          if featureSet.allowsEscapeInGroupName && currentChar == '\\' then parseGroupNameEscape()
          else
            val c = currentCodePoint
            nextCodePoint(c)
            c
        if c < 0 || !(if name.length == 0 then isGroupNameStart(c) else isGroupNameContinue(c)) then
          reset(save)
          boundary.break()
        name.appendCodePoint(c)

    assert(name.length > 0, "Invalid identifier", start)
    if featureSet.limitsGroupNameLength then assert(offset - start <= 128, "Invalid identifier", start)
    name.toString

  /** Parses a `\\u` escape sequence in a group name, and returns its code point or `-1` if it is invalid. */
  private def parseGroupNameEscape(): Int =
    next()
    if isEnd || currentChar != 'u' then return -1
    next()
    if !isEnd && currentChar == '{' then
      next()
      if !isHexDigit then return -1
      val value = parseHexDigits(Int.MaxValue)
      if value > 0x10ffff || isEnd || currentChar != '}' then return -1
      next()
      return value.toInt

    val high = parseHex4()
    if CodePointUtil.isHighSurrogate(high) && source.startsWith("\\u", offset) then
      val save = offset
      reset(offset + 2)
      val low = parseHex4()
      if CodePointUtil.isLowSurrogate(low) then return CodePointUtil.decodeSurrogatePair(high, low)
      reset(save)
    high

  private def parseHex4(): Int =
    val start = offset
    for _ <- 1 to 4 do
      if !isHexDigit then return -1
      next()
    Integer.parseInt(source.slice(start, offset), 16)

  private def isGroupNameStart: Boolean =
    !isEnd && isGroupNameStart(currentCodePoint)

  private def isGroupNameStart(c: Int): Boolean =
    if 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' then true
    else if c == '_' then featureSet.allowsUnderscoreInGroupName
    else if c == '$' then featureSet.allowsDollarInGroupName
    else if c < 0x80 then false
    else if featureSet.allowsUnicodeIdentifierGroupName then Character.isUnicodeIdentifierStart(c)
    else if featureSet.allowsUnicodeWordGroupName then
      (c < 0x10000 || featureSet.readsAsUnicode) && Character.isLetter(c)
    else if featureSet.allowsWordClassGroupName then
      (c < 0x10000 || featureSet.readsAsUnicode) && CodePointUtil.isWord(c)
    else if featureSet.allowsWordIdentifierGroupName then CodePointUtil.isWordIDStart(c)
    else false

  private def isGroupNameContinue(c: Int): Boolean =
    if c < 0x80 then CodePointUtil.isDigit(c) || isGroupNameStart(c)
    else if featureSet.allowsUnicodeIdentifierGroupName then Character.isUnicodeIdentifierPart(c)
    else if featureSet.allowsUnicodeWordGroupName then
      (c < 0x10000 || featureSet.readsAsUnicode) && Character.isLetterOrDigit(c)
    else if featureSet.allowsWordClassGroupName then
      (c < 0x10000 || featureSet.readsAsUnicode) && CodePointUtil.isWord(c)
    else if featureSet.allowsWordIdentifierGroupName then CodePointUtil.isWordIDContinue(c)
    else false

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

  private def assert(condition: Boolean, message: String, pos: Int = offset): Unit =
    if !condition then fail(message, pos)

  private def fail(message: String, pos: Int = offset): Nothing =
    throw new ParsingException(message, Some(pos))
