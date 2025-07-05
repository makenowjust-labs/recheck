package codes.quine.labs.resyntax.parser

import scala.annotation.switch
import scala.collection.mutable
import scala.util.Try

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

object Parser {
  private[parser] final case class ParsingContext(
      inClass: Boolean,
      skipsComment: Boolean
  )

  private[parser] object ParsingContext {
    def from(featureSet: FeatureSet): ParsingContext = ParsingContext(
      inClass = false,
      skipsComment = featureSet.skipsComment
    )
  }

  def parse(source: String, flagSet: FlagSet, dialect: Dialect): Node = {
    val analysis = QuickAnalysis.from(source)
    val featureSet = FeatureSet.from(analysis, dialect, flagSet)
    val parser = new Parser(source, analysis, featureSet)
    parser.parse()
  }
}

private[parser] final class Parser(
    private[this] val source: String,
    private[this] val analysis: QuickAnalysis,
    private[this] val featureSet: FeatureSet
) {
  private[this] var offset: Int = 0

  private[this] var currentChar: Char =
    if (source.isEmpty) '\u0000' else source.charAt(offset)

  private[this] var context: ParsingContext = ParsingContext.from(featureSet)

  def parse(): Node = {
    val node = parseDisjunction()
    if (!isEnd) {
      assert(currentChar == ')', "Unexpected")
      fail("Unmatched ')'")
    }
    node
  }

  private def parseDisjunction(): Node = {
    val start = offset
    val builder = Seq.newBuilder[Node]
    builder.addOne(parseSequence())

    while (!isEnd && currentChar == '|') {
      next()
      builder.addOne(parseSequence())
    }

    val nodes = builder.result()
    if (nodes.size == 1) {
      return nodes.head
    }

    val end = offset
    Node(NodeData.Disjunction(nodes), SourceLocation(start, end))
  }

  private def parseSequence(): Node = {
    val start = offset
    val stack = mutable.Stack.empty[Node]

    while (!isSequenceStop) {
      parseSequenceItem(stack)
    }

    val nodes = stack.toSeq
    if (nodes.size == 1) {
      return nodes.head
    }

    val end = offset
    Node(NodeData.Sequence(nodes.reverse), SourceLocation(start, end))
  }

  private def parseSequenceItem(stack: mutable.Stack[Node]): Unit = {
    if (context.skipsComment && skipComment()) {
      return
    }

    (currentChar: @switch) match {
      case '*' =>
        parseSimpleRepeat(stack, Quantifier.Star)
      case '+' =>
        parseSimpleRepeat(stack, Quantifier.Plus)
      case '?' =>
        parseSimpleRepeat(stack, Quantifier.Question)
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
    }
  }

  private def parseSimpleRepeat(stack: mutable.Stack[Node], buildQuantifier: BacktrackStrategy => Quantifier): Unit = {
    val node = popRepeatable(stack)
    next()
    val strategy = parseBacktrackStrategy(exact = false)
    val quantifier = buildQuantifier(strategy)
    pushRepeat(stack, node, quantifier)
  }

  private def parseCurlyQuantifier(stack: mutable.Stack[Node]): Unit = {
    val save = offset
    next()
    parseIntOption() match {
      case Some(min) =>
        (currentChar: @switch) match {
          case '}' =>
            next()
            val strategy = parseBacktrackStrategy(exact = true)
            val quantifier = Quantifier.Exact(min, strategy)
            pushRepeat(stack, popRepeatable(stack), quantifier)
          case ',' =>
            next()
            (currentChar: @switch) match {
              case '}' =>
                next()
                val strategy = parseBacktrackStrategy(exact = false)
                val quantifier = Quantifier.Unbounded(min, strategy)
                pushRepeat(stack, popRepeatable(stack), quantifier)
              case _ =>
                parseIntOption() match {
                  case Some(max) =>
                    if (currentChar != '}') {
                      resetBracket(stack, save)
                      return
                    }
                    next()
                    val strategy = parseBacktrackStrategy(exact = false)
                    val quantifier = Quantifier.Bounded(min, max, strategy)
                    pushRepeat(stack, popRepeatable(stack), quantifier)
                  case None =>
                    resetBracket(stack, save)
                }
            }
          case _ =>
            resetBracket(stack, save)
        }
      case None =>
        if (!featureSet.hasMaxBounded || currentChar != ',') {
          resetBracket(stack, save)
          return
        }
        next()
        parseIntOption() match {
          case Some(max) =>
            if (currentChar != '}') {
              resetBracket(stack, save)
            }
            next()
            val strategy = parseBacktrackStrategy(exact = false)
            val quantifier = Quantifier.MaxBounded(max, strategy)
            pushRepeat(stack, popRepeatable(stack), quantifier)
          case None =>
            resetBracket(stack, save)
        }
    }
  }

  private def popRepeatable(stack: mutable.Stack[Node]): Node = {
    assert(stack.nonEmpty, "Nothing to repeat")
    val node = stack.pop()
    assertRepeatable(node)
    node
  }

  private def assertRepeatable(node: Node): Unit =
    node.data match {
      case NodeData.Group(_: GroupKind.PositiveLookAhead | _: GroupKind.NegativeLookAhead, _) =>
        assert(featureSet.allowsLookAheadRepeat, "Nothing to repeat")
      case NodeData.Group(_: GroupKind.PositiveLookBehind | _: GroupKind.NegativeLookBehind, _) =>
        assert(featureSet.allowsLookBehindRepeat, "Nothing to repeat")
      case NodeData.Caret | NodeData.Dollar =>
        assert(featureSet.allowsZeroWidthAssertRepeat, "Nothing to repeat")
      case NodeData.Repeat(_, _) =>
        assert(featureSet.allowsNestedRepeat, "Nested quantifier")
      case _ =>
        // TODO: backslash assertion
        ()
    }

  private def pushRepeat(stack: mutable.Stack[Node], node: Node, quantifier: Quantifier): Unit = {
    val end = offset
    stack.push(Node(NodeData.Repeat(node, quantifier), node.loc.copy(end = end)))
  }

  private def parseBacktrackStrategy(exact: Boolean): BacktrackStrategy = {
    if (isEnd || exact && !featureSet.allowsMeaninglessBacktrackStrategy) {
      return BacktrackStrategy.Greedy
    }

    if (currentChar == '?') {
      next()
      return BacktrackStrategy.Lazy
    }

    if (featureSet.hasPossessiveBacktrackStrategy && currentChar == '+') {
      next()
      return BacktrackStrategy.Possessive
    }

    BacktrackStrategy.Greedy
  }

  private def resetBracket(stack: mutable.Stack[Node], save: Int): Unit = {
    if (!featureSet.allowsBrokenCurly) {
      fail("Incomplete quantifier", save)
    }
    reset(save)
    stack.push(Node(NodeData.Literal('{'), SourceLocation(save, save + 1)))
    next()
  }

  private def parseGroup(stack: mutable.Stack[Node]): Unit = {
    val start = offset
    next()
    (currentChar: @switch) match {
      case '?' =>
        parseExtendedGroup(stack, start)
      case '*' =>
        parseAlphabeticGroup(stack, start)
      case _ =>
        parseGroupBody(stack, GroupKind.IndexedCapture, start)
    }
  }

  private def parseExtendedGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    (currentChar: @switch) match {
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
    }
  }

  private def parseNonCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.NonCapture, start)
  }

  private def parseBranchResetGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasBranchReset, "Invalid group")
    next()
    protectContext {
      val node = parseDisjunction()
      closeGroup()
      val nodes = node.data match {
        case NodeData.Disjunction(nodes) => nodes
        case _                           => Seq(node)
      }
      stack.push(Node(NodeData.Command(CommandKind.BranchReset(nodes)), SourceLocation(start, offset)))
    }
  }

  private def parsePositiveLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.PositiveLookAhead(AssertNameStyle.Symbolic), start)
  }

  private def parseNegativeLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), start)
  }

  private def parseAngleGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    (currentChar: @switch) match {
      case '=' =>
        parsePositiveLookBehindGroup(stack, start)
      case '!' =>
        parseNegativeLookBehindGroup(stack, start)
      case '*' =>
        parseNonAtomicPositiveLookBehindGroup(stack, start)
      case _ =>
        parseAngleNamedCaptureGroup(stack, start)
    }
  }

  private def parsePositiveLookBehindGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.PositiveLookBehind(AssertNameStyle.Symbolic), start)
  }

  private def parseNegativeLookBehindGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.NegativeLookBehind(AssertNameStyle.Symbolic), start)
  }

  private def parseNonAtomicPositiveLookBehindGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasNonAtomicLookAround, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.NonAtomicPositiveLookBehind(AssertNameStyle.Symbolic), start)
  }

  private def parseAtomicGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasAtomicGroup, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.Atomic(AssertNameStyle.Symbolic), start)
  }

  private def parseNonAtomicPositiveLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasNonAtomicLookAround, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.NonAtomicPositiveLookAhead(AssertNameStyle.Symbolic), start)
  }

  private def parseAbsenceGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasAbsenceOperator, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.Absence, start)
  }

  private[this] val LookAroundTests = Seq(
    ("?=", "*positive_lookahead:", "*pla:", GroupKind.PositiveLookAhead),
    ("?!", "*negative_lookahead:", "*nla:", GroupKind.NegativeLookAhead),
    ("?<=", "*positive_lookbehind:", "*plb:", GroupKind.PositiveLookBehind),
    ("?<!", "*negative_lookbehind:", "*nlb:", GroupKind.NegativeLookBehind)
  )

  private def parseConditionalGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasConditional, "Invalid group")
    next()

    for (n <- parseIntOption()) {
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Indexed(n), start)
      return
    }

    if (featureSet.hasNamedCaptureTest && currentChar == '<') {
      next()
      val name = parseName(end = '>')
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Angle, name), start)
      return
    }

    if (featureSet.hasNamedCaptureTest && currentChar == '\'') {
      next()
      val name = parseName(end = '\'')
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Quote, name), start)
      return
    }

    if (featureSet.hasDefineTest && startWith("DEFINE")) {
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Define, start)
      return
    }

    if (featureSet.hasVersionTest && startWith("VERSION")) {
      val lt = !isEnd && currentChar == '>'
      if (lt) {
        next()
      }
      expect('=', "Invalid condition")
      val major = parseIntOption().getOrElse(fail("Invalid condition"))
      expect('.', "Invalid condition")
      val minor = parseIntOption().getOrElse(fail("Invalid condition"))
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Version(lt, major, minor), start)
      return
    }

    if (currentChar == 'R' && featureSet.hasRecursionTest) {
      next()
      (currentChar: @switch) match {
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
      }
      return
    }

    if (featureSet.hasBareNamedCaptureTest && isIDStart) {
      val name = parseID()
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Bare, name), start)
      return
    }

    val isRelative = !isEnd && (currentChar == '+' || currentChar == '-')
    if (featureSet.hasRelativeIndexedCaptureTest && isRelative) {
      val minus = currentChar == '-'
      next()
      parseIntOption() match {
        case Some(n) =>
          closeCondition()
          parseConditionalGroupBody(stack, ConditionalTest.Relative(if (minus) -n else n), start)
          return
        case None =>
          fail("Invalid condition")
      }
    }

    if (featureSet.hasLookAroundTest) {
      for ((symbolic, alphabetic, abbrev, la) <- LookAroundTests) {
        if (parseLookAroundTest(stack, start, symbolic, alphabetic, abbrev, la)) {
          return
        }
      }

      parseLookAroundConditional(stack, None, start)
      return
    }

    fail("Invalid condition")
  }

  private def closeCondition(): Unit = {
    expect(')', "Invalid condition")
  }

  private def parseConditionalGroupBody(stack: mutable.Stack[Node], test: ConditionalTest, start: Int): Unit = {
    protectContext {
      val yes = protectContext(parseSequence())
      val no = Option.when(!isEnd && currentChar == '|') {
        next()
        protectContext(parseSequence())
      }
      closeGroup()
      val end = offset
      stack.push(Node(NodeData.Command(CommandKind.Conditional(test, yes, no)), SourceLocation(start, end)))
    }
  }

  private def parseLookAroundTest(
      stack: mutable.Stack[Node],
      start: Int,
      symbolic: String,
      alphabetic: String,
      abbrev: String,
      la: AssertNameStyle => GroupKind.LookAround
  ): Boolean = {
    if (startWith(symbolic)) {
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Symbolic)), start)
      return true
    }

    if (featureSet.hasAlphabeticGroup && startWith(alphabetic)) {
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Alphabetic)), start)
      return true
    }

    if (featureSet.hasAlphabeticGroup && startWith(abbrev)) {
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Abbrev)), start)
      return true
    }

    false
  }

  private def parseLookAroundConditional(
      stack: mutable.Stack[Node],
      kind: Option[GroupKind.LookAround],
      start: Int
  ): Unit = {
    val node = protectContext(parseDisjunction())
    closeCondition()
    parseConditionalGroupBody(stack, ConditionalTest.LookAround(kind, node), start)
  }

  private def parseAngleNamedCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasAngleNamedCapture, "Invalid group")
    parseNamedCaptureGroup(stack, '>', NameStyle.Angle, start)
  }

  private def parseQuoteNamedCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasQuoteNamedCapture, "Invalid group")
    next()
    parseNamedCaptureGroup(stack, '\'', NameStyle.Quote, start)
  }

  private def parseNamedCaptureGroup(stack: mutable.Stack[Node], end: Char, style: NameStyle, start: Int): Unit = {
    if (featureSet.hasBalanceGroup) {
      parseBalanceGroup(stack, end, style, start)
      return
    }

    val name = parseName(end = end)
    parseGroupBody(stack, GroupKind.NamedCapture(style, name), start)
  }

  private def parseBalanceGroup(stack: mutable.Stack[Node], end: Char, style: NameStyle, start: Int): Unit = {
    if (currentChar == '-') {
      next()
      val test = parseName(end = end)
      parseGroupBody(stack, GroupKind.Balance(style, None, test), start)
      return
    }

    val name = parseID()
    if (currentChar == end) {
      next()
      parseGroupBody(stack, GroupKind.NamedCapture(style, name), start)
      return
    }

    if (currentChar == '-') {
      next()
      val test = parseName(end = end)
      parseGroupBody(stack, GroupKind.Balance(style, Some(name), test), start)
      return
    }

    fail("Invalid identifier")
  }

  private def parsePGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasPGroup, "Invalid group")
    next()
    (currentChar: @switch) match {
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
    }
  }

  private def parseRGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasRCall, "Invalid group")
    next()
    pushCommand(stack, CommandKind.RCall, start)
  }

  private def parseNamedCallGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasCallCommand, "Invalid group")
    next()
    val name = parseID()
    pushCommand(stack, CommandKind.NamedCall(name), start)
  }

  private def parseIndexedCallGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasCallCommand, "Invalid group")
    val n = parseIntOption().getOrElse(fail("Invalid group"))
    pushCommand(stack, CommandKind.IndexedCall(n), start)
  }

  private def parsePlusGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasCallCommand, "Invalid group")
    next()
    val n = parseIntOption().getOrElse(fail("Invalid group"))
    pushCommand(stack, CommandKind.RelativeCall(n), start)
  }

  private def parseMinusGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseIntOption() match {
      case Some(n) =>
        assert(featureSet.hasCallCommand, "Invalid group")
        pushCommand(stack, CommandKind.RelativeCall(-n), start)
      case None =>
        assert(featureSet.hasInlineFlag, "Invalid group")
        val added = FlagSet()
        val removed = parseFlagSet()
        val diff = FlagSetDiff(added, Some(removed))
        (currentChar: @switch) match {
          case ')' =>
            pushCommand(stack, CommandKind.InlineFlag(FlagSetDiff(added, Some(removed))), start)
            applyFlagSetDiff(diff)
          case ':' =>
            next()
            applyFlagSetDiffWith(diff) {
              parseGroupBody(stack, GroupKind.InlineFlag(FlagSetDiff(added, Some(removed))), start)
            }
          case _ =>
            fail("Invalid group")
        }
    }
  }

  private def parseResetFlagGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasResetFlag, "Invalid group")
    next()
    val flagSet = parseFlagSet()
    (currentChar: @switch) match {
      case ')' =>
        pushCommand(stack, CommandKind.ResetFlag(flagSet), start)
        applyResetFlag(flagSet)
      case ':' =>
        next()
        applyResetFlagWith(flagSet) {
          parseGroupBody(stack, GroupKind.ResetFlag(flagSet), start)
        }
      case _ =>
        fail("Invalid group")
    }
  }

  private def parseCommentGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasInlineComment, "Invalid group")
    next()
    val textStart = offset
    while (!isEnd && currentChar != ')') {
      if (featureSet.processBackslashInCommentGroup && currentChar == '\\') {
        next()
        if (!isEnd) {
          next()
        }
      } else {
        next()
      }
    }
    assert(!isEnd, "Invalid group")
    val end = offset
    val text = source.slice(textStart, end)
    pushCommand(stack, CommandKind.Comment(text), start)
  }

  private def parseInlineCodeGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasInlineCode, "Invalid group")
    next()
    val code = parseCode(end = '}')
    pushCommand(stack, CommandKind.InlineCode(code), start)
  }

  private def parseEmbedCodeGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasInlineCode, "Invalid group")
    next()
    expect('{', "Invalid group")
    val code = parseCode(end = '}')
    pushCommand(stack, CommandKind.EmbedCode(code), start)
  }

  private def parseCalloutGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasCallout, "Invalid group")
    next()
    (currentChar: @switch) match {
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
        parseIntOption() match {
          case Some(value) =>
            pushCommand(stack, CommandKind.CalloutInt(value), start)
          case None =>
            pushCommand(stack, CommandKind.Callout, start)
        }
    }
  }

  private def parseCode(end: Char, escapeDouble: Boolean = false): String = {
    val start = offset
    while (
      !isEnd &&
      !(currentChar == end && !(escapeDouble && offset + 1 < source.length && source.charAt(offset + 1) == end))
    ) {
      if (currentChar == end && escapeDouble && offset + 1 < source.length && source.charAt(offset + 1) == end) {
        next()
      }
      next()
    }
    assert(!isEnd, "Invalid group")
    val code = source.slice(start, offset)
    next()
    code
  }

  private def parseInlineFlagGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasInlineFlag, "Invalid group")

    val added = parseFlagSet()

    (currentChar: @switch) match {
      case '-' =>
        next()
        val removed = parseFlagSet()
        val diff = FlagSetDiff(added, Some(removed))
        (currentChar: @switch) match {
          case ':' =>
            next()
            applyFlagSetDiffWith(diff) {
              parseGroupBody(stack, GroupKind.InlineFlag(diff), start)
            }
          case ')' =>
            pushInlineFlagCommand(stack, diff, start)
          case _ =>
            fail("Invalid flag")
        }
      case ':' =>
        next()
        val diff = FlagSetDiff(added, None)
        applyFlagSetDiffWith(diff) {
          parseGroupBody(stack, GroupKind.InlineFlag(diff), start)
        }
      case ')' =>
        val diff = FlagSetDiff(added, None)
        pushInlineFlagCommand(stack, diff, start)
      case _ =>
        fail("Invalid flag")
    }
  }

  private def pushInlineFlagCommand(stack: mutable.Stack[Node], diff: FlagSetDiff, start: Int): Unit = {
    pushCommand(stack, CommandKind.InlineFlag(diff), start)
    if (featureSet.hasIncomprehensiveInlineFlag) {
      parseOpaqueDisjunction(stack, diff)
    } else {
      applyFlagSetDiff(diff)
    }
  }

  private def parseOpaqueDisjunction(stack: mutable.Stack[Node], diff: FlagSetDiff): Unit = {
    applyFlagSetDiffWith(diff) {
      val node = parseDisjunction()
      node.data match {
        case s: NodeData.Sequence =>
          if (s.nodes.isEmpty) {
            return
          }
        case _ => ()
      }
      stack.push(node)
    }
  }

  private[this] val BacktrackControls = Seq(
    ("ACCEPT", BacktrackControlKind.Accept),
    ("FAIL", BacktrackControlKind.Fail),
    ("MARK", BacktrackControlKind.Mark),
    ("COMMIT", BacktrackControlKind.Commit),
    ("PRUNE", BacktrackControlKind.Prune),
    ("SKIP", BacktrackControlKind.Skip),
    ("THEN", BacktrackControlKind.Then)
  )

  private[this] val AlphabeticGroups = Seq(
    ("positive_lookahead:", "pla:", GroupKind.PositiveLookAhead),
    ("negative_lookahead:", "nla:", GroupKind.NegativeLookAhead),
    ("positive_lookbehind:", "plb:", GroupKind.PositiveLookBehind),
    ("negative_lookbehind:", "nlb:", GroupKind.NegativeLookBehind),
    ("atomic:", "", GroupKind.Atomic),
    ("script_run:", "sr:", GroupKind.ScriptRun),
    ("atomic_script_run:", "asr:", GroupKind.AtomicScriptRun)
  ) ++ {
    if (featureSet.hasNonAtomicLookAround) {
      Seq(
        ("non_atomic_positive_lookahead:", "napla:", GroupKind.NonAtomicPositiveLookAhead),
        ("non_atomic_positive_lookbehind:", "naplb:", GroupKind.NonAtomicPositiveLookBehind)
      )
    } else Seq.empty
  }

  private def parseAlphabeticGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    assert(featureSet.hasAlphabeticGroup, "Invalid group")

    next()

    for ((command, kind) <- BacktrackControls) {
      if (parseBacktrackControl(stack, start, command, kind)) {
        return
      }
    }

    for ((alphabetic, abbrev, kind) <- AlphabeticGroups) {
      if (parseAlphabetic(stack, start, alphabetic, abbrev, kind)) {
        return
      }
    }

    expect(':', "Invalid group")
    val name = parseID()
    pushCommand(stack, CommandKind.BacktrackControl(None, Some(name)), start)
  }

  private def parseBacktrackControl(
      stack: mutable.Stack[Node],
      start: Int,
      command: String,
      kind: BacktrackControlKind
  ): Boolean = {
    if (!startWith(command)) {
      return false
    }

    (currentChar: @switch) match {
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
    }
  }

  private def parseAlphabetic(
      stack: mutable.Stack[Node],
      start: Int,
      alphabetic: String,
      abbrev: String,
      k: AssertNameStyle => GroupKind
  ): Boolean = {
    if (startWith(alphabetic)) {
      parseGroupBody(stack, k(AssertNameStyle.Alphabetic), start)
      return true
    }

    if (abbrev.nonEmpty && startWith(abbrev)) {
      parseGroupBody(stack, k(AssertNameStyle.Abbrev), start)
      return true
    }

    false
  }

  private def parseGroupBody(stack: mutable.Stack[Node], kind: GroupKind, start: Int): Unit = {
    val node = protectContext(parseDisjunction())
    closeGroup()
    val end = offset
    stack.push(Node(NodeData.Group(kind, node), SourceLocation(start, end)))
  }

  private def pushCommand(stack: mutable.Stack[Node], kind: CommandKind, start: Int): Unit = {
    closeGroup()
    val end = offset
    stack.push(Node(NodeData.Command(kind), SourceLocation(start, end)))
  }

  private def closeGroup(): Unit = {
    expect(')', "Unclosed group")
  }

  private def parseFlagSet(): FlagSet = {
    var flagSet = FlagSet()
    while (!isEnd) {
      (currentChar: @switch) match {
        case 'i' =>
          flagSet = flagSet.copy(ignoreCase = true)
        case 'm' =>
          flagSet = flagSet.copy(multiline = true)
        case 'x' =>
          flagSet = flagSet.copy(verbose = true)
        case 'J' =>
          if (!featureSet.hasFlagUpperJ) {
            return flagSet
          }
          flagSet = flagSet.copy(dupNames = true)
        case 'L' =>
          if (!featureSet.hasFlagUpperL) {
            return flagSet
          }
          flagSet = flagSet.copy(localeUpper = true)
        case 'U' =>
          if (!featureSet.hasFlagUpperU) {
            return flagSet
          }
          flagSet = flagSet.copy(ungreedy = true)
        case 'a' =>
          if (!featureSet.hasFlagA) {
            return flagSet
          }
          flagSet = flagSet.copy(ascii = true)
        case 'd' =>
          if (!featureSet.hasFlagD) {
            return flagSet
          }
          flagSet = flagSet.copy(hasIndices = true)
        case 'l' =>
          if (!featureSet.hasFlagL) {
            return flagSet
          }
          flagSet = flagSet.copy(localeLower = true)
        case 'n' =>
          if (!featureSet.hasFlagN) {
            return flagSet
          }
          flagSet = flagSet.copy(explicitCapture = true)
        case 'p' =>
          if (!featureSet.hasFlagP) {
            return flagSet
          }
          flagSet = flagSet.copy(preserve = true)
        case 's' =>
          if (!featureSet.hasFlagS) {
            return flagSet
          }
          flagSet = flagSet.copy(dotAll = true)
        case 'u' =>
          if (!featureSet.hasFlagU) {
            return flagSet
          }
          flagSet = flagSet.copy(unicode = true)
        case _ =>
          return flagSet
      }
      next()
    }
    flagSet
  }

  private def parseCaret(stack: mutable.Stack[Node]): Unit = {
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Caret, SourceLocation(start, end)))
  }

  private def parseDollar(stack: mutable.Stack[Node]): Unit = {
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Dollar, SourceLocation(start, end)))
  }

  private def parseDot(stack: mutable.Stack[Node]): Unit = {
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Dot, SourceLocation(start, end)))
  }

  private def parseBackslash(stack: mutable.Stack[Node]): Unit = {
    val start = offset
    next()
    val backslash = parseBackslashKind()
    val end = offset
    stack.push(Node(NodeData.Backslash(backslash), SourceLocation(start, end)))

    if (backslash != BackslashKind.CaseCommand(CaseCommandKind.QuoteCommand)) {
      return
    }

    while (!isEnd && !source.startsWith("\\E", offset)) {
      parseLiteral(stack)
    }

    val eStart = offset
    if (startWith("\\E")) {
      val backslash = NodeData.Backslash(BackslashKind.CaseCommand(CaseCommandKind.EndCaseCommand))
      val eEnd = offset
      stack.push(Node(backslash, SourceLocation(eStart, eEnd)))
    }
  }

  private def parseBackslashKind(): BackslashKind =
    (currentChar: @switch) match {
      case 'a' => parseBackslashLowerA()
      case 'b' => parseBackslashLowerB()
      case 'c' => parseBackslashLowerC()
      case 'd' => parseBackslashLowerD()
      case 'e' => parseBackslashLowerE()
      case 'f' => parseBackslashLowerF()
      case 'g' => parseBackslashLowerG()
      case 'h' => parseBackslashLowerH()
      case 'k' => parseBackslashLowerK()
      case 'l' => parseBackslashLowerL()
      case 'n' => parseBackslashLowerN()
      case 'o' => parseBackslashLowerO()
      case 'p' => parseBackslashLowerP()
      case 'q' => parseBackslashLowerQ()
      case 'r' => parseBackslashLowerR()
      case 's' => parseBackslashLowerS()
      case 't' => parseBackslashLowerT()
      case 'u' => parseBackslashLowerU()
      case 'v' => parseBackslashLowerV()
      case 'w' => parseBackslashLowerW()
      case 'x' => parseBackslashLowerX()
      case 'z' => parseBackslashLowerZ()
      case 'A' => parseBackslashUpperA()
      case 'B' => parseBackslashUpperB()
      case 'D' => parseBackslashUpperD()
      case 'E' => parseBackslashUpperE()
      case 'F' => parseBackslashUpperF()
      case 'G' => parseBackslashUpperG()
      case 'H' => parseBackslashUpperH()
      case 'K' => parseBackslashUpperK()
      case 'L' => parseBackslashUpperL()
      case 'N' => parseBackslashUpperN()
      case 'P' => parseBackslashUpperP()
      case 'Q' => parseBackslashUpperQ()
      case 'R' => parseBackslashUpperR()
      case 'S' => parseBackslashUpperS()
      case 'U' => parseBackslashUpperU()
      case 'V' => parseBackslashUpperV()
      case 'W' => parseBackslashUpperW()
      case 'X' => parseBackslashUpperX()
      case 'Z' => parseBackslashUpperZ()
      case '0' => parseBackslashOctal()
      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        parseBackslashBackReference()
      case '/' | '|' | '+' | '*' | '?' | '{' | '}' | '(' | ')' | '^' | '$' | '.' | '\\' | '[' | ']' | '&' =>
        parseBackslashEscape()
      case _ => parseBackslashUnknown()
    }

  private def parseBackslashLowerA(): BackslashKind = {
    if (!featureSet.hasBackslashBell) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.Escape(EscapeStyle.Single('a'), 0x0007)
  }

  private def parseBackslashLowerB(): BackslashKind = {
    if (context.inClass) {
      return parseBackslashBackslash()
    }

    parseBackslashBoundary()
  }

  private def parseBackslashBackslash(): BackslashKind = {
    next()
    BackslashKind.Escape(EscapeStyle.Single('b'), 0x0008)
  }

  private[this] val BoundaryModifiers = Seq(
    ("{gcb}", BoundaryModifier.GcbModifier),
    ("{lb}", BoundaryModifier.LbModifier),
    ("{sb}", BoundaryModifier.SbModifier),
    ("{wb}", BoundaryModifier.WbModifier)
  )

  private def parseBackslashBoundary(): BackslashKind = {
    next()

    if (featureSet.hasBoundaryGModifier && startWith("{g}")) {
      return BackslashKind.Assert(AssertKind.Boundary(Some(BoundaryModifier.GModifier)))
    }

    if (featureSet.hasBoundaryModifier) {
      for ((name, modifier) <- BoundaryModifiers) {
        if (startWith(name)) {
          return BackslashKind.Assert(AssertKind.Boundary(Some(modifier)))
        }
      }
    }

    BackslashKind.Assert(AssertKind.Boundary(None))
  }

  private def parseBackslashLowerC(): BackslashKind = {
    if (!featureSet.hasBackslashControl) {
      return parseBackslashUnknown()
    }

    next()
    if (!isControl) {
      assert(featureSet.allowsAlphabeticUnknownBackslash, "Invalid backslash escape")
      return BackslashKind.Unknown('c')
    }

    val char = currentChar
    next()

    val value = char.toInt % 32
    BackslashKind.Escape(EscapeStyle.Control(char), value)
  }

  private def parseBackslashLowerD(): BackslashKind = {
    next()
    BackslashKind.EscapeClass(EscapeClassKind.Digit)
  }

  private def parseBackslashLowerE(): BackslashKind = {
    if (!featureSet.hasBackslashEscape) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.Escape(EscapeStyle.Single('e'), 0x1b)
  }

  private def parseBackslashLowerF(): BackslashKind = {
    next()
    BackslashKind.Escape(EscapeStyle.Single('f'), 0x0c)
  }

  private def parseBackslashLowerG(): BackslashKind = {
    if (!(featureSet.hasBackslashGBackReference || featureSet.hasBackslashGCall)) {
      return parseBackslashUnknown()
    }

    next()
    (currentChar: @switch) match {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        assert(featureSet.hasBackslashGBackReference, "Invalid backslash escape")
        val index = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        BackslashKind.EscapeBackReference(
          BackReferenceStyle.GBackReference(NameStyle.Bare),
          Reference.IndexedReference(index)
        )
      case '+' | '-' =>
        assert(featureSet.hasBackslashGBackReference, "Invalid backslash escape")
        val sign = if (currentChar == '-') -1 else 1
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
        val style = if (currentChar == '<') NameStyle.Angle else NameStyle.Quote
        next()
        val ref = parseBaseRef()
        expect(if (style == NameStyle.Angle) '>' else '\'', "Invalid backslash escape")
        BackslashKind.EscapeCall(style, ref)
      case _ =>
        fail("Invalid backslash escape")
    }
  }

  private def parseBackslashLowerH(): BackslashKind = {
    if (!(featureSet.hasBackslashHorizontalSpace || featureSet.hasBackslashHexDigit)) {
      return parseBackslashUnknown()
    }

    next()
    val klass = if (featureSet.hasBackslashHorizontalSpace) EscapeClassKind.Horizontal else EscapeClassKind.HexDigit
    BackslashKind.EscapeClass(klass)
  }

  private def parseBackslashLowerK(): BackslashKind = {
    if (!featureSet.hasBackslashKBackReference || context.inClass) {
      return parseBackslashUnknown()
    }

    next()
    (currentChar: @switch) match {
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
    }
  }

  private def parseRef(): Reference = {
    val ref = parseBaseRef()

    if (!featureSet.hasLeveledBackReference) {
      return ref
    }

    (currentChar: @switch) match {
      case '+' | '-' =>
        val sign = if (currentChar == '-') -1 else 1
        next()
        val level = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        Reference.LeveledReference(ref, sign * level)
      case _ => ref
    }
  }

  private def parseBaseRef(): Reference.BaseReference = {
    if (!featureSet.allowsNonNameBaseRef) {
      val name = parseID()
      return Reference.NamedReference(name)
    }

    (currentChar: @switch) match {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        val index = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        Reference.IndexedReference(index)
      case '+' | '-' =>
        val sign = if (currentChar == '-') -1 else 1
        next()
        val n = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        Reference.RelativeReference(sign * n)
      case _ =>
        val name = parseID()
        Reference.NamedReference(name)
    }
  }

  private def parseBackslashLowerL(): BackslashKind = {
    if (!featureSet.hasBackslashCaseCommand) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.CaseCommand(CaseCommandKind.SingleLowerCaseCommand)
  }

  private def parseBackslashLowerN(): BackslashKind = {
    next()
    BackslashKind.Escape(EscapeStyle.Single('n'), 0x0a)
  }

  private def parseBackslashLowerO(): BackslashKind = {
    if (!featureSet.hasBackslashOctal) {
      return parseBackslashUnknown()
    }

    next()
    expect('{', "Invalid backslash escape")
    val value = parseOctalIntOption().getOrElse(fail("Invalid backslash escape"))
    expect('}', "Invalid backslash escape")

    BackslashKind.Escape(EscapeStyle.Octal, value)
  }

  private def parseBackslashLowerP(): BackslashKind = {
    if (!featureSet.hasBackslashUnicodeProperty) {
      return parseBackslashUnknown()
    }

    next()
    (currentChar: @switch) match {
      case '{' =>
        next()
        val name = parseID()
        val klass = if (currentChar == '=') {
          next()
          val value = parseID()
          EscapeClassKind.UnicodePropertyValue(name, value)
        } else {
          EscapeClassKind.UnicodeProperty(name)
        }
        expect('}', "Invalid backslash escape")
        BackslashKind.EscapeClass(klass)
      case _ =>
        assert(featureSet.hasBackslashUnicodePropertyBare, "Invalid backslash escape")
        assert(!isEnd, "Invalid backslash escape")
        val name = currentChar.toString
        next()
        BackslashKind.EscapeClass(EscapeClassKind.UnicodeBareProperty(name))
    }
  }

  private def parseBackslashLowerQ(): BackslashKind = {
    if (!(featureSet.hasBackslashQuoteSet && context.inClass)) {
      return parseBackslashUnknown()
    }

    next()
    expect('{', "Invalid backslash escape")
    val strings = Seq.newBuilder[Seq[QuoteLiteral]]
    strings.addOne(parseQuoteString())
    while (!isEnd && currentChar == '|') {
      next()
      strings.addOne(parseQuoteString())
    }
    expect('}', "Invalid backslash escape")

    BackslashKind.EscapeClass(EscapeClassKind.QuoteSet(strings.result()))
  }

  private def parseQuoteString(): Seq[QuoteLiteral] = {
    val string = Seq.newBuilder[QuoteLiteral]

    while (!(isEnd || currentChar == '|' || currentChar == '}')) {
      if (currentChar == '\\') {
        val save = offset
        next()
        val backslash = parseBackslashKind()
        backslash match {
          case kind: BackslashKind.BackslashValue =>
            string.addOne(QuoteLiteral.QuoteBackslash(kind))
          case _ =>
            fail("Invalid backslash escape", save)
        }
      } else {
        val value = if (featureSet.readsAsUnicode) currentCodePoint else currentChar
        nextCodePoint(value)
        string.addOne(QuoteLiteral.QuoteValue(value))
      }
    }

    string.result()
  }

  private def parseBackslashLowerR(): BackslashKind = {
    next()
    BackslashKind.Escape(EscapeStyle.Single('r'), 0x0d)
  }

  private def parseBackslashLowerS(): BackslashKind = {
    next()
    BackslashKind.EscapeClass(EscapeClassKind.Space)
  }

  private def parseBackslashLowerT(): BackslashKind = {
    next()
    BackslashKind.Escape(EscapeStyle.Single('t'), 0x09)
  }

  private def parseBackslashLowerU(): BackslashKind = {
    if (!(featureSet.hasBackslashUnicodeHex || featureSet.hasBackslashCaseCommand)) {
      return parseBackslashUnknown()
    }

    val save = offset
    next()

    if (featureSet.hasBackslashUnicodeBracket && currentChar == '{') {
      next()
      val value = parseHexIntOption().getOrElse(fail("Invalid backslash escape"))
      expect('}', "Invalid backslash escape")
      return BackslashKind.Escape(EscapeStyle.UnicodeBracket, value)
    }

    if (featureSet.hasBackslashCaseCommand) {
      return BackslashKind.CaseCommand(CaseCommandKind.SingleUpperCaseCommand)
    }

    for (_ <- 1 to 4) {
      if (!isHexDigit) {
        reset(save)
        return parseBackslashUnknown()
      }
      next()
    }

    val str = source.slice(offset - 4, offset)
    val value = Try(Integer.parseInt(str, 16)).toOption.getOrElse(fail("Invalid integer"))
    BackslashKind.Escape(EscapeStyle.UnicodeHex4, value)
  }

  private def parseBackslashLowerV(): BackslashKind = {
    if (!(featureSet.hasBackslashVerticalSpace || featureSet.hasBackslashVerticalTab)) {
      return parseBackslashUnknown()
    }

    next()

    if (featureSet.hasBackslashVerticalSpace) {
      return BackslashKind.EscapeClass(EscapeClassKind.Vertical)
    }

    BackslashKind.Escape(EscapeStyle.Single('v'), 0x0b)
  }

  private def parseBackslashLowerW(): BackslashKind = {
    next()
    BackslashKind.EscapeClass(EscapeClassKind.Word)
  }

  private def parseBackslashLowerX(): BackslashKind = {
    val save = offset
    next()

    if (featureSet.hasBackslashXBracket && currentChar == '{') {
      next()
      val value = parseHexIntOption().getOrElse(fail("Invalid backslash escape"))
      expect('}', "Invalid backslash escape")
      return BackslashKind.Escape(EscapeStyle.HexBracket, value)
    }

    if (!isHexDigit) {
      reset(save)
      return parseBackslashUnknown()
    }

    next()

    if (featureSet.hasBackslashXHex1 && !isHexDigit) {
      val str = source.slice(offset - 1, offset)
      val value = Try(Integer.parseInt(str, 16)).getOrElse(fail("Invalid backslash escape"))
      return BackslashKind.Escape(EscapeStyle.Hex1, value)
    }

    if (!isHexDigit) {
      reset(save)
      return parseBackslashUnknown()
    }

    next()
    val str = source.slice(offset - 2, offset)
    val value = Try(Integer.parseInt(str, 16)).getOrElse(fail("Invalid backslash escape"))
    BackslashKind.Escape(EscapeStyle.Hex2, value)
  }

  private def parseBackslashLowerZ(): BackslashKind = {
    if (!featureSet.hasBackslashEnd) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.Assert(AssertKind.LowerEnd)
  }

  private def parseBackslashUpperA(): BackslashKind = {
    if (!featureSet.hasBackslashBegin) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.Assert(AssertKind.Begin)
  }

  private def parseBackslashUpperB(): BackslashKind = {
    if (context.inClass) {
      return parseBackslashUnknown()
    }

    next()

    if (featureSet.hasBoundaryModifier) {
      if (startWith("{g}")) {
        return BackslashKind.Assert(AssertKind.NonBoundary(Some(BoundaryModifier.GModifier)))
      }

      for ((name, modifier) <- BoundaryModifiers) {
        if (startWith(name)) {
          return BackslashKind.Assert(AssertKind.NonBoundary(Some(modifier)))
        }
      }
    }

    BackslashKind.Assert(AssertKind.NonBoundary(None))
  }

  private def parseBackslashUpperD(): BackslashKind = {
    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonDigit)
  }

  private def parseBackslashUpperE(): BackslashKind = {
    if (!(featureSet.hasBackslashCaseCommand || featureSet.hasBackslashQuoteCommand)) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.CaseCommand(CaseCommandKind.EndCaseCommand)
  }

  private def parseBackslashUpperF(): BackslashKind = {
    if (!featureSet.hasBackslashCaseCommand) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.CaseCommand(CaseCommandKind.FoldCaseCommand)
  }

  private def parseBackslashUpperG(): BackslashKind = {
    if (!featureSet.hasBackslashStickyAssert) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.Assert(AssertKind.Sticky)
  }

  private def parseBackslashUpperH(): BackslashKind = {
    if (!(featureSet.hasBackslashHorizontalSpace || featureSet.hasBackslashHexDigit)) {
      return parseBackslashUnknown()
    }

    next()
    val klass =
      if (featureSet.hasBackslashHorizontalSpace) EscapeClassKind.NonHorizontal else EscapeClassKind.NonHexDigit
    BackslashKind.EscapeClass(klass)
  }

  private def parseBackslashUpperK(): BackslashKind = {
    if (!featureSet.hasBackslashCut) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.Assert(AssertKind.Cut)
  }

  private def parseBackslashUpperL(): BackslashKind = {
    if (!featureSet.hasBackslashCaseCommand) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.CaseCommand(CaseCommandKind.LowerCaseCommand)
  }

  private def parseBackslashUpperN(): BackslashKind = {
    if (!featureSet.hasBackslashNonNewline) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonNewline)
  }

  private def parseBackslashUpperP(): BackslashKind = {
    if (!featureSet.hasBackslashUnicodeProperty) {
      return parseBackslashUnknown()
    }

    next()
    (currentChar: @switch) match {
      case '{' =>
        next()
        val name = parseID()
        val klass = if (currentChar == '=') {
          next()
          val value = parseID()
          EscapeClassKind.NonUnicodePropertyValue(name, value)
        } else {
          EscapeClassKind.NonUnicodeProperty(name)
        }
        expect('}', "Invalid backslash escape")
        BackslashKind.EscapeClass(klass)
      case _ =>
        assert(featureSet.hasBackslashUnicodePropertyBare, "Invalid backslash escape")
        assert(!isEnd, "Invalid backslash escape")
        val name = currentChar.toString
        next()
        BackslashKind.EscapeClass(EscapeClassKind.NonUnicodeBareProperty(name))
    }
  }

  private def parseBackslashUpperQ(): BackslashKind = {
    if (!featureSet.hasBackslashQuoteCommand) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.CaseCommand(CaseCommandKind.QuoteCommand)
  }

  private def parseBackslashUpperR(): BackslashKind = {
    if (!featureSet.hasBackslashGeneralNewline) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline)
  }

  private def parseBackslashUpperS(): BackslashKind = {
    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonSpace)
  }

  private def parseBackslashUpperU(): BackslashKind = {
    if (!(featureSet.hasBackslashUnicodeHex8 || featureSet.hasBackslashCaseCommand)) {
      return parseBackslashUnknown()
    }

    next()

    if (featureSet.hasBackslashCaseCommand) {
      return BackslashKind.CaseCommand(CaseCommandKind.UpperCaseCommand)
    }

    for (_ <- 1 to 8) {
      assert(isHexDigit, "Invalid backslash escape")
      next()
    }

    val str = source.slice(offset - 8, offset)
    val value = Try(Integer.parseInt(str, 16)).toOption.getOrElse(fail("Invalid integer"))
    BackslashKind.Escape(EscapeStyle.UnicodeHex8, value)
  }

  private def parseBackslashUpperV(): BackslashKind = {
    if (!featureSet.hasBackslashVerticalSpace) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonVertical)
  }

  private def parseBackslashUpperW(): BackslashKind = {
    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonWord)
  }

  private def parseBackslashUpperX(): BackslashKind = {
    if (!featureSet.hasBackslashGraphemeCluster) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.EscapeClass(EscapeClassKind.GraphemeCluster)
  }

  private def parseBackslashUpperZ(): BackslashKind = {
    if (!featureSet.hasBackslashUpperEnd) {
      return parseBackslashUnknown()
    }

    next()
    BackslashKind.Assert(AssertKind.UpperEnd)
  }

  private def parseBackslashOctal(): BackslashKind = {
    assert(featureSet.hasBackslashBareOctal, "Invalid backslash escape")

    val size = if ('0' <= currentChar && currentChar <= '3') 3 else 2
    val valueStart = offset
    for (_ <- 1 to size; if isOctalDigit) {
      next()
    }
    val str = source.slice(valueStart, offset)
    val value = Try(Integer.parseInt(str, 8)).getOrElse(fail("Invalid integer"))
    BackslashKind.Escape(EscapeStyle.BareOctal, value)
  }

  private def parseBackslashBackReference(): BackslashKind = {
    if (context.inClass) {
      return parseBackslashOctal()
    }

    val save = offset
    val n = parseIntOption().getOrElse(fail("Invalid backslash escape"))

    if (featureSet.checksValidBackReference && !(0 < n && n <= analysis.captureSize)) {
      reset(save)
      if (isOctalDigit) {
        return parseBackslashOctal()
      }

      return parseBackslashUnknown()
    }

    BackslashKind.EscapeBackReference(BackReferenceStyle.BareBackReference, Reference.IndexedReference(n))
  }

  private def parseBackslashEscape(): BackslashKind = {
    val value = currentChar
    next()
    BackslashKind.Escape(EscapeStyle.Single(value), value.toInt)
  }

  private def parseBackslashUnknown(): BackslashKind = {
    if (isAlphabetic) {
      assert(featureSet.allowsAlphabeticUnknownBackslash, "Invalid backslash escape")
    }

    assert(!isEnd, "Invalid backslash escape")
    val value = currentCodePoint
    nextCodePoint(value)
    BackslashKind.Unknown(value)
  }

  private def parseClass(stack: mutable.Stack[Node]): Unit = protectContext {
    context = context.copy(inClass = true)

    val start = offset

    next()
    val invert = currentChar == '^'
    if (invert) {
      next()
    }

    val item = parseClassItem(!featureSet.allowsClassEmpty)
    expect(']', "Unclosed ']'")

    val end = offset
    stack.push(Node(NodeData.Class(invert, item), SourceLocation(start, end)))
  }

  private def parseClassItem(first: Boolean): ClassItem = {
    val start = offset

    var item = parseClassItemUnion(first)

    var continue = true
    while (continue) {
      continue = false

      if (featureSet.allowsClassIntersection && startWith("&&")) {
        continue = true
        val right = parseClassItemUnion(false)
        val end = offset
        item = ClassItem(ClassItemData.ClassIntersection(item, right), SourceLocation(start, end))
      }

      if (featureSet.allowsClassDiff && startWith("--")) {
        continue = true
        val right = parseClassItemUnion(false)
        val end = offset
        item = ClassItem(ClassItemData.ClassDiff(item, right), SourceLocation(start, end))
      }
    }

    item
  }

  private def parseClassItemUnion(first: Boolean): ClassItem = {
    val start = offset
    val seq = Seq.newBuilder[ClassItem]

    while ((first && start == offset && !isEnd) || !isClassItemStop) {
      seq.addOne(parseClassItemAtom())
    }

    val end = offset
    val items = seq.result()
    if (items.size == 1) {
      return items.head
    }
    ClassItem(ClassItemData.ClassUnion(items), SourceLocation(start, end))
  }

  private def isClassItemStop: Boolean =
    isEnd ||
      currentChar == ']' ||
      featureSet.allowsClassIntersection && source.startsWith("&&", offset) ||
      featureSet.allowsClassDiff && source.startsWith("--", offset)

  private def parseClassItemAtom(): ClassItem = {
    val start = offset
    val begin = parseClassItemNonRange()

    if (!isClassItemStop && currentChar == '-' && isValueClassItem(begin)) {
      val save = offset
      next()

      if (isClassItemStop) {
        reset(save)
        return begin
      }

      if ((featureSet.allowsClassPosix || featureSet.allowsClassNest) && currentChar == '[') {
        reset(save)
        return begin
      }

      val end = parseClassItemNonRange()
      val endOffset = offset
      if (!isValueClassItem(end)) {
        reset(save)
        return begin
      }

      return ClassItem(
        ClassItemData.ClassRange(begin.asInstanceOf[ClassItem.Value], end.asInstanceOf[ClassItem.Value]),
        SourceLocation(start, endOffset)
      )
    }

    begin
  }

  private def parseClassItemNonRange(): ClassItem = {
    val start = offset

    if (featureSet.allowsClassPosix && startWith("[:")) {
      val invert = currentChar == '^'
      if (invert) {
        next()
      }
      val name = parseID()
      if (startWith(":]")) {
        val end = offset
        return ClassItem(ClassItemData.ClassPosix(invert, name), SourceLocation(start, end))
      }
      reset(start)
    }

    if (featureSet.allowsClassNest && currentChar == '[') {
      next()
      val invert = currentChar == '^'
      if (invert) {
        next()
      }
      val item = parseClassItem(!featureSet.allowsClassEmpty)
      if (currentChar == ']') {
        next()
        val end = offset
        return ClassItem(ClassItemData.ClassNest(invert, item), SourceLocation(start, end))
      }
      assert(featureSet.allowsBrokenCloseBracket, "Unclosed ']'")
      reset(start)
    }

    if (currentChar == '\\') {
      next()
      val backslash = parseBackslashKind()
      val end = offset
      backslash match {
        case kind: BackslashKind.BackslashValue =>
          return ClassItem.Value(ClassItemData.ClassBackslashValue(kind), SourceLocation(start, end))
        case BackslashKind.EscapeClass(klass) =>
          return ClassItem(ClassItemData.ClassBackslashClass(klass), SourceLocation(start, end))
        case _ =>
          fail("Invalid escape sequence", start)
      }
    }

    parseClassItemLiteral()
  }

  private def parseClassItemLiteral(): ClassItem = {
    val value =
      if (featureSet.readsAsUnicode) currentCodePoint else currentChar.toInt
    val start = offset
    nextCodePoint(value)
    val end = offset
    ClassItem.Value(ClassItemData.ClassLiteral(value), SourceLocation(start, end))
  }

  private def isValueClassItem(item: ClassItem): Boolean =
    item.isInstanceOf[ClassItem.Value]

  private def parseCloseCurly(stack: mutable.Stack[Node]): Unit = {
    assert(featureSet.allowsBrokenCloseCurly, "Incomplete quantifier")
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Literal('}'), SourceLocation(start, end)))
  }

  private def parseCloseBracket(stack: mutable.Stack[Node]): Unit = {
    assert(featureSet.allowsBrokenCloseBracket, "Unclosed ']'")
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Literal(']'), SourceLocation(start, end)))
  }

  private def parseLiteral(stack: mutable.Stack[Node]): Unit = {
    val value =
      if (featureSet.readsAsUnicode) currentCodePoint else currentChar.toInt
    val start = offset
    nextCodePoint(value)
    val end = offset
    stack.push(Node(NodeData.Literal(value), SourceLocation(start, end)))
  }

  private def protectContext[A](body: => A): A = {
    val savedContext = context
    val value = body
    context = savedContext
    value
  }

  private def applyFlagSetDiff(diff: FlagSetDiff): Unit = {
    if (diff.added.verbose) {
      context = context.copy(skipsComment = true)
    }
    if (diff.removed.exists(_.verbose)) {
      context = context.copy(skipsComment = false)
    }
  }

  private def applyFlagSetDiffWith(diff: FlagSetDiff)(body: => Unit): Unit = {
    protectContext {
      applyFlagSetDiff(diff)
      body
    }
  }

  private def applyResetFlag(flagSet: FlagSet): Unit = {
    context = context.copy(skipsComment = false)
    if (flagSet.verbose) {
      context = context.copy(skipsComment = true)
    }
  }

  private def applyResetFlagWith(flagSet: FlagSet)(body: => Unit): Unit = {
    protectContext {
      applyResetFlag(flagSet)
      body
    }
  }

  private def parseName(end: Char): String = {
    val name = parseID()
    assert(!isEnd && currentChar == end, "Invalid identifier")
    next()
    name
  }

  private def parseID(): String = {
    val start = offset

    if (!featureSet.allowsInvalidIdentifier) {
      assert(isIDStart, "Invalid identifier")
      next()
    }
    while (isIDContinue) {
      next()
    }

    val end = offset
    assert(start < end, "Invalid identifier")

    source.slice(start, end)
  }

  private def parseIntOption(): Option[Int] = {
    if (!isDigit) {
      return None
    }

    val start = offset
    while (isDigit) {
      next()
    }
    val end = offset

    val str = source.slice(start, end)
    str.toIntOption.orElse(fail("Invalid integer", start))
  }

  private def parseOctalIntOption(): Option[Int] = {
    if (!isOctalDigit) {
      return None
    }

    val start = offset

    while (isOctalDigit) {
      next()
    }
    val end = offset

    val str = source.slice(start, end)
    Try(Integer.parseInt(str, 8)).toOption.orElse(fail("Invalid integer"))
  }

  private def parseHexIntOption(): Option[Int] = {
    if (!isHexDigit) {
      return None
    }

    val start = offset

    while (isHexDigit) {
      next()
    }
    val end = offset

    val str = source.slice(start, end)
    Try(Integer.parseInt(str, 16)).toOption.orElse(fail("Invalid integer"))
  }

  private def skipComment(): Boolean = {
    var skipped = false

    var continue = true
    while (continue) {
      continue = false

      if (isSpace) {
        while (isSpace) {
          next()
        }
        continue = true
        skipped = true
      }

      if (!isEnd && currentChar == '#') {
        while (!isEnd && !isNewline) {
          if (featureSet.processBackslashInLineComment && !isEnd && currentChar == '\\') {
            next()
            if (!isEnd) {
              next()
            }
          } else {
            next()
          }
        }
        continue = true
        skipped = true
      }
    }

    skipped
  }

  private def next(): Unit = reset(offset + 1)

  private def nextCodePoint(codePoint: Int): Unit =
    reset(if (codePoint >= 0x10000) offset + 2 else offset + 1)

  private def reset(save: Int): Unit = {
    offset = save
    currentChar = if (offset >= source.length) '\u0000' else source.charAt(offset)
  }

  private def currentCodePoint: Int = {
    val codePoint1 = currentChar.toInt
    if (CodePointUtil.isHighSurrogate(codePoint1) && offset + 1 < source.length) {
      val codePoint2 = source.charAt(offset + 1).toInt
      if (CodePointUtil.isLowSurrogate(codePoint2)) {
        CodePointUtil.decodeSurrogatePair(codePoint1, codePoint2)
      } else {
        codePoint1
      }
    } else {
      codePoint1
    }
  }

  private def expect(c: Char, message: String): Unit = {
    if (isEnd || currentChar != c) {
      fail(message)
    }
    next()
  }

  private def startWith(s: String): Boolean = {
    if (source.startsWith(s, offset)) {
      reset(offset + s.length)
      true
    } else {
      false
    }
  }

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
    !isEnd && (
      ('a' <= currentChar && currentChar <= 'z' || 'A' <= currentChar && currentChar <= 'Z') ||
        (featureSet.hasBackslashControlExtra && '@' <= currentChar && currentChar <= '_')
    )

  private def isAlphabetic: Boolean =
    !isEnd && ('a' <= currentChar && currentChar <= 'z' || 'A' <= currentChar && currentChar <= 'Z')

  private def isIDStart: Boolean =
    !isEnd && CodePointUtil.isIDStart(currentChar.toInt)

  private def isIDContinue: Boolean =
    !isEnd && CodePointUtil.isIDContinue(currentChar.toInt)

  private def assert(condition: Boolean, message: String): Unit = {
    if (!condition) {
      fail(message)
    }
  }

  private def fail(message: String, pos: Int = offset): Nothing =
    throw new ParsingException(message, Some(pos))
}
