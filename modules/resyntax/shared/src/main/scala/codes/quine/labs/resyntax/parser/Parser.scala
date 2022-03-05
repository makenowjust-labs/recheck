package codes.quine.labs.resyntax.parser

import scala.annotation.switch
import scala.collection.mutable

import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BacktrackControlKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.NameStyle
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.parser.Parser.ParsingContext

object Parser {
  private[parser] final case class ParsingContext(
      skipsComment: Boolean
  )

  private[parser] object ParsingContext {
    def from(featureSet: FeatureSet): ParsingContext = ParsingContext(
      skipsComment = featureSet.skipsComment
    )
  }

  def parse(source: String, flagSet: FlagSet, dialect: Dialect): Node = {
    val featureSet = FeatureSet.from(dialect, flagSet)
    val parser = new Parser(source, featureSet)
    parser.parse()
  }
}

private[parser] final class Parser(
    private[this] val source: String,
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
      case NodeData.Repeat(_, _) =>
        assert(featureSet.allowsNestedRepeat, "Nested quantifier")
      case _ =>
        // TODO: backslash and other assertion
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
    if (!featureSet.allowsBrokenBracket) {
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

  private def parseCaret(stack: mutable.Stack[Node]): Unit = ???

  private def parseDollar(stack: mutable.Stack[Node]): Unit = ???

  private def parseDot(stack: mutable.Stack[Node]): Unit = {
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Dot, SourceLocation(start, end)))
  }

  private def parseBackslash(stack: mutable.Stack[Node]): Unit = ???

  private def parseClass(stack: mutable.Stack[Node]): Unit = ???

  private def parseCloseCurly(stack: mutable.Stack[Node]): Unit = ???

  private def parseCloseBracket(stack: mutable.Stack[Node]): Unit = ???

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
