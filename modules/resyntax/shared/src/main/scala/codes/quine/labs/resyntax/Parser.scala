package codes.quine.labs.resyntax

import scala.annotation.switch
import scala.collection.mutable

import codes.quine.labs.resyntax.Parser.ParsingContext
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
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.SourceLocation

object Parser {
  final case class ParsingContext(
      skipsComment: Boolean
  )

  object ParsingContext {
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

final class Parser(
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
      currentChar match {
        case ')' => fail("Unmatched ')'")
        case _   => fail("Unexpected end")
      }
    }
    node
  }

  def parseDisjunction(): Node = {
    val start = offset
    val builder = Seq.newBuilder[Node]
    builder.addOne(parseSequence())

    while (!isEnd && currentChar == '|') {
      next()
      builder.addOne(parseSequence())
    }

    val nodes = builder.result()
    if (nodes.size == 1) {
      nodes.head
    } else {
      val end = offset
      Node.Disjunction(nodes, Some(SourceLocation(start, end)))
    }
  }

  def parseSequence(): Node = {
    val start = offset
    val stack = mutable.Stack.empty[Node]

    while (!isSequenceStop) {
      if (context.skipsComment && skipComment()) {
        // If comment is skipped, we should go to next loop.
      } else {
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
    }

    val nodes = stack.toSeq
    if (nodes.size == 1) {
      nodes.head
    } else {
      val end = offset
      Node.Sequence(nodes.reverse, Some(SourceLocation(start, end)))
    }
  }

  def parseSimpleRepeat(stack: mutable.Stack[Node], buildQuantifier: BacktrackStrategy => Quantifier): Unit = {
    val node = popRepeatable(stack)
    next()
    val strategy = parseBacktrackStrategy(exact = false)
    val quantifier = buildQuantifier(strategy)
    pushRepeat(stack, node, quantifier)
  }

  def parseCurlyQuantifier(stack: mutable.Stack[Node]): Unit = {
    val save = offset
    next()
    parseIntOption() match {
      case Some(min) =>
        if (!isEnd && currentChar == '}') {
          next()
          val strategy = parseBacktrackStrategy(exact = true)
          val quantifier = Quantifier.Exact(min, strategy)
          pushRepeat(stack, popRepeatable(stack), quantifier)
        } else if (!isEnd && currentChar == ',') {
          next()
          if (!isEnd && currentChar == '}') {
            next()
            val strategy = parseBacktrackStrategy(exact = false)
            val quantifier = Quantifier.Unbounded(min, strategy)
            pushRepeat(stack, popRepeatable(stack), quantifier)
          } else {
            parseIntOption() match {
              case Some(max) =>
                if (!isEnd && currentChar == '}') {
                  next()
                  val strategy = parseBacktrackStrategy(exact = false)
                  val quantifier = Quantifier.Bounded(min, max, strategy)
                  pushRepeat(stack, popRepeatable(stack), quantifier)
                } else {
                  resetBracket(stack, save)
                }
              case None =>
                resetBracket(stack, save)
            }
          }
        } else {
          resetBracket(stack, save)
        }
      case _ =>
        if (!isEnd && currentChar == ',' && featureSet.hasMaxBounded) {
          next()
          parseIntOption() match {
            case Some(max) =>
              if (!isEnd && currentChar == '}') {
                next()
                val strategy = parseBacktrackStrategy(exact = false)
                val quantifier = Quantifier.MaxBounded(max, strategy)
                pushRepeat(stack, popRepeatable(stack), quantifier)
              } else {
                resetBracket(stack, save)
              }
            case None =>
              resetBracket(stack, save)
          }
        } else {
          resetBracket(stack, save)
        }
    }
  }

  def popRepeatable(stack: mutable.Stack[Node]): Node = {
    if (stack.isEmpty) {
      fail("Nothing to repeat")
    }
    val node = stack.pop()
    assertRepeatable(node)
    node
  }

  def assertRepeatable(node: Node): Unit =
    node match {
      case Node.Group(_: GroupKind.PositiveLookAhead | _: GroupKind.NegativeLookAhead, _, _) =>
        if (!featureSet.allowsLookAheadRepeat) {
          fail("Invalid quantifier", node.loc.map(_.start).getOrElse(offset))
        }
      case Node.Group(_: GroupKind.PositiveLookBehind | _: GroupKind.NegativeLookBehind, _, _) =>
        if (!featureSet.allowsLookBehindRepeat) {
          fail("Invalid quantifier", node.loc.map(_.start).getOrElse(offset))
        }
      case _ =>
        // TODO: backslash and other assertion
        ()
    }

  def pushRepeat(stack: mutable.Stack[Node], node: Node, quantifier: Quantifier): Unit = {
    val end = offset
    stack.push(Node.Repeat(node, quantifier, node.loc.map(_.copy(end = end))))
  }

  def parseBacktrackStrategy(exact: Boolean): BacktrackStrategy = {
    if (isEnd || exact && !featureSet.allowsMeaninglessBacktrackStrategy) {
      return BacktrackStrategy.Greedy
    }

    if (currentChar == '?') {
      next()
      return BacktrackStrategy.Lazy
    }

    if (currentChar == '+' && featureSet.hasPossessiveBacktrackStrategy) {
      next()
      return BacktrackStrategy.Possessive
    }

    BacktrackStrategy.Greedy
  }

  def resetBracket(stack: mutable.Stack[Node], save: Int): Unit = {
    if (!featureSet.allowsBrokenBracket) {
      fail("Incomplete quantifier", save)
    }
    reset(save)
    stack.push(Node.Literal('{', Some(SourceLocation(save, save + 1))))
    next()
  }

  def parseGroup(stack: mutable.Stack[Node]): Unit = {
    val start = offset
    next()
    if (isEnd) {
      fail("Invalid")
    }

    (currentChar: @switch) match {
      case '?' =>
        parseExtendedGroup(stack, start)
      case '*' =>
        parseAlphabeticGroup(stack, start)
      case _ =>
        parseGroupBody(stack, GroupKind.IndexedCapture, start)
    }
  }

  def parseExtendedGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    if (isEnd) {
      fail("Invalid group")
    }

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

  def parseNonCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.NonCapture, start)
  }

  def parseBranchResetGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasBranchReset) {
      fail("Invalid group")
    }
    next()
    protectContext {
      val node = parseDisjunction()
      closeGroup()
      val nodes = node match {
        case Node.Disjunction(nodes, _) => nodes
        case _                          => Seq(node)
      }
      stack.push(Node.Command(CommandKind.BranchReset(nodes), Some(SourceLocation(start, offset))))
    }
  }

  def parsePositiveLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.PositiveLookAhead(AssertNameStyle.Symbolic), start)
  }

  def parseNegativeLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), start)
  }

  def parseAngleGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    if (isEnd) {
      fail("Invalid group")
    }

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

  def parsePositiveLookBehindGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.PositiveLookBehind(AssertNameStyle.Symbolic), start)
  }

  def parseNegativeLookBehindGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseGroupBody(stack, GroupKind.NegativeLookBehind(AssertNameStyle.Symbolic), start)
  }

  def parseNonAtomicPositiveLookBehindGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasNonAtomicLookAround) {
      fail("Invalid group")
    }
    next()
    parseGroupBody(stack, GroupKind.NonAtomicPositiveLookBehind(AssertNameStyle.Symbolic), start)
  }

  def parseAtomicGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasAtomicGroup) {
      fail("Invalid group")
    }
    next()
    parseGroupBody(stack, GroupKind.Atomic(AssertNameStyle.Symbolic), start)
  }

  def parseNonAtomicPositiveLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasNonAtomicLookAround) {
      fail("Invalid group")
    }
    next()
    parseGroupBody(stack, GroupKind.NonAtomicPositiveLookAhead(AssertNameStyle.Symbolic), start)
  }

  def parseAbsenceGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasAbsenceOperator) {
      fail("Invalid group")
    }
    next()
    parseGroupBody(stack, GroupKind.Absence, start)
  }

  def parseConditionalGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasConditional) {
      fail("Invalod group")
    }

    next()

    for (n <- parseIntOption()) {
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Indexed(n), start)
      return
    }

    if (!isEnd && currentChar == '<' && featureSet.hasNamedCaptureTest) {
      next()
      val name = parseName(end = '>')
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Angle, name), start)
      return
    }

    if (!isEnd && currentChar == '\'' && featureSet.hasNamedCaptureTest) {
      next()
      val name = parseName(end = '\'')
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Quote, name), start)
      return
    }

    if (source.startsWith("DEFINE", offset) && featureSet.hasDefineTest) {
      reset(offset + "DEFINE".length)
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Define, start)
      return
    }

    if (source.startsWith("VERSION", offset) && featureSet.hasVersionTest) {
      reset(offset + "VERSION".length)
      val lt = !isEnd && currentChar == '>'
      if (lt) {
        next()
      }
      if (isEnd || currentChar != '=') {
        fail("Invalid condition")
      }
      next()
      val major = parseIntOption() match {
        case Some(n) => n
        case None    => fail("Invalid condition")
      }
      if (isEnd || currentChar != '.') {
        fail("Invalid condition")
      }
      next()
      val minor = parseIntOption() match {
        case Some(n) => n
        case None    => fail("Invalid condition")
      }
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Version(lt, major, minor), start)
      return
    }

    if (!isEnd && currentChar == 'R' && featureSet.hasRecursionTest) {
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
          parseIntOption() match {
            case Some(n) =>
              closeCondition()
              parseConditionalGroupBody(stack, ConditionalTest.IndexedRecursion(n), start)
            case None =>
              fail("Invalid group")
          }
      }
      return
    }

    if (isIDStart && featureSet.hasBareNamedCaptureTest) {
      val name = parseID()
      closeCondition()
      parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Bare, name), start)
      return
    }

    val isRelative = !isEnd && (currentChar == '+' || currentChar == '-')
    if (isRelative && featureSet.hasRelativeIndexedCaptureTest) {
      val minus = currentChar == '-'
      next()
      parseIntOption() match {
        case Some(n) =>
          parseConditionalGroupBody(stack, ConditionalTest.Relative(if (minus) -n else n), start)
          return
        case None =>
          fail("Invalid condition")
      }
    }

    if (featureSet.hasLookAroundTest) {
      if (parseLookAroundTest(stack, GroupKind.PositiveLookAhead, start)("?=", "*positive_lookahead:", "*pla:")) {
        return
      }

      if (parseLookAroundTest(stack, GroupKind.NegativeLookAhead, start)("?!", "*negative_lookahead:", "*nla:")) {
        return
      }

      if (parseLookAroundTest(stack, GroupKind.PositiveLookBehind, start)("?<=", "*positive_lookbehind:", "*plb:")) {
        return
      }

      if (parseLookAroundTest(stack, GroupKind.NegativeLookBehind, start)("?<!", "*negative_lookbehind:", "*nlb:")) {
        return
      }

      parseLookAroundConditional(stack, None, start)
      return
    }

    fail("Invalid condition")
  }

  def closeCondition(): Unit = {
    if (isEnd || currentChar != ')') {
      fail("Invalid condition")
    }
    next()
  }

  def parseConditionalGroupBody(stack: mutable.Stack[Node], test: ConditionalTest, start: Int): Unit = {
    protectContext {
      val yes = protectContext(parseSequence())
      val no = Option.when(!isEnd && currentChar == '|') {
        next()
        protectContext(parseSequence())
      }
      closeGroup()
      val end = offset
      stack.push(Node.Command(CommandKind.Conditional(test, yes, no), Some(SourceLocation(start, end))))
    }
  }

  def parseLookAroundTest(stack: mutable.Stack[Node], la: AssertNameStyle => GroupKind.LookAround, start: Int)(
      symbolic: String,
      alphabetic: String,
      abbrev: String
  ): Boolean = {
    if (source.startsWith(symbolic, offset)) {
      reset(offset + symbolic.length)
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Symbolic)), start)
      return true
    }

    if (source.startsWith(alphabetic, offset) && featureSet.hasAlphabeticGroup) {
      reset(offset + alphabetic.length)
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Alphabetic)), start)
      return true
    }

    if (source.startsWith(abbrev, offset) && featureSet.hasAlphabeticGroup) {
      reset(offset + abbrev.length)
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Abbrev)), start)
      return true
    }

    false
  }

  def parseLookAroundConditional(stack: mutable.Stack[Node], kind: Option[GroupKind.LookAround], start: Int): Unit = {
    val node = protectContext(parseDisjunction())
    closeCondition()
    parseConditionalGroupBody(stack, ConditionalTest.LookAround(kind, node), start)
  }

  def parseAngleNamedCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasAngleNamedCapture) {
      fail("Invalid group")
    }
    parseNamedCaptureGroup(stack, '>', NameStyle.Angle, start)
  }

  def parseQuoteNamedCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasQuoteNamedCapture) {
      fail("Invalid group")
    }
    next()
    parseNamedCaptureGroup(stack, '\'', NameStyle.Quote, start)
  }

  def parseNamedCaptureGroup(stack: mutable.Stack[Node], end: Char, style: NameStyle, start: Int): Unit = {
    if (featureSet.hasBalanceGroup) {
      parseBalanceGroup(stack, end, style, start)
    } else {
      val name = parseName(end = end)
      parseGroupBody(stack, GroupKind.NamedCapture(style, name), start)
    }
  }

  def parseBalanceGroup(stack: mutable.Stack[Node], end: Char, style: NameStyle, start: Int): Unit = {
    if (!isEnd && currentChar == '-') {
      next()
      val test = parseName(end = end)
      parseGroupBody(stack, GroupKind.Balance(style, None, test), start)
    } else {
      val name = parseID()
      if (!isEnd && currentChar == '-') {
        next()
        val test = parseName(end = end)
        parseGroupBody(stack, GroupKind.Balance(style, Some(name), test), start)
      } else if (!isEnd && currentChar == end) {
        next()
        parseGroupBody(stack, GroupKind.NamedCapture(style, name), start)
      } else {
        fail("Invalid identifier")
      }
    }
  }

  def parsePGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasPGroup) {
      fail("Invalid group")
    }
    next()
    if (isEnd) {
      fail("Invalid group")
    }

    (currentChar: @switch) match {
      case '<' =>
        next()
        val name = parseName(end = '>')
        parseGroupBody(stack, GroupKind.PNamedCapture(name), start)
      case '=' =>
        val name = parseID()
        pushCommand(stack, CommandKind.PBackReference(name), start)
      case '>' =>
        if (!featureSet.hasPCall) {
          fail("Invalid group")
        }
        val name = parseID()
        pushCommand(stack, CommandKind.PNamedCall(name), start)
      case _ =>
        fail("Invalid group")
    }
  }

  def parseRGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasRCall) {
      fail("Invalid group")
    }
    next()
    pushCommand(stack, CommandKind.RCall, start)
  }

  def parseNamedCallGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasCallCommand) {
      fail("Invalid group")
    }
    next()
    val name = parseID()
    pushCommand(stack, CommandKind.NamedCall(name), start)
  }

  def parseIndexedCallGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasCallCommand) {
      fail("Invalid group")
    }
    parseIntOption() match {
      case Some(n) =>
        pushCommand(stack, CommandKind.IndexedCall(n), start)
      case None =>
        fail("Invalid group")
    }
  }

  def parsePlusGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasCallCommand) {
      fail("Invalid group")
    }
    next()
    parseIntOption() match {
      case Some(n) =>
        pushCommand(stack, CommandKind.RelativeCall(n), start)
      case None =>
        fail("Invalid group")
    }
  }

  def parseMinusGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    next()
    parseIntOption() match {
      case Some(n) =>
        if (!featureSet.hasCallCommand) {
          fail("Invalid group")
        }
        pushCommand(stack, CommandKind.RelativeCall(-n), start)
      case None =>
        if (!featureSet.hasInlineFlag) {
          fail("Invalid group")
        }
        val added = FlagSet()
        val removed = parseFlagSet()
        val diff = FlagSetDiff(added, Some(removed))
        if (isEnd) {
          fail("Invalid group")
        }
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

  def parseResetFlagGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasResetFlag) {
      fail("Invalid group")
    }
    next()
    val flagSet = parseFlagSet()
    if (isEnd) {
      fail("Invalid group")
    }
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

  def parseCommentGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasInlineComment) {
      fail("Invalid group")
    }
    next()
    val textStart = offset
    while (isEnd || currentChar != ')') {
      if (featureSet.processBackslashInCommentGroup && currentChar == '\\') {
        next()
        if (!isEnd) {
          next()
        }
      } else {
        next()
      }
    }
    if (isEnd) {
      fail("Invalid group")
    }
    val end = offset
    val text = source.slice(textStart, end)
    pushCommand(stack, CommandKind.Comment(text), start)
  }

  def parseInlineCodeGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasInlineCode) {
      fail("Invalid group")
    }
    next()
    val code = parseCode(end = '}')
    pushCommand(stack, CommandKind.InlineCode(code), start)
  }

  def parseEmbedCodeGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasInlineCode) {
      fail("Invalid group")
    }
    next()
    if (isEnd || currentChar != '{') {
      fail("Invalid group")
    }
    val code = parseCode(end = '}')
    pushCommand(stack, CommandKind.EmbedCode(code), start)
  }

  def parseCalloutGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasCallout) {
      fail("Invalid group")
    }
    next()
    if (isEnd) {
      fail("Invalid group")
    }
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

  def parseCode(end: Char, escapeDouble: Boolean = false): String = {
    val start = offset
    while (
      isEnd || !(currentChar == end && !(escapeDouble && offset + 1 < source.length && source.charAt(
        offset + 1
      ) == end))
    ) {
      next()
    }
    if (isEnd) {
      fail("Invalid group")
    }
    val code = source.slice(start, offset)
    next()
    code
  }

  def parseInlineFlagGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasInlineFlag) {
      fail("Invalid group")
    }
    val added = parseFlagSet()
    if (isEnd) {
      fail("Invalid group")
    }
    (currentChar: @switch) match {
      case '-' =>
        next()
        val removed = parseFlagSet()
        val diff = FlagSetDiff(added, Some(removed))
        if (isEnd) {
          fail("Invalid group")
        }
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

  def pushInlineFlagCommand(stack: mutable.Stack[Node], diff: FlagSetDiff, start: Int): Unit = {
    pushCommand(stack, CommandKind.InlineFlag(diff), start)
    if (featureSet.hasIncomprehensiveInlineFlag) {
      parseOpaqueDisjunction(stack, diff)
    } else {
      applyFlagSetDiff(diff)
    }
  }

  def parseOpaqueDisjunction(stack: mutable.Stack[Node], diff: FlagSetDiff): Unit = {
    applyFlagSetDiffWith(diff) {
      val start = offset
      val node = parseDisjunction()
      val end = offset
      stack.push(Node.Group(GroupKind.Opaque, node, Some(SourceLocation(start, end))))
    }
  }

  def parseAlphabeticGroup(stack: mutable.Stack[Node], start: Int): Unit = {
    if (!featureSet.hasAlphabeticGroup) {
      fail("Invalid group")
    }

    next()

    if (parseBacktrackControl(stack, start)("ACCEPT", BacktrackControlKind.Accept)) {
      return
    }

    if (parseBacktrackControl(stack, start)("FAIL", BacktrackControlKind.Fail)) {
      return
    }

    if (parseBacktrackControl(stack, start)("MARK", BacktrackControlKind.Mark)) {
      return
    }

    if (parseBacktrackControl(stack, start)("COMMIT", BacktrackControlKind.Commit)) {
      return
    }

    if (parseBacktrackControl(stack, start)("PRUNE", BacktrackControlKind.Prune)) {
      return
    }

    if (parseBacktrackControl(stack, start)("SKIP", BacktrackControlKind.Skip)) {
      return
    }

    if (parseBacktrackControl(stack, start)("THEN", BacktrackControlKind.Then)) {
      return
    }

    if (parseAlphabetic(stack, start, "positive_lookahead:", "pla:", GroupKind.PositiveLookAhead)) {
      return
    }

    if (parseAlphabetic(stack, start, "negative_lookahead:", "nla:", GroupKind.NegativeLookAhead)) {
      return
    }

    if (parseAlphabetic(stack, start, "positive_lookbehind:", "plb:", GroupKind.PositiveLookBehind)) {
      return
    }

    if (parseAlphabetic(stack, start, "negative_lookbehind:", "nlb:", GroupKind.NegativeLookBehind)) {
      return
    }

    if (parseAlphabetic(stack, start, "atomic:", GroupKind.Atomic)) {
      return
    }

    if (parseAlphabetic(stack, start, "script_run:", "sr:", GroupKind.ScriptRun)) {
      return
    }

    if (parseAlphabetic(stack, start, "atomic_script_run:", "asr:", GroupKind.AtomicScriptRun)) {
      return
    }

    if (featureSet.hasNonAtomicLookAround) {
      if (
        parseAlphabetic(stack, start, "non_atomic_positive_lookahead:", "napla:", GroupKind.NonAtomicPositiveLookAhead)
      ) {
        return
      }

      if (
        parseAlphabetic(
          stack,
          start,
          "non_atomic_positive_lookbehind:",
          "naplb:",
          GroupKind.NonAtomicPositiveLookBehind
        )
      ) {
        return
      }
    }

    fail("Invalid group")
  }

  def parseBacktrackControl(
      stack: mutable.Stack[Node],
      start: Int
  )(command: String, kind: BacktrackControlKind): Boolean = {
    if (!source.startsWith(command, offset)) {
      return false
    }
    reset(offset + command.length)
    if (!isEnd) {
      fail("Invalid group")
    }

    (currentChar: @switch) match {
      case ':' =>
        next()
        val name = parseID()
        pushCommand(stack, CommandKind.BacktrackControl(kind, Some(name)), start)
        return true
      case ')' =>
        pushCommand(stack, CommandKind.BacktrackControl(kind, None), start)
        return true
      case _ =>
        fail("Invalid group")
    }
  }

  def parseAlphabetic(
      stack: mutable.Stack[Node],
      start: Int,
      alphabetic: String,
      abbrev: String,
      k: AssertNameStyle => GroupKind
  ): Boolean = {
    if (source.startsWith(alphabetic, offset)) {
      reset(offset + alphabetic.length)
      parseGroupBody(stack, k(AssertNameStyle.Alphabetic), start)
      return true
    }

    if (source.startsWith(abbrev, offset)) {
      reset(offset + abbrev.length)
      parseGroupBody(stack, k(AssertNameStyle.Abbrev), start)
      return true
    }

    false
  }

  def parseAlphabetic(
      stack: mutable.Stack[Node],
      start: Int,
      alphabetic: String,
      k: AssertNameStyle => GroupKind
  ): Boolean = {
    if (source.startsWith(alphabetic, offset)) {
      reset(offset + alphabetic.length)
      parseGroupBody(stack, k(AssertNameStyle.Alphabetic), start)
      return true
    }

    false
  }

  def parseGroupBody(stack: mutable.Stack[Node], kind: GroupKind, start: Int): Unit = {
    val node = protectContext(parseDisjunction())
    closeGroup()
    val end = offset
    stack.push(Node.Group(kind, node, Some(SourceLocation(start, end))))
  }

  def pushCommand(stack: mutable.Stack[Node], kind: CommandKind, start: Int): Unit = {
    closeGroup()
    val end = offset
    stack.push(Node.Command(kind, Some(SourceLocation(start, end))))
  }

  def closeGroup(): Unit = {
    if (isEnd || currentChar != ')') {
      fail("Unclosed group")
    }
    next()
  }

  def parseFlagSet(): FlagSet = {
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

  def parseCaret(stack: mutable.Stack[Node]): Unit = ???

  def parseDollar(stack: mutable.Stack[Node]): Unit = ???

  def parseBackslash(stack: mutable.Stack[Node]): Unit = ???

  def parseClass(stack: mutable.Stack[Node]): Unit = ???

  def parseCloseCurly(stack: mutable.Stack[Node]): Unit = ???

  def parseCloseBracket(stack: mutable.Stack[Node]): Unit = ???

  def parseLiteral(stack: mutable.Stack[Node]): Unit = {
    val value =
      if (featureSet.readsAsUnicode) currentCodePoint else currentChar.toInt
    val start = offset
    nextCodePoint(value)
    val end = offset
    stack.push(Node.Literal(value, Some(SourceLocation(start, end))))
  }

  def protectContext[A](body: => A): A = {
    val savedContext = context
    val value = body
    context = savedContext
    value
  }

  def applyFlagSetDiff(diff: FlagSetDiff): Unit = {
    if (diff.added.verbose) {
      context = context.copy(skipsComment = true)
    }
    if (diff.removed.exists(_.verbose)) {
      context = context.copy(skipsComment = false)
    }
  }

  def applyFlagSetDiffWith(diff: FlagSetDiff)(body: => Unit): Unit = {
    protectContext {
      applyFlagSetDiff(diff)
      body
    }
  }

  def applyResetFlag(flagSet: FlagSet): Unit = {
    context = context.copy(skipsComment = false)
    if (flagSet.verbose) {
      context = context.copy(skipsComment = true)
    }
  }

  def applyResetFlagWith(flagSet: FlagSet)(body: => Unit): Unit = {
    protectContext {
      applyResetFlag(flagSet)
      body
    }
  }

  def parseName(end: Char): String = {
    val start = offset
    val name = parseID()
    if (isEnd || currentChar != end) {
      fail("Invalid identifier", start)
    }
    next()
    name
  }

  def parseID(): String = {
    val start = offset

    if (!featureSet.allowsInvalidIdentifier) {
      if (!isIDStart) {
        fail("Invalid identifier")
      }
      next()
    }
    while (isIDContinue) {
      next()
    }

    val end = offset
    if (start == end) {
      fail("Invalid identifier")
    }

    source.slice(start, end)
  }

  def parseIntOption(): Option[Int] = {
    if (!isDigit) {
      return None
    }

    val start = offset
    while (isDigit) {
      next()
    }
    val end = offset

    val str = source.slice(start, end)
    str.toIntOption match {
      case Some(n) => Some(n)
      case None    => fail("Invalid integer", start)
    }
  }

  def skipComment(): Boolean = {
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
        while (!isNewline) {
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

  def next(): Unit = reset(offset + 1)

  def nextCodePoint(codePoint: Int): Unit =
    reset(if (codePoint >= 0x10000) offset + 2 else offset + 1)

  def reset(save: Int): Unit = {
    offset = save
    currentChar = if (offset >= source.length) '\u0000' else source.charAt(offset)
  }

  def currentCodePoint: Int = {
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

  def isEnd: Boolean = offset >= source.length

  def isSequenceStop: Boolean =
    isEnd || currentChar == '|' || currentChar == ')'

  def isSpace: Boolean =
    !isEnd && CodePointUtil.isSpace(currentChar.toInt)

  def isNewline: Boolean =
    !isEnd && CodePointUtil.isNewline(currentChar.toInt)

  def isDigit: Boolean =
    !isEnd && CodePointUtil.isDigit(currentChar.toInt)

  def isIDStart: Boolean =
    !isEnd && CodePointUtil.isIDStart(currentChar.toInt)

  def isIDContinue: Boolean =
    !isEnd && CodePointUtil.isIDContinue(currentChar.toInt)

  def extend(loc: Option[SourceLocation], end: Int): Option[SourceLocation] =
    loc.map(_.copy(end = end))

  def fail(message: String, pos: Int = offset): Nothing =
    throw new ParsingException(message, Some(pos))
}
