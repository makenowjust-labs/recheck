package codes.quine.labs.resyntax

import codes.quine.labs.resyntax.Parser.ParsingContext

import scala.collection.mutable
import codes.quine.labs.resyntax.ast.{
  AssertNameStyle,
  BacktrackStrategy,
  CommandKind,
  FlagSet,
  FlagSetDiff,
  GroupKind,
  NameStyle,
  Node,
  Quantifier,
  SourceLocation
}

import scala.annotation.switch

object Parser {
  final case class ParsingContext(
      skipsComment: Boolean
  )

  object ParsingContext {
    def from(featureSet: FeatureSet): ParsingContext = ParsingContext(
      skipsComment = featureSet.skipsComment
    )
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
            val node = popRepeatable(stack)
            next()
            val strategy = parseBacktrackStrategy(exact = false)
            val quantifier = Quantifier.Star(strategy)
            pushRepeat(stack, node, quantifier)
          case '+' =>
            val node = popRepeatable(stack)
            next()
            val strategy = parseBacktrackStrategy(exact = false)
            val quantifier = Quantifier.Plus(strategy)
            pushRepeat(stack, node, quantifier)
          case '?' =>
            val node = popRepeatable(stack)
            next()
            val strategy = parseBacktrackStrategy(exact = false)
            val quantifier = Quantifier.Question(strategy)
            pushRepeat(stack, node, quantifier)
          case '{' =>
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
          case '(' =>
            parseGroup(stack)
          case _ =>
            val value =
              if (featureSet.readsAsUnicode) currentCodePoint else currentChar.toInt
            val start = offset
            nextCodePoint(value)
            val end = offset
            stack.push(Node.Literal(value, Some(SourceLocation(start, end))))
        }
      }
    }

    val nodes = stack.toSeq
    if (nodes.size == 1) {
      nodes.head
    } else {
      val end = offset
      Node.Sequence(nodes, Some(SourceLocation(start, end)))
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
    if (!isEnd && currentChar == '?') {
      next()
      if (isEnd) {
        fail("Invalid group")
      }
      (currentChar: @switch) match {
        case ':' =>
          next()
          parseGroupBody(stack, GroupKind.NonCapture, start)
        case '=' =>
          next()
          parseGroupBody(stack, GroupKind.PositiveLookAhead(AssertNameStyle.Symbolic), start)
        case '!' =>
          next()
          parseGroupBody(stack, GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), start)
        case '\'' =>
          if (!featureSet.hasQuoteNamedCapture) {
            fail("Invalid group")
          }
          next()
          if (featureSet.hasBalanceGroup) {
            if (!isEnd && currentChar == '-') {
              val test = parseName(end = '\'')
              parseGroupBody(stack, GroupKind.Balance(NameStyle.Quote, None, test), start)
            } else {
              val name = parseID()
              if (!isEnd && currentChar == '-') {
                val test = parseName(end = '\'')
                parseGroupBody(stack, GroupKind.Balance(NameStyle.Quote, Some(name), test), start)
              } else if (!isEnd && currentChar == '\'') {
                next()
                parseGroupBody(stack, GroupKind.NamedCapture(NameStyle.Quote, name), start)
              } else {
                fail("Invalid identifier")
              }
            }
          } else {
            val name = parseName(end = '\'')
            parseGroupBody(stack, GroupKind.NamedCapture(NameStyle.Quote, name), start)
          }
        case '<' =>
          next()
          if (isEnd) {
            fail("Invalid group")
          }
          (currentChar: @switch) match {
            case '=' =>
              next()
              parseGroupBody(stack, GroupKind.PositiveLookBehind(AssertNameStyle.Symbolic), start)
            case '!' =>
              next()
              parseGroupBody(stack, GroupKind.PositiveLookBehind(AssertNameStyle.Symbolic), start)
            case '*' =>
              next()
              parseGroupBody(stack, GroupKind.NonAtomicPositiveLookBehind(AssertNameStyle.Symbolic), start)
            case _ =>
              if (!featureSet.hasAngleNamedCapture) {
                fail("Invalid group")
              }
              if (featureSet.hasBalanceGroup) {
                if (!isEnd && currentChar == '-') {
                  val test = parseName(end = '>')
                  parseGroupBody(stack, GroupKind.Balance(NameStyle.Angle, None, test), start)
                } else {
                  val name = parseID()
                  if (!isEnd && currentChar == '-') {
                    val test = parseName(end = '>')
                    parseGroupBody(stack, GroupKind.Balance(NameStyle.Angle, Some(name), test), start)
                  } else if (!isEnd && currentChar == '>') {
                    next()
                    parseGroupBody(stack, GroupKind.NamedCapture(NameStyle.Angle, name), start)
                  } else {
                    fail("Invalid identifier")
                  }
                }
              } else {
                val name = parseName(end = '>')
                parseGroupBody(stack, GroupKind.NamedCapture(NameStyle.Angle, name), start)
              }
          }
        case '>' =>
          if (!featureSet.hasAtomicGroup) {
            fail("Invalid group")
          }
          next()
          parseGroupBody(stack, GroupKind.Atomic(AssertNameStyle.Symbolic), start)
        case '*' =>
          if (!featureSet.hasNonAtomicLookAround) {
            fail("Invalid group")
          }
          next()
          parseGroupBody(stack, GroupKind.NonAtomicPositiveLookAhead(AssertNameStyle.Symbolic), start)
        case '#' =>
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
        case '{' =>
          if (!featureSet.hasInlineCode) {
            fail("Invalid group")
          }
          next()
          val code = parseCode(end = '}')
          pushCommand(stack, CommandKind.InlineCode(code), start)
        case '?' =>
          if (!featureSet.hasInlineCode) {
            fail("Invalid group")
          }
          next()
          if (isEnd || currentChar != '{') {
            fail("Invalid group")
          }
          val code = parseCode(end = '}')
          pushCommand(stack, CommandKind.EmbedCode(code), start)
        case '&' =>
          if (!featureSet.hasCallCommand) {
            fail("Invalid group")
          }
          next()
          val name = parseID()
          pushCommand(stack, CommandKind.NamedCall(name), start)
        case '+' =>
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
        case '-' =>
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
        case '^' =>
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
        case '~' =>
          if (!featureSet.hasAbsenceOperator) {
            fail("Invalid group")
          }
          next()
          parseGroupBody(stack, GroupKind.Absence, start)
        case '|' =>
          if (!featureSet.hasBranchReset) {
            fail("Invalid group")
          }
          next()
          protectContext {
            val node = parseDisjunction()
            if (isEnd || currentChar != ')') {
              fail("Unclosed group")
            }
            next()
            val nodes = node match {
              case Node.Disjunction(nodes, _) => nodes
              case _                          => Seq(node)
            }
            stack.push(Node.Command(CommandKind.BranchReset(nodes), Some(SourceLocation(start, offset))))
          }
        case '(' =>
        // TODO: conditional
        case 'C' =>
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
        case 'P' =>
          if (!featureSet.hasPGroup) {
            fail("Invalid group")
          }
          next()
          if (!isEnd && currentChar == '<') {
            next()
            val name = parseName(end = '>')
            parseGroupBody(stack, GroupKind.PNamedCapture(name), start)
          } else if (!isEnd && currentChar == '=') {
            val name = parseID()
            pushCommand(stack, CommandKind.PBackReference(name), start)
          } else if (!isEnd && currentChar == '>' && featureSet.hasPCall) {
            val name = parseID()
            pushCommand(stack, CommandKind.PNamedCall(name), start)
          } else {
            fail("Invalid group")
          }
        case 'R' =>
          if (!featureSet.hasRCall) {
            fail("Invalid group")
          }
          next()
          pushCommand(stack, CommandKind.RCall, start)
        case _ =>
          parseIntOption() match {
            case Some(n) =>
              if (!featureSet.hasCallCommand) {
                fail("Invalid group")
              }
              pushCommand(stack, CommandKind.IndexedCall(n), start)
            case None =>
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
                      pushCommand(stack, CommandKind.InlineFlag(diff), start)
                      if (featureSet.hasIncomprehensiveInlineFlag) {
                        applyFlagSetDiffWith(diff) {
                          val start = offset
                          val node = parseDisjunction()
                          val end = offset
                          stack.push(Node.Group(GroupKind.Opaque, node, Some(SourceLocation(start, end))))
                        }
                      } else {
                        applyFlagSetDiff(diff)
                      }
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
                  pushCommand(stack, CommandKind.InlineFlag(diff), start)
                  if (featureSet.hasIncomprehensiveInlineFlag) {
                    applyFlagSetDiffWith(diff) {
                      val start = offset
                      val node = parseDisjunction()
                      val end = offset
                      stack.push(Node.Group(GroupKind.Opaque, node, Some(SourceLocation(start, end))))
                    }
                  } else {
                    applyFlagSetDiff(diff)
                  }
                case _ =>
                  fail("Invalid flag")
              }
          }
      }
    } else if (!isEnd && currentChar == '*') {
      // TODO:
    } else {
      parseGroupBody(stack, GroupKind.IndexedCapture, start)
    }
  }

  def parseGroupBody(stack: mutable.Stack[Node], kind: GroupKind, start: Int): Unit = {
    val savedContext = context
    val node = parseDisjunction()
    context = savedContext
    if (isEnd || currentChar != ')') {
      fail("Unclosed group")
    }
    next()
    val end = offset
    stack.push(Node.Group(kind, node, Some(SourceLocation(start, end))))
  }

  def pushCommand(stack: mutable.Stack[Node], kind: CommandKind, start: Int): Unit = {
    val node = parseDisjunction()
    if (isEnd || currentChar != ')') {
      fail("Unclosed group")
    }
    next()
    val end = offset
    stack.push(Node.Command(kind, Some(SourceLocation(start, end))))
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

  def protectContext(body: => Unit): Unit = {
    val savedContext = context
    body
    context = savedContext
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
        val start = offset
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
    currentChar = source.charAt(offset)
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
