package codes.quine.labs.resyntax.parser

import scala.annotation.switch
import scala.annotation.tailrec
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
import codes.quine.labs.resyntax.ast.CaseForm
import codes.quine.labs.resyntax.ast.CasePart
import codes.quine.labs.resyntax.ast.CaseRegionEnd
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
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
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.CompatibilityPropertyTable
import codes.quine.labs.resyntax.charset.PropertyTable
import codes.quine.labs.resyntax.charset.UTF16Encoding
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.DotNetCharClass
import codes.quine.labs.resyntax.dialect.LineTerminatorSet
import codes.quine.labs.resyntax.dialect.PerlSource
import codes.quine.labs.resyntax.parser.Parser.ICUClassState
import codes.quine.labs.resyntax.parser.Parser.ICUClassToken
import codes.quine.labs.resyntax.parser.Parser.ParsingContext

object Parser:

  /** The maximum nesting depth of a pattern.
    *
    * A group, a look-around condition, a class, an operand of a class set operation, and a repeat of a repeat nest one
    * deeper, and a deeper pattern throws [[NestingLimitException]], so that the recursive analyses on the pattern do
    * not overflow the stack.
    */
  val MaxNestingDepth: Int = 150

  /** DecimalDigits is the decimal digits of Unicode (`Nd`), whose runs of ten start at zero. */
  private lazy val DecimalDigits: CharSet = PropertyTable.GeneralCategory.value("Nd").get.set

  /** PerlGroupNameStart is the characters that start a group name in Perl's UTF-8 pattern: `XID_Start` restricted to
    * `\\w`.
    */
  private lazy val PerlGroupNameStart: CharSet =
    PropertyTable.binary("XID_Start").get.set.intersect(CompatibilityPropertyTable.Word)

  /** PerlGroupNameContinue is the characters of a group name in Perl's UTF-8 pattern: `XID_Continue` restricted to
    * `\\w`.
    */
  private lazy val PerlGroupNameContinue: CharSet =
    PropertyTable.binary("XID_Continue").get.set.intersect(CompatibilityPropertyTable.Word)

  private[parser] final case class ParsingContext(
      inClass: Boolean,
      skipsComment: Boolean,
      verboseMore: Boolean,
      unixLines: Boolean,
      allowsDuplicatedGroupName: Boolean,
      explicitCapture: Boolean
  )

  private[parser] object ParsingContext:

    def from(featureSet: FeatureSet): ParsingContext = ParsingContext(
      inClass = false,
      skipsComment = featureSet.comment.skipsComment,
      verboseMore = featureSet.comment.verboseMore,
      unixLines = featureSet.unixLines,
      allowsDuplicatedGroupName = featureSet.group.allowsDuplicatedGroupName,
      explicitCapture = featureSet.group.explicitCapture
    )

  /** Parses the source in the dialect, whose unit mode is `mode` (see [[determineUnitMode]]).
    *
    * A Perl regex literal (the default of [[Dialect.Perl.source]]) is rewritten as Perl's lexer does first, and the
    * locations of the result point into the given source. A source that the engine reads as bytes (see
    * [[SourceEncoding]]) is read by its bytes, and the locations of a byte point to its character.
    */
  def parse(source: String, flagSet: FlagSet, dialect: Dialect, mode: UnitMode): Node =
    val encoding = FeatureSet.from(SourcePrescan.from(source), dialect, flagSet, mode).sourceEncoding
    validateEncoding(source, encoding)
    val bytes = if encoding == SourceEncoding.UTF8 then Rewritten.utf8(source) else None
    val text = bytes.fold(source)(_.text)
    val rewritten = dialect match
      case Dialect.Perl(_, PerlSource.Literal) =>
        PerlLiteralRewriter.rewrite(text, flagSet.verbose, bytes = encoding == SourceEncoding.UTF8)
      case _ => None
    val rewrites = rewritten.toSeq ++ bytes.toSeq
    try rewrites.foldLeft(parseText(rewritten.fold(text)(_.text), flagSet, dialect, mode))((n, r) => r.relocate(n))
    catch
      case ex: ParsingException if rewrites.nonEmpty =>
        throw new ParsingException(ex.message, ex.pos.map(pos => rewrites.foldLeft(pos)((p, r) => r.position(p))))
      case ex: NestingLimitException if rewrites.nonEmpty =>
        throw new NestingLimitException(ex.limit, rewrites.foldLeft(ex.pos)((p, r) => r.position(p)))

  /** Determines the unit of the input of the pattern, which is `IRPattern.mode` of its IR, without parsing the source.
    *
    * It reads only PCRE's start options of the source.
    */
  def determineUnitMode(source: String, flagSet: FlagSet, dialect: Dialect): UnitMode =
    dialect match
      case Dialect.JavaScript =>
        if flagSet.unicode || flagSet.unicodeSets then UnitMode.UTF16CodePoint else UnitMode.UTF16
      case Dialect.Java | Dialect.ICU => UnitMode.UTF16CodePoint
      case Dialect.DotNet             => UnitMode.UTF16
      case Dialect.Python(tpe)        =>
        if tpe == Dialect.Python.Type.Bytes then UnitMode.Byte else UnitMode.CodePoint
      case Dialect.PythonRegex(tpe) =>
        if tpe == Dialect.Python.Type.Bytes then UnitMode.Byte else UnitMode.CodePoint
      // Ruby's `n` is the encoding option (NOENCODING), which `FlagSet` keeps as `explicitCapture`.
      case Dialect.Ruby(encoding) =>
        if flagSet.explicitCapture || encoding == Dialect.Ruby.Encoding.ASCII8Bit then UnitMode.Byte
        else UnitMode.ScalarValue
      case Dialect.Perl(utf8, _) => if utf8 then UnitMode.CodePoint else UnitMode.Byte
      case Dialect.PCRE          =>
        if flagSet.unicode || SourcePrescan.from(source).hasUTFStartOption then UnitMode.ScalarValue
        else UnitMode.Byte
      case Dialect.BoostRegex(tpe) =>
        if tpe == Dialect.BoostRegex.Type.Char then UnitMode.Byte else UnitMode.ScalarValue

  private def validateEncoding(source: String, encoding: SourceEncoding): Unit =
    if encoding == SourceEncoding.Latin1 then
      val pos = source.indexWhere(_ > 0xff)
      if pos >= 0 then throw new ParsingException("Non-Latin-1 character in bytes pattern", Some(pos))

  private def parseText(source: String, flagSet: FlagSet, dialect: Dialect, mode: UnitMode): Node =
    val prescan = SourcePrescan.from(source)
    val featureSet = FeatureSet.from(prescan, dialect, flagSet, mode)
    val parser = new Parser(source, prescan, featureSet)
    // python-regex parses the pattern again in `V1` from the start, when it meets a `V1` inline flag.
    try parser.parse()
    catch case _: Version1Restart => parseText(source, flagSet.copy(version1 = true), dialect, mode)

  /** Version1Restart is thrown at a `V1` inline flag of a python-regex pattern parsed in `V0`. */
  private[parser] final class Version1Restart extends Exception("V1 inline flag")

  /** ICUClassToken is a token of ICU's class as ICU's scanner reads it. */
  private[parser] enum ICUClassToken:

    /** Character is a character, which is `quoted` if an escape makes it a literal for sure (e.g. `\x41`). */
    case Character(item: ClassItem.Value, quoted: Boolean)

    /** Quote is `\Q...\E` of characters, whose region has the literals for sure. */
    case Quote(item: ClassItem)

    /** Escape is a backslash and a character that the set states read (e.g. `w` of `\w`), which is not consumed yet. */
    case Escape(letter: Char)

    case End

  /** ICUClassState is what ICU's class has read last, which decides how a `-` or `&` after it reads. */
  private[parser] enum ICUClassState:

    /** Start is the start of the class, where `--` and `&&` are errors. */
    case Start

    /** Literal is a character or a quote, whose last character can begin a range. */
    case Literal(item: ClassItem)

    /** Set is a nested class or a property, which a `-` or `&` and another nested class follow as an operator. */
    case Set

    /** Range is a range or an escape class, which a `-` or `&` follows as a character. */
    case Range

    /** Operator is right after `--` or `&&`, where a `-` or `&` is a character. */
    case Operator

private[parser] final class Parser(
    private val source: String,
    private val prescan: SourcePrescan,
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

  private val groupNames: mutable.Map[String, Vector[List[(Int, Int, Boolean)]]] = mutable.Map.empty

  /** The number of the capture groups opened so far, where a branch reset counts its alternative with the most. */
  private var previousCaptureCount: Int = 0

  /** The offset of the first reference by a group number, which Ruby rejects in a pattern with named captures. */
  private var numberedReferenceOffset: Option[Int] = None

  /** Ruby's `\u{X Y}` just read, whose first code point is read as an escape, until the caller takes it. */
  private var pendingUnicodeList: Option[PendingUnicodeList] = None

  /** PendingUnicodeList is the code points of Ruby's `\u{X Y}`, and the text after `\u` unless it is the plain text. */
  private final case class PendingUnicodeList(values: Seq[Int], written: Option[String])

  /** The `(?#...)` ranges that Ruby removes from the source before parsing, as maps from start to end and back. */
  private val removedCommentEnds: Map[Int, Int] =
    if featureSet.comment.removesCommentGroup then scanRemovedComments().toMap else Map.empty

  private val removedCommentStarts: Map[Int, Int] = removedCommentEnds.map(_.swap)

  /** The end of the last removed comment that is already in the result. */
  private var removedCommentEmittedEnd: Int = 0

  /** The nesting depth limited by [[Parser.MaxNestingDepth]]. */
  private var depth: Int = 0

  /** The inline flags between a repeated node and its quantifier, which python-regex keeps after the repeat. */
  private var skippedInlineFlags: List[Node] = Nil

  reset(0)

  def parse(): Node =
    val node = parseDisjunction()
    if !isEnd then
      assert(currentChar == ')', "Unexpected")
      fail("Unmatched ')'")
    if featureSet.reference.rejectsNumberedReferenceWithNamedCapture && groupNames.nonEmpty then
      for pos <- numberedReferenceOffset do fail("Numbered reference with named capture", pos)
    node

  private def parseDisjunction(isBranchReset: Boolean = false): Node =
    val start = offset
    val builder = Seq.newBuilder[Node]
    val id = disjunctionCount
    disjunctionCount += 1
    val savedAlternativePath = alternativePath
    val captureCountAtStart = previousCaptureCount
    var maxCaptureCount = previousCaptureCount
    var index = 0
    alternativePath = (id, index, isBranchReset) :: savedAlternativePath
    builder.addOne(parseSequence())

    while !isEnd && (currentChar == '|' || isNewlineAlternation) do
      next()
      index += 1
      alternativePath = (id, index, isBranchReset) :: savedAlternativePath
      if isBranchReset then
        maxCaptureCount = maxCaptureCount.max(previousCaptureCount)
        previousCaptureCount = captureCountAtStart
      builder.addOne(parseSequence())
    alternativePath = savedAlternativePath
    if isBranchReset then previousCaptureCount = maxCaptureCount.max(previousCaptureCount)

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
    pushRemovedComments(stack)
    sequenceStart = savedSequenceStart

    val nodes = stack.toSeq
    if nodes.size == 1 then return nodes.head

    val end = offset
    Node(NodeData.Sequence(nodes.reverse), SourceLocation(start, end))

  private def parseSequenceItem(stack: mutable.Stack[Node]): Unit =
    pushRemovedComments(stack)
    if context.skipsComment && parseBlank(stack) then return

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
    val textStart = quantifierTextStart(stack)
    val node = popRepeatable(stack)
    next()
    val strategy = parseBacktrackStrategy(exact = false)
    val quantifier = buildQuantifier(strategy)
    pushRepeat(stack, node, quantifier, textStart)

  /** Returns the start of the text of a quantifier here, which has the blanks and comments before it. */
  private def quantifierTextStart(stack: mutable.Stack[Node]): Int =
    stack.takeWhile(isComment).lastOption.fold(offset)(_.loc.start)

  private def parseCurlyQuantifier(stack: mutable.Stack[Node]): Unit =
    val save = offset
    val textStart = quantifierTextStart(stack)
    if stack.forall(isComment) && featureSet.quantifier.readsLeadingCurlyAsLiteral then
      resetBracket(stack, save)
      return

    next()
    skipQuantifierBlank()
    skipQuantifierComment()
    val minOption = parseRepeatCountOption()
    skipQuantifierBlank()
    skipQuantifierComment()

    if minOption.isDefined && currentChar == '}' then
      next()
      val strategy = parseBacktrackStrategy(exact = true)
      pushCurlyRepeat(stack, Quantifier.Exact(minOption.get, strategy), textStart)
      return

    if isEnd || currentChar != ',' || minOption.isEmpty && !featureSet.quantifier.hasMaxBounded then
      resetBracket(stack, save)
      return

    next()
    skipConstructComment()
    skipQuantifierBlank()
    val maxOption = parseRepeatCountOption()
    skipQuantifierBlank()
    skipQuantifierComment()

    if isEnd || currentChar != '}' || minOption.isEmpty && maxOption.isEmpty && !featureSet.quantifier.allowsCurlyWithoutCount
    then
      resetBracket(stack, save)
      return

    next()
    val strategy = parseBacktrackStrategy(exact = false, curly = true)
    val quantifier = (minOption, maxOption) match
      case (Some(min), Some(max)) =>
        if max < min && !featureSet.quantifier.allowsReversedBoundedRepeat then
          fail("Numbers out of order in {} quantifier", save)
        Quantifier.Bounded(min, max, strategy)
      case (Some(min), None) => Quantifier.Unbounded(min, strategy)
      case (None, Some(max)) => Quantifier.MaxBounded(max, strategy)
      case (None, None)      => Quantifier.Unbounded(0, strategy)
    pushCurlyRepeat(stack, quantifier, textStart)

  private def pushCurlyRepeat(stack: mutable.Stack[Node], quantifier: Quantifier, textStart: Int): Unit =
    stack.find(!isComment(_)) match
      // Java ignores a curly quantifier following another quantifier or nothing: `a{2}{3}` matches `aa`.
      case Some(node @ Node(repeat: NodeData.Repeat, _)) if featureSet.quantifier.ignoresCurlyAfterRepeat =>
        while stack.headOption.exists(isComment) do stack.pop()
        stack.pop()
        stack.push(Node(withIgnoredCurly(node, repeat), node.loc.copy(end = offset)))
      case Some(
            node @ Node(
              region @ NodeData.CaseRegion(_, init :+ CasePart(last @ Node(repeat: NodeData.Repeat, _), text), _),
              _
            )
          ) if featureSet.quantifier.ignoresCurlyAfterRepeat =>
        while stack.headOption.exists(isComment) do stack.pop()
        stack.pop()
        val part = CasePart(Node(withIgnoredCurly(last, repeat), last.loc.copy(end = offset)), text)
        stack.push(Node(region.copy(parts = init :+ part), node.loc.copy(end = offset)))
      // The blank before an ignored curly quantifier keeps its text (e.g. ` {2}` of the extended mode).
      case None if featureSet.quantifier.ignoresCurlyAfterRepeat =>
        stack.headOption match
          case Some(Node(NodeData.Blank(text), loc)) =>
            stack.pop()
            stack.push(Node(NodeData.Blank(text + writtenText(loc.end)), loc.copy(end = offset)))
          case _ => ()
      case _ =>
        pushRepeat(stack, popRepeatable(stack), quantifier, textStart)

  /** Returns the repeat with the text up to here, where Java ignores a curly quantifier. */
  private def withIgnoredCurly(node: Node, repeat: NodeData.Repeat): NodeData.Repeat =
    repeat.copy(written = Some(repeat.written.getOrElse(repeat.quantifier.plainText) + writtenText(node.loc.end)))

  private def parseRepeatCountOption(): Option[Int] =
    // Boost.Regex reads a negative or invalid number as none, which makes `a{1,-1}` `a{1,}`.
    if featureSet.numberSyntax != NumberSyntax.Digits then
      val value = parseTraitsNumber(10)
      return Option.when(value >= 0)(value.min(Int.MaxValue).toInt)
    parseSaturatedIntOption(featureSet.quantifier.maxRepeatCount, "Number too big in {} quantifier", offset)

  private def skipBraceBlank(force: Boolean = false): Unit =
    if force || featureSet.escape.allowsBlankInBrace then skipBlank()

  /** Skips the blanks in a quantifier, where Boost.Regex skips any whitespace (e.g. `a{ 1 ,\n2 }`). */
  private def skipQuantifierBlank(): Unit =
    if featureSet.comment.skipsSpaceInQuantifier then while isQuantifierSpace do next()
    else skipBraceBlank()

  /** Tests whether the current character is whitespace that Boost.Regex skips in a quantifier, which is also `\\n` with
    * `N`.
    */
  private def isQuantifierSpace: Boolean =
    isSpace || !isEnd && currentChar == '\n'

  /** Skips the comments of the extended mode in a quantifier (e.g. python-regex's `a{1, 2}` with `x`). */
  private def skipQuantifierComment(): Unit =
    if featureSet.comment.skipsCommentInQuantifier then skipConstructComment()

  private def skipBlank(): Unit =
    while !isEnd && (currentChar == ' ' || currentChar == '\t') do next()

  private def popRepeatable(stack: mutable.Stack[Node]): Node =
    while stack.headOption.exists(n => isComment(n) || featureSet.quantifier.repeatsAcrossInlineFlag && isInlineFlag(n))
    do
      val node = stack.pop()
      if isInlineFlag(node) then skippedInlineFlags ::= node
    assert(stack.nonEmpty, "Nothing to repeat")
    val node = stack.pop()
    validateRepeatable(repeatTarget(node))
    node

  /** Returns the node that a quantifier after the node repeats, which is the last code point of Ruby's `\u{...}` (e.g.
    * `\u{42}` of `\u{41 42}*`) or the last character of a quote (e.g. `b` of `\Qab\E*`).
    */
  private def repeatTarget(node: Node): Node = node.data match
    case NodeData.UnicodeList(nodes, _)   => nodes.last
    case NodeData.CaseRegion(_, parts, _) => parts.last.value
    case _                                => node

  private def validateRepeatable(node: Node): Unit =
    node.data match
      case NodeData.Group(_: GroupKind.PositiveLookAhead | _: GroupKind.NegativeLookAhead, _) =>
        assert(featureSet.quantifier.allowsLookAheadRepeat, "Nothing to repeat")
      case NodeData.Group(_: GroupKind.PositiveLookBehind | _: GroupKind.NegativeLookBehind, _) =>
        assert(featureSet.quantifier.allowsLookBehindRepeat, "Nothing to repeat")
      case NodeData.Caret | NodeData.Dollar =>
        assert(
          featureSet.quantifier.allowsZeroWidthAssertRepeat || featureSet.quantifier.allowsCaretDollarRepeat,
          "Nothing to repeat"
        )
      case NodeData.Backslash(_: BackslashKind.Assert) =>
        assert(featureSet.quantifier.allowsZeroWidthAssertRepeat, "Nothing to repeat")
      case NodeData.Repeat(_, _, _) =>
        assert(featureSet.quantifier.allowsNestedRepeat, "Nested quantifier")
      // Boost.Regex's `(?i)` is an empty group, which a quantifier repeats (e.g. `a(?i)+` matches `a`).
      case NodeData.Command(_: CommandKind.InlineFlag) =>
        assert(featureSet.quantifier.allowsInlineFlagRepeat, "Nothing to repeat")
      case NodeData.Command(
            _: CommandKind.ResetFlag | _: CommandKind.StartOption | CommandKind.Callout | _: CommandKind.CalloutInt |
            _: CommandKind.CalloutString
          ) =>
        fail("Nothing to repeat")
      case NodeData.Command(_: CommandKind.Fuzzy) =>
        fail("Nothing to repeat")
      case NodeData.Command(CommandKind.BacktrackControl(kind, _)) =>
        assert(
          featureSet.quantifier.allowsBacktrackControlRepeat || kind == Some(BacktrackControlKind.Accept),
          "Nothing to repeat"
        )
      case _ =>
        ()

  private def pushRepeat(stack: mutable.Stack[Node], node: Node, quantifier: Quantifier, textStart: Int): Unit =
    val written = writtenUnlessPlain(textStart, quantifier.plainText)
    node.data match
      case list @ NodeData.UnicodeList(nodes, _) =>
        val data = list.copy(nodes = nodes.init :+ repeat(nodes.last, quantifier, written))
        stack.push(Node(data, node.loc.copy(end = offset)))
      case region @ NodeData.CaseRegion(_, parts, _) =>
        val part = parts.last.copy(value = repeat(parts.last.value, quantifier, written))
        val data = region.copy(parts = parts.init :+ part, end = CaseRegionEnd.ClosedBeforeQuantifier)
        stack.push(Node(data, node.loc.copy(end = offset)))
      case _ => stack.push(repeat(node, quantifier, written))
    pushSkippedInlineFlags(stack)

  private def repeat(node: Node, quantifier: Quantifier, written: Option[String]): Node =
    validateRepeatNesting(node)
    Node(NodeData.Repeat(node, quantifier, written), node.loc.copy(end = offset))

  private def pushSkippedInlineFlags(stack: mutable.Stack[Node]): Unit =
    skippedInlineFlags.foreach(stack.push)
    skippedInlineFlags = Nil

  private def parseBacktrackStrategy(exact: Boolean, curly: Boolean = false): BacktrackStrategy =
    if isEnd || exact && !featureSet.quantifier.allowsMeaninglessBacktrackStrategy then return BacktrackStrategy.Greedy

    val save = offset
    if featureSet.comment.skipsCommentBeforeBacktrackStrategy then skipCommentBeforeBacktrackStrategy()
    // Boost.Regex skips only the whitespace of `x` there, so `a* #c\n?` repeats a repeat.
    if featureSet.comment.skipsSpaceBeforeBacktrackStrategy && context.skipsComment then
      while isQuantifierSpace do next()

    if !isEnd && currentChar == '?' then
      next()
      return BacktrackStrategy.Lazy

    // Ruby reads `+` after `{n,m}` as another repeat, so `a{2,3}+` is `(?:a{2,3})+`.
    val hasPossessive =
      if curly then featureSet.quantifier.hasPossessiveCurlyBacktrackStrategy
      else featureSet.quantifier.hasPossessiveBacktrackStrategy
    if hasPossessive && !isEnd && currentChar == '+' then
      next()
      return BacktrackStrategy.Possessive

    reset(save)
    BacktrackStrategy.Greedy

  /** Skips comments between a quantifier and its `?` or `+`, which some engines read as if they were not there. */
  private def skipCommentBeforeBacktrackStrategy(): Unit =
    var continue = true
    while continue do
      continue = context.skipsComment && skipComment()
      if featureSet.comment.skipsInlineCommentBeforeBacktrackStrategy && source.startsWith("(?#", offset) then
        val start = offset
        reset(offset + 2)
        parseCommentGroup(mutable.Stack.empty, start)
        continue = true

  /** Parses the whitespace and comments of the extended mode as a blank node, and returns whether it found any. */
  private def parseBlank(stack: mutable.Stack[Node]): Boolean =
    val start = offset
    val skipped = skipComment()
    if skipped then stack.push(Node(NodeData.Blank(sliceSource(start, offset)), SourceLocation(start, offset)))
    skipped

  /** Tests whether a quantifier after the node repeats the node before it, as after a comment (e.g. `a(?#c)*`).
    *
    * PCRE ignores an empty quote and `\E` out of a quote, and so does Java an empty quote (e.g. `a\Q\E*` is `a*`).
    */
  private def isComment(node: Node): Boolean =
    node.data match
      case NodeData.Command(_: CommandKind.Comment) | NodeData.Blank(_)                  => true
      case NodeData.CaseRegion(_, parts, _)                                              => parts.isEmpty
      case NodeData.Backslash(BackslashKind.CaseCommand(CaseCommandKind.EndCaseCommand)) => true
      case _                                                                             => false

  private def isInlineFlag(node: Node): Boolean =
    node.data match
      case NodeData.Command(_: CommandKind.InlineFlag) => true
      case _                                           => false

  private def resetBracket(stack: mutable.Stack[Node], save: Int): Unit =
    if featureSet.group.hasFuzzyConstraint && parseFuzzyConstraint(stack, save) then return
    if !featureSet.quantifier.allowsBrokenCurly then fail("Incomplete quantifier", save)
    if !featureSet.quantifier.allowsBrokenCurlyAfterAlphabeticBackslash && isAfterAlphabeticBackslash(stack, save) then
      fail("Unescaped left brace", save)
    reset(save)
    stack.push(Node(NodeData.Literal('{'), SourceLocation(save, save + 1)))
    next()

  /** Parses python-regex's fuzzy constraint on the node before the `{` at `start` (e.g. `a{e<=1}`), and returns whether
    * there is one.
    *
    * It follows `parse_fuzzy` of `_regex_core.py`: the constraint is items separated by `,`, and an optional `:` and a
    * test character class, and anything else after `{` is a literal `{`.
    */
  private def parseFuzzyConstraint(stack: mutable.Stack[Node], start: Int): Boolean =
    reset(start + 1)
    val constraints = mutable.Set.empty[Char]
    var hasCostEquation = false

    def startWithToken(s: String): Boolean =
      skipConstructComment()
      startWith(s)

    def digits(): String =
      val builder = new StringBuilder
      skipConstructComment()
      while isDigit do
        builder.append(currentChar)
        next()
        skipConstructComment()
      builder.result()

    def compare(): Option[Boolean] =
      if startWithToken("<=") then Some(true) else if startWithToken("<") then Some(false) else None

    def costLimit(inclusive: Boolean): BigInt =
      val pos = offset
      val value = digits()
      if value.isEmpty then fail("Bad fuzzy cost limit", pos)
      BigInt(value) - (if inclusive then 0 else 1)

    def isConstraint(c: Char): Boolean = "deis".contains(c) && !constraints.contains(c)

    // `parse_cost_constraint`: `e`, `e<=1` or `1<=e<=2`.
    def costConstraint(): Boolean =
      skipConstructComment()
      if isAlphabetic then
        val c = currentChar
        next()
        if !isConstraint(c) then return false
        for inclusive <- compare() do
          val pos = offset
          if costLimit(inclusive) < 0 then fail("Bad fuzzy cost limit", pos)
        constraints.add(c)
        true
      else if isDigit then
        val min0 = costLimit(inclusive = true)
        compare() match
          case None               => false
          case Some(minInclusive) =>
            skipConstructComment()
            if isEnd || !isConstraint(currentChar) then return false
            val c = currentChar
            next()
            compare() match
              case None               => false
              case Some(maxInclusive) =>
                val pos = offset
                val max = costLimit(maxInclusive)
                val min = min0 + (if minInclusive then 0 else 1)
                if !(0 <= min && min <= max) then fail("Bad fuzzy cost limit", pos)
                constraints.add(c)
                true
      else false

    // `parse_cost_equation`: `2i+2d<=4`.
    def costEquation(): Boolean =
      if hasCostEquation then fail("More than one cost equation")
      val costs = mutable.Set.empty[Char]
      def term(): Boolean =
        digits()
        skipConstructComment()
        if isEnd || !"dis".contains(currentChar) then return false
        if costs.contains(currentChar) then fail("Repeated fuzzy cost")
        costs.add(currentChar)
        next()
        true

      var hasTerms = term()
      while hasTerms && startWithToken("+") do hasTerms = term()
      if !hasTerms then return false
      compare() match
        case None            => false
        case Some(inclusive) =>
          val pos = offset
          if costLimit(inclusive) < 0 then fail("Bad fuzzy cost limit", pos)
          hasCostEquation = true
          true

    def item(): Boolean =
      val save = offset
      costConstraint() || {
        reset(save)
        costEquation()
      }

    val itemsStart = offset
    var isFuzzy = item()
    while isFuzzy && startWithToken(",") do isFuzzy = item()
    if !isFuzzy then
      reset(start)
      return false

    if startWithToken(":") then parseFuzzyTest()
    skipConstructComment()
    val constraint = sliceSource(itemsStart, offset)
    expect('}', "Expected }")
    val node = popRepeatable(stack)
    validateRepeatable(node)
    stack.push(Node(NodeData.Command(CommandKind.Fuzzy(node, constraint)), node.loc.copy(end = offset)))
    pushSkippedInlineFlags(stack)
    true

  /** Parses the test of python-regex's fuzzy constraint after `:`, which is a character, an escape, `.` or a class. */
  private def parseFuzzyTest(): Unit =
    skipConstructComment()
    if isEnd || "()|?*+{^$#".contains(currentChar) then fail("Expected character set")
    (currentChar: @switch) match
      case '\\' => parseBackslash(mutable.Stack.empty)
      case '['  => parseClass(mutable.Stack.empty)
      case _    => nextCodePoint(currentCodePoint)

  private def isAfterAlphabeticBackslash(stack: mutable.Stack[Node], pos: Int): Boolean =
    stack.headOption.exists: node =>
      node.loc.end == pos && isAlphabetic(source.charAt(pos - 1)) && (node.data match
        case _: NodeData.Backslash => node.loc.start + 2 == pos
        // Only a literal is another node ending with a letter.
        // Perl looks only at the source text, so it also rejects a letter after an escaped backslash (e.g. `\\p{`).
        case _ => node.loc.start + 1 == pos && pos >= 2 && source.charAt(pos - 2) == '\\')

  private def parseGroup(stack: mutable.Stack[Node]): Unit =
    val start = offset
    next()
    val bodyStart = offset
    if featureSet.comment.skipsCommentInGroupOpening then skipConstructComment()
    (currentChar: @switch) match
      case '?' =>
        parseExtendedGroup(stack, start)
      case '*' =>
        if featureSet.group.hasVerbGroup then parseVerbGroup(stack, start)
        else if featureSet.group.hasBareVerbGroup then parseBoostRegexVerbGroup(stack, start)
        else parseAlphabeticGroup(stack, start)
      case _ =>
        // Java skips whitespace and comments after `(` to find `?`, and they are in the body otherwise (e.g. `( a)`).
        reset(bodyStart)
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
      case '+' | '-' =>
        if featureSet.inlineFlag.allowsInlineFlagSign then parseSignedInlineFlagGroup(stack, start)
        else if currentChar == '+' then parsePlusGroup(stack, start)
        else parseMinusGroup(stack, start)
      case '^' =>
        parseResetFlagGroup(stack, start)
      // Java has no `(?#...)`, and reads flags after `(?` and a comment in the extended mode (e.g. `(?#c\n:a)`).
      case '#' =>
        if featureSet.comment.hasInlineComment then parseCommentGroup(stack, start)
        else parseInlineFlagGroup(stack, start)
      case '{' =>
        parseInlineCodeGroup(stack, start)
      case '?' =>
        parseEmbedCodeGroup(stack, start)
      case 'C' =>
        parseCalloutGroup(stack, start)
      case _ =>
        if featureSet.inlineFlag.allowsInlineFlagSign then parseSignedInlineFlagGroup(stack, start)
        else parseInlineFlagGroup(stack, start)

  private def parseNonCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    parseGroupBody(stack, GroupKind.NonCapture, start)

  private def parseBranchResetGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasBranchReset, "Invalid group")
    next()
    protectContext:
      val node = nest(start)(parseDisjunction(isBranchReset = true))
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
    skipConstructComment()
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
    assert(featureSet.group.hasNonAtomicLookAround, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.NonAtomicPositiveLookBehind(AssertNameStyle.Symbolic), start)

  private def parseAtomicGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasAtomicGroup, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.Atomic(AssertNameStyle.Symbolic), start)

  private def parseNonAtomicPositiveLookAheadGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasNonAtomicLookAround, "Invalid group")
    next()
    parseGroupBody(stack, GroupKind.NonAtomicPositiveLookAhead(AssertNameStyle.Symbolic), start)

  private def parseAbsenceGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasAbsenceOperator, "Invalid group")
    next()
    if isEnd || currentChar != '|' then
      parseGroupBody(stack, GroupKind.Absence, start)
      return

    next()
    if !isEnd && currentChar == ')' then
      pushCommand(stack, CommandKind.AbsenceRangeClear, start)
      return

    protectContext:
      val absent = nest(start)(protectContext(parseSequence()))
      if !isEnd && currentChar == '|' then
        next()
        val exp = nest(start)(protectContext(parseDisjunction()))
        pushCommand(stack, CommandKind.AbsenceExpression(absent, exp), start)
      else pushCommand(stack, CommandKind.AbsenceStopper(absent), start)

  private val LookAroundTests: Seq[(String, String, String, AssertNameStyle => GroupKind.LookAround)] = Seq(
    ("?=", "*positive_lookahead:", "*pla:", GroupKind.PositiveLookAhead(_)),
    ("?!", "*negative_lookahead:", "*nla:", GroupKind.NegativeLookAhead(_)),
    ("?<=", "*positive_lookbehind:", "*plb:", GroupKind.PositiveLookBehind(_)),
    ("?<!", "*negative_lookbehind:", "*nlb:", GroupKind.NegativeLookBehind(_))
  )

  private def parseConditionalGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasConditional, "Invalid group")
    next()
    nest(start)(parseConditionalGroupTest(stack, start))

  private def parseConditionalGroupTest(stack: mutable.Stack[Node], start: Int): Unit =
    boundary:
      val indexStart = offset
      for n <- parseIntOption() do
        assert(n != 0 || featureSet.group.allowsZeroIndexedCaptureTest, "Invalid condition", indexStart)
        markNumberedReference(indexStart)
        assert(
          source.charAt(indexStart) != '0' || featureSet.group.allowsLeadingZeroInCaptureTest,
          "Invalid condition",
          indexStart
        )
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Indexed(n), start)
        boundary.break()

      if featureSet.group.hasNamedCaptureTest && currentChar == '<' then
        next()
        val test = parseNamedCaptureTest(NameStyle.Angle, '>')
        closeCondition()
        parseConditionalGroupBody(stack, test, start)
        boundary.break()

      if featureSet.group.hasNamedCaptureTest && currentChar == '\'' then
        next()
        val test = parseNamedCaptureTest(NameStyle.Quote, '\'')
        closeCondition()
        parseConditionalGroupBody(stack, test, start)
        boundary.break()

      if featureSet.group.hasDefineTest && startWith("DEFINE)") then
        reset(offset - 1)
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Define, start)
        boundary.break()

      if featureSet.group.hasVersionTest && startWith("VERSION") then
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
          case c         => isDigit(c))
      if featureSet.group.hasRecursionTest && isRecursionTest then
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

      if featureSet.group.hasBareNamedCaptureTest && isGroupNameStart then
        val name = parseGroupNameBody()
        closeCondition()
        parseConditionalGroupBody(stack, ConditionalTest.Named(NameStyle.Bare, name), start)
        boundary.break()

      val isRelative = !isEnd && (currentChar == '+' || currentChar == '-')
      if featureSet.group.hasRelativeIndexedCaptureTest && isRelative then
        val minus = currentChar == '-'
        next()
        parseIntOption() match
          case Some(n) =>
            closeCondition()
            parseConditionalGroupBody(stack, ConditionalTest.Relative(if minus then -n else n), start)
            boundary.break()
          case None =>
            fail("Invalid condition")

      if featureSet.group.hasCallout && startWith("?C") then
        val callout = parseCallout()
        closeCondition()
        if context.skipsComment then skipComment()
        expect('(', "Invalid condition")
        for (symbolic, alphabetic, abbrev, la) <- LookAroundTests do
          if parseLookAroundTest(stack, start, symbolic, alphabetic, abbrev, la, Some(callout)) then boundary.break()
        fail("Invalid condition")

      if featureSet.group.hasLookAroundTest then
        for (symbolic, alphabetic, abbrev, la) <- LookAroundTests do
          if parseLookAroundTest(stack, start, symbolic, alphabetic, abbrev, la) then boundary.break()

      if featureSet.group.hasBareLookAroundTest then
        val testStart = offset - 1
        // .NET reads `(?(?:x)...)` and `(?(?>x)...)` as `(?(x)...)` because the condition is a look-ahead.
        if !startWith("?:") then startWith("?>")
        parseLookAroundConditional(stack, None, start, testStart)
        boundary.break()

      fail("Invalid condition")

  private def parseNamedCaptureTest(style: NameStyle, end: Char): ConditionalTest =
    if !featureSet.group.allowsNonNameCaptureTest then return ConditionalTest.Named(style, parseGroupName(end))

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
      if test == ConditionalTest.Define && !featureSet.group.allowsDefineAlternative then
        assert(isEnd || currentChar != '|', "Invalid condition")
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
      la: AssertNameStyle => GroupKind.LookAround,
      callout: Option[CommandKind] = None
  ): Boolean =
    val testStart = offset - 1
    if startWith(symbolic) then
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Symbolic)), start, testStart, callout)
      return true

    if featureSet.group.hasAlphabeticGroup && startWith(alphabetic) then
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Alphabetic)), start, testStart, callout)
      return true

    if featureSet.group.hasAlphabeticGroup && startWith(abbrev) then
      parseLookAroundConditional(stack, Some(la(AssertNameStyle.Abbrev)), start, testStart, callout)
      return true

    false

  private def parseLookAroundConditional(
      stack: mutable.Stack[Node],
      kind: Option[GroupKind.LookAround],
      start: Int,
      testStart: Int,
      callout: Option[CommandKind] = None
  ): Unit =
    val node = nest(testStart)(protectContext(parseDisjunction()))
    closeCondition()
    val test = ConditionalTest.LookAround(kind, node)
    parseConditionalGroupBody(stack, callout.fold(test)(ConditionalTest.CalloutLookAround(_, test)), start)

  private def parseAngleNamedCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasAngleNamedCapture, "Invalid group")
    parseNamedCaptureGroup(stack, '>', NameStyle.Angle, start)

  private def parseQuoteNamedCaptureGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasQuoteNamedCapture, "Invalid group")
    next()
    parseNamedCaptureGroup(stack, '\'', NameStyle.Quote, start)

  private def parseNamedCaptureGroup(stack: mutable.Stack[Node], end: Char, style: NameStyle, start: Int): Unit =
    if featureSet.group.hasBalanceGroup then
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
    assert(featureSet.group.hasPGroup || featureSet.reference.hasPCall, "Invalid group")
    next()
    skipConstructComment()
    (currentChar: @switch) match
      case '<' =>
        assert(featureSet.group.hasPGroup, "Invalid group")
        next()
        val name = parseGroupName(end = '>')
        addGroupName(name, start)
        parseGroupBody(stack, GroupKind.PNamedCapture(name), start)
      case '=' =>
        assert(featureSet.group.hasPGroup, "Invalid group")
        next()
        val name = parseReferenceName()
        pushCommand(stack, CommandKind.PBackReference(name), start)
      case '>' =>
        assert(featureSet.reference.hasPCall, "Invalid group")
        next()
        val name = parseGroupNameBody()
        pushCommand(stack, CommandKind.PNamedCall(name), start)
      case '&' =>
        assert(featureSet.reference.hasPAmpersandCall, "Invalid group")
        next()
        val name = parseGroupNameBody()
        pushCommand(stack, CommandKind.PNamedCall(name), start)
      case _ =>
        fail("Invalid group")

  private def addGroupName(name: String, start: Int): Unit =
    val paths = groupNames.getOrElse(name, Vector.empty)
    if !context.allowsDuplicatedGroupName then
      for path <- paths do
        val exclusive = path.exists: (id, index, isBranchReset) =>
          (isBranchReset || featureSet.group.allowsDuplicatedGroupNameInAlternatives) &&
            alternativePath.exists((id2, index2, _) => id == id2 && index != index2)
        assert(exclusive, "Duplicated group name", start)
    groupNames(name) = paths :+ alternativePath

  private def parseRGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.reference.hasRCall, "Invalid group")
    next()
    pushCommand(stack, CommandKind.RCall, start)

  private def parseNamedCallGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.reference.hasCallCommand, "Invalid group")
    next()
    val name = parseGroupNameBody()
    pushCommand(stack, CommandKind.NamedCall(name), start)

  private def parseIndexedCallGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.reference.hasCallCommand, "Invalid group")
    val n = parseIntOption().getOrElse(fail("Invalid group"))
    pushCommand(stack, CommandKind.IndexedCall(n), start)

  private def parsePlusGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.reference.hasCallCommand, "Invalid group")
    next()
    val n = parseIntOption().getOrElse(fail("Invalid group"))
    pushCommand(stack, CommandKind.RelativeCall(n), start)

  private def parseMinusGroup(stack: mutable.Stack[Node], start: Int): Unit =
    val flagsStart = offset
    next()
    parseIntOption() match
      case Some(n) =>
        assert(featureSet.reference.hasCallCommand, "Invalid group")
        pushCommand(stack, CommandKind.RelativeCall(-n), start)
      case None =>
        assert(featureSet.inlineFlag.hasInlineFlagGroup, "Invalid group")
        val removedStart = offset
        val (removed, removedFlags) = parseRemovedFlagSet(context.skipsComment)
        validateInlineFlagRemoval("", removedFlags, removedStart)
        val diff = flagSetDiff(FlagSet(), Some(removed), flagsStart)
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
    assert(featureSet.inlineFlag.hasResetFlag, "Invalid group")
    next()
    val flagsStart = offset
    val (flagSet, _) = parseFlagSet(resetting = true)
    val written = writtenUnlessPlain(flagsStart, flagSet.letters)
    (currentChar: @switch) match
      case ')' =>
        pushCommand(stack, CommandKind.ResetFlag(flagSet, written), start)
        applyResetFlag(flagSet)
      case ':' =>
        next()
        applyResetFlagWith(flagSet):
          parseGroupBody(stack, GroupKind.ResetFlag(flagSet, written), start)
      case _ =>
        fail("Invalid group")

  private def parseCommentGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.comment.hasInlineComment, "Invalid group")
    next()
    val textStart = offset
    while !isEnd && currentChar != ')' do
      if featureSet.comment.processBackslashInCommentGroup && currentChar == '\\' then
        next()
        if !isEnd then next()
      else next()
    val text = source.slice(textStart, offset)
    // Boost.Regex's `(?#` without `)` is a comment to the end.
    if isEnd && featureSet.group.allowsUnclosedCommentGroup then
      stack.push(Node(NodeData.Command(CommandKind.Comment(text)), SourceLocation(start, offset)))
      return
    assert(!isEnd, "Invalid group")
    pushCommand(stack, CommandKind.Comment(text), start)

  /** Pushes the removed comments that end right before the current position, which are at a sequence position. */
  private def pushRemovedComments(stack: mutable.Stack[Node]): Unit =
    if removedCommentStarts.isEmpty then return
    var comments = List.empty[Node]
    var end = offset
    while end > removedCommentEmittedEnd && removedCommentStarts.contains(end) do
      val start = removedCommentStarts(end)
      comments ::= Node(
        NodeData.Command(CommandKind.Comment(source.slice(start + 3, end - 1))),
        SourceLocation(start, end)
      )
      end = start
    if comments.nonEmpty then
      comments.foreach(stack.push)
      removedCommentEmittedEnd = offset

  /** Finds the `(?#...)` ranges as Ruby's `unescape_nonascii0` in `re.c` does. */
  private def scanRemovedComments(): Seq[(Int, Int)] =
    val ranges = Seq.newBuilder[(Int, Int)]

    def skipEscape(i: Int): Int =
      source.charAt(i) match
        case 'c' | 'C' | 'M'                       => skipControlEscape(i)
        case c if UTF16Encoding.isHighSurrogate(c) => i + 2
        case _                                     => i + 1

    // It follows `read_escaped_byte` because `\cX`, `\C-X` and `\M-X` consume `X` even if it is `(` or `[`.
    def skipControlEscape(start: Int): Int =
      var i = start
      while i < source.length do
        val c = source.charAt(i)
        i += 1
        if c == 'M' || c == 'C' then
          if i >= source.length || source.charAt(i) != '-' then return i
          i += 1
        if c != 'M' && c != 'C' && c != 'c' || i >= source.length then return i
        if source.charAt(i) != '\\' then return i + 1
        i += 1
      i

    def scan(start: Int, initialExtended: Boolean, recurse: Boolean): Int =
      var i = start
      var inClass = 0
      var parens = 1
      var extended = initialExtended
      while i < source.length do
        val c = source.charAt(i)
        i += 1
        c match
          case '\\' =>
            if i < source.length then i = skipEscape(i)
          case '#' if extended && inClass == 0 =>
            while i < source.length && source.charAt(i) != '\n' do i += 1
            if i < source.length then i += 1
          case '[' =>
            inClass += 1
          case ']' =>
            if inClass > 0 then inClass -= 1
          case ')' =>
            if inClass == 0 && recurse then
              parens -= 1
              if parens == 0 then return i
          case '(' if inClass == 0 && i + 1 < source.length && source.charAt(i) == '?' =>
            if source.charAt(i + 1) == '#' then
              var j = i + 2
              var closed = false
              while !closed && j < source.length do
                val d = source.charAt(j)
                j += 1
                if d == ')' then closed = true
                else if d == '\\' && j < source.length then
                  j += (if UTF16Encoding.isHighSurrogate(source.charAt(j)) then 2 else 1)
              // An unterminated `(?#` is left as is, so it is a syntax error.
              if closed then
                ranges.addOne((i - 1, j))
                i = j
            else
              if recurse then parens += 1
              var invert = false
              var localExtended = 0
              var k = i + 1
              boundary:
                while k < source.length do
                  source.charAt(k) match
                    case 'x'                         => localExtended = if invert then -1 else 1
                    case '-'                         => invert = true
                    case 'i' | 'm' | 'a' | 'd' | 'u' => ()
                    case d @ (':' | ')')             =>
                      if localExtended != 0 && (localExtended == 1) != extended then
                        if d == ':' then i = scan(i, localExtended == 1, recurse = true)
                        else extended = localExtended == 1
                      boundary.break()
                    case _ => boundary.break()
                  k += 1
          case '(' =>
            if inClass == 0 && recurse then parens += 1
          case _ => ()
      i

    scan(0, featureSet.comment.skipsComment, recurse = false)
    ranges.result()

  private def parseInlineCodeGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasInlineCode, "Invalid group")
    next()
    val code = parseCode(end = '}')
    pushCommand(stack, CommandKind.InlineCode(code), start)

  private def parseEmbedCodeGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasInlineCode, "Invalid group")
    next()
    expect('{', "Invalid group")
    val code = parseCode(end = '}')
    pushCommand(stack, CommandKind.EmbedCode(code), start)

  private def parseCalloutGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasCallout, "Invalid group")
    next()
    pushCommand(stack, parseCallout(), start)

  private def parseCallout(): CommandKind =
    (currentChar: @switch) match
      case '`' | '\'' | '"' | '^' | '%' | '#' | '$' =>
        val delim = currentChar
        next()
        val value = parseCode(end = delim, escapeDouble = true)
        CommandKind.CalloutString(delim, delim, value)
      case '{' =>
        next()
        val value = parseCode(end = '}', escapeDouble = true)
        CommandKind.CalloutString('{', '}', value)
      case _ =>
        parseIntOption() match
          case Some(value) =>
            assert(value <= 255, "Invalid group")
            CommandKind.CalloutInt(value)
          case None =>
            CommandKind.Callout

  private def parseCode(end: Char, escapeDouble: Boolean = false): String =
    val start = offset
    def isDoubledEnd: Boolean = escapeDouble && source.startsWith(s"$end$end", offset)

    while !isEnd && (currentChar != end || isDoubledEnd) do
      if isDoubledEnd then next()
      next()
    assert(!isEnd, "Invalid group")
    val code = source.slice(start, offset)
    next()
    code

  private def parseInlineFlagGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.inlineFlag.hasInlineFlagGroup, "Invalid group")

    val flagsStart = offset
    val (added, addedFlags) = parseFlagSet()

    (currentChar: @switch) match
      case '-' =>
        next()
        val removedStart = offset
        val (removed, removedFlags) = parseRemovedFlagSet(context.skipsComment || added.verbose)
        validateInlineFlagRemoval(addedFlags, removedFlags, removedStart)
        restartAtVersion1Flag(added)
        val diff = flagSetDiff(added, Some(removed), flagsStart)
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
        restartAtVersion1Flag(added)
        val diff = flagSetDiff(added, None, flagsStart)
        next()
        applyFlagSetDiffWith(diff):
          parseGroupBody(stack, GroupKind.InlineFlag(diff), start)
      case ')' =>
        assert(addedFlags.nonEmpty || featureSet.inlineFlag.allowsEmptyInlineFlag, "Invalid flag")
        restartAtVersion1Flag(added)
        val diff = flagSetDiff(added, None, flagsStart)
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
    assert(offset > flagsStart || featureSet.inlineFlag.allowsEmptyInlineFlag, "Invalid flag")

    val diff = flagSetDiff(added, removed, flagsStart)
    (currentChar: @switch) match
      case ':' =>
        next()
        applyFlagSetDiffWith(diff):
          parseGroupBody(stack, GroupKind.InlineFlag(diff), start)
      case ')' =>
        pushInlineFlagCommand(stack, diff, start)
      case _ =>
        fail("Invalid flag")

  /** Returns the diff of the inline flags whose text starts at the position and ends here before `:` or `)`. */
  private def flagSetDiff(added: FlagSet, removed: Option[FlagSet], textStart: Int): FlagSetDiff =
    val diff = FlagSetDiff(added, removed)
    val plain = PlainText.flagSetDiff(diff, currentChar == ':', context.skipsComment, featureSet)
    diff.copy(written = writtenUnlessPlain(textStart, plain))

  /** Parses inline flag letters to remove, and Ruby also accepts more `-` among them (e.g. `(?i-m-x)`).
    *
    * `skipping` tells whether the extended mode is on before the letters.
    */
  private def parseRemovedFlagSet(skipping: Boolean): (FlagSet, String) =
    var (removed, removedFlags) = parseFlagSet(removing = true, skipping = skipping)
    while featureSet.inlineFlag.allowsRepeatedInlineFlagMinus && !isEnd && currentChar == '-' do
      next()
      val (flagSet, flags) = parseFlagSet(removing = true, initial = removed, skipping = skipping)
      removed = flagSet
      removedFlags += flags
    (removed, removedFlags)

  private def validateInlineFlagRemoval(addedFlags: String, removedFlags: String, removedStart: Int): Unit =
    if removedFlags.isEmpty then
      if addedFlags.isEmpty then assert(featureSet.inlineFlag.allowsEmptyInlineFlagDiff, "Invalid flag")
      else assert(featureSet.inlineFlag.allowsEmptyInlineFlagRemoval, "Invalid flag")
    if !featureSet.inlineFlag.allowsToggledInlineFlag then
      for (c, i) <- removedFlags.zipWithIndex do if addedFlags.contains(c) then fail("Invalid flag", removedStart + i)
    // python-regex's global flags (`b`, `e`, `p`, `r`, `V0` and `V1`) apply to the whole pattern.
    if featureSet.inlineFlag.rejectsGlobalInlineFlagRemoval then
      for (c, i) <- removedFlags.zipWithIndex do if "beprV".contains(c) then fail("Invalid flag", removedStart + i)

  /** Throws [[Parser.Version1Restart]] at python-regex's `V1` inline flag in `V0`, which parses the pattern again. */
  private def restartAtVersion1Flag(added: FlagSet): Unit =
    if featureSet.inlineFlag.restartsAtVersion1Flag && added.version1 then throw new Parser.Version1Restart

  private def pushInlineFlagCommand(stack: mutable.Stack[Node], diff: FlagSetDiff, start: Int): Unit =
    assert(featureSet.inlineFlag.hasInlineFlag, "Invalid group", start)
    if diff.removed.isDefined then assert(featureSet.inlineFlag.hasInlineFlagRemovalCommand, "Invalid group", start)
    if featureSet.inlineFlag.requiresInlineFlagAtStart then
      val atStart = sequenceStart == 0 && stack.forall:
        case Node(NodeData.Command(_: CommandKind.InlineFlag), _) => true
        case node                                                 => isComment(node)
      assert(atStart, "Inline flag not at the start", start)
    pushCommand(stack, CommandKind.InlineFlag(diff), start)
    if featureSet.inlineFlag.hasIncomprehensiveInlineFlag then parseOpaqueDisjunction(stack, diff, start)
    else applyFlagSetDiff(diff)

  private def parseOpaqueDisjunction(stack: mutable.Stack[Node], diff: FlagSetDiff, start: Int): Unit =
    boundary:
      applyFlagSetDiffWith(diff):
        val node = nest(start)(parseDisjunction())
        node.data match
          case s: NodeData.Sequence =>
            if s.nodes.isEmpty then boundary.break()
          case _ => ()
        stack.push(node)

  private val Verbs = Map(
    "FAIL" -> BacktrackControlKind.Fail,
    "F" -> BacktrackControlKind.F,
    "PRUNE" -> BacktrackControlKind.Prune,
    "SKIP" -> BacktrackControlKind.Skip
  )

  /** Parses python-regex's `(*VERB)`.
    *
    * Without a letter after `(*`, it is a capture group starting with `*`, which repeats nothing.
    */
  private def parseVerbGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    val nameStart = offset
    val name = new java.lang.StringBuilder
    skipConstructComment()
    while !isEnd && currentChar != ')' && currentChar != '>' do
      name.appendCodePoint(currentCodePoint)
      nextCodePoint(currentCodePoint)
      skipConstructComment()
    if name.length == 0 || !Character.isLetter(name.codePointAt(0)) then fail("Nothing to repeat", nameStart - 1)
    val kind = Verbs.getOrElse(name.toString, fail("Unknown verb", nameStart))
    pushCommand(stack, CommandKind.BacktrackControl(Some(kind), None), start)

  private val BoostRegexVerbs = Seq(
    "ACCEPT" -> BacktrackControlKind.Accept,
    "COMMIT" -> BacktrackControlKind.Commit,
    "FAIL" -> BacktrackControlKind.Fail,
    "F" -> BacktrackControlKind.F,
    "PRUNE" -> BacktrackControlKind.Prune,
    "SKIP" -> BacktrackControlKind.Skip,
    "THEN" -> BacktrackControlKind.Then
  )

  /** Parses Boost.Regex's `(*VERB)`, which has no name after the verb. */
  private def parseBoostRegexVerbGroup(stack: mutable.Stack[Node], start: Int): Unit =
    next()
    val (name, kind) = BoostRegexVerbs
      .find((name, _) => source.startsWith(s"$name)", offset))
      .getOrElse:
        fail("Invalid group", start)
    reset(offset + name.length)
    pushCommand(stack, CommandKind.BacktrackControl(Some(kind), None), start)

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
    if featureSet.group.hasNonAtomicLookAround then
      Seq(
        ("non_atomic_positive_lookahead:", "napla:", GroupKind.NonAtomicPositiveLookAhead(_)),
        ("non_atomic_positive_lookbehind:", "naplb:", GroupKind.NonAtomicPositiveLookBehind(_))
      )
    else Seq.empty
  }

  private def parseAlphabeticGroup(stack: mutable.Stack[Node], start: Int): Unit =
    assert(featureSet.group.hasAlphabeticGroup, "Invalid group")

    next()

    boundary:
      if featureSet.group.hasStartOption && start == startOptionEnd && parseStartOption(stack, start) then
        startOptionEnd = offset
        boundary.break()

      for (command, kind) <- BacktrackControls do
        if parseBacktrackControl(stack, start, command, kind) then boundary.break()

      for (alphabetic, abbrev, kind) <- AlphabeticGroups do
        if parseAlphabetic(stack, start, alphabetic, abbrev, kind) then boundary.break()

      if featureSet.group.hasScanSubstring then
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
          // PCRE2 accepts `(*TURKISH_CASING)` only in UTF mode (in the 8-bit library), by `u` or `(*UTF)`.
          if name == "TURKISH_CASING" then assert(featureSet.readsAsUnicode, "Invalid group")
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
    sliceSource(start, offset)

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
    kind match
      case GroupKind.IndexedCapture if !context.explicitCapture => previousCaptureCount += 1
      case _: GroupKind.NamedCapture | _: GroupKind.PNamedCapture | GroupKind.Balance(_, Some(_), _) =>
        previousCaptureCount += 1
      case _ => ()
    val node = nest(start)(protectContext(parseDisjunction()))
    closeGroup()
    validateGroupBody(kind, node, start)
    val end = offset
    stack.push(Node(NodeData.Group(kind, node), SourceLocation(start, end)))

  /** Checks that Boost.Regex's look-ahead or atomic group matches something, though `(?!)` and a look-behind may not
    * (e.g. `(?=)` and `(?>(?#c))` are errors).
    */
  private def validateGroupBody(kind: GroupKind, node: Node, start: Int): Unit =
    kind match
      case _: GroupKind.PositiveLookAhead | _: GroupKind.Atomic
          if featureSet.group.rejectsEmptyLookAheadAndAtomicGroup && isNothing(node) =>
        fail("Invalid or empty zero width assertion", start)
      case _ => ()

  /** Tests whether the node is nothing to Boost.Regex, which has no state for comments and `\Q\E`. */
  private def isNothing(node: Node): Boolean =
    node.data match
      case NodeData.Sequence(nodes) => nodes.forall(isNothing)
      case _                        => isComment(node)

  private def pushCommand(stack: mutable.Stack[Node], kind: CommandKind, start: Int): Unit =
    closeGroup()
    val end = offset
    stack.push(Node(NodeData.Command(kind), SourceLocation(start, end)))

  private def closeGroup(): Unit =
    expect(')', "Unclosed group")

  /** Parses inline flag letters, and returns the flag set and the source string of the letters.
    *
    * `skipping` tells whether the extended mode is on before the letters, where Java skips whitespace and comments
    * around the letters, and an `x` turns it on or off at once (e.g. `(?x i)`).
    */
  private def parseFlagSet(
      removing: Boolean = false,
      resetting: Boolean = false,
      initial: FlagSet = FlagSet(),
      skipping: Boolean = context.skipsComment
  ): (FlagSet, String) =
    val start = offset
    var flagSet = initial
    def result: (FlagSet, String) = (flagSet, sliceSource(start, offset))

    def skipFlagComment(): Unit =
      if isSkippingFlagComment(skipping, removing, flagSet) && featureSet.comment.skipsCommentInConstruct then
        skipComment()

    // Ruby's last character set flag wins (e.g. `(?au)` is `(?u)`).
    def charset(flagSet: FlagSet): FlagSet =
      if featureSet.inlineFlag.overridesCharsetInlineFlag then
        flagSet.copy(ascii = false, hasIndices = false, unicode = false)
      else flagSet

    skipFlagComment()
    while !isEnd do
      val flagStart = offset
      val c = if featureSet.inlineFlag.ignoresInlineFlagCase then currentChar.toLower else currentChar
      (c: @switch) match
        case 'i' =>
          flagSet = flagSet.copy(ignoreCase = true)
        case 'm' =>
          flagSet = flagSet.copy(multiline = true)
        case 'x' =>
          if !featureSet.inlineFlag.hasFlagX then return result
          flagSet = flagSet.copy(verbose = true)
          if featureSet.inlineFlag.hasFlagXX && sliceSource(start, offset).contains('x') then
            flagSet = flagSet.copy(verboseMore = true)
        case 'J' =>
          if !featureSet.inlineFlag.hasFlagUpperJ then return result
          flagSet = flagSet.copy(dupNames = true)
        case 'L' =>
          if !featureSet.inlineFlag.hasFlagUpperL then return result
          flagSet = flagSet.copy(localeUpper = true)
        case 'U' =>
          if !featureSet.inlineFlag.hasFlagUpperU then return result
          flagSet = flagSet.copy(ungreedy = true)
        // python-regex's version flags are `V0` and `V1`, between whose letters the extended mode skips comments.
        case 'V' =>
          if !featureSet.inlineFlag.hasFlagUpperV then return result
          val save = offset
          next()
          skipFlagComment()
          if isEnd || (currentChar != '0' && currentChar != '1') then
            reset(save)
            return result
          flagSet = if currentChar == '0' then flagSet.copy(version0 = true) else flagSet.copy(version1 = true)
        case 'a' =>
          if !featureSet.inlineFlag.hasFlagA then return result
          val subOption =
            if featureSet.inlineFlag.hasFlagASubOption then source.lift(offset + 1).filter("DPSTW".contains(_))
            else None
          flagSet = subOption match
            case Some('D') => flagSet.copy(asciiDigit = true)
            case Some('P') => flagSet.copy(asciiPOSIX = true)
            case Some('S') => flagSet.copy(asciiSpace = true)
            case Some('T') => flagSet.copy(asciiPOSIXDigit = true)
            case Some('W') => flagSet.copy(asciiWord = true)
            case _         => charset(flagSet).copy(ascii = true)
          if subOption.isDefined then next()
          else if featureSet.inlineFlag.hasFlagAA && sliceSource(start, offset).contains('a') then
            flagSet = flagSet.copy(asciiRestrict = true)
        case 'b' =>
          if !featureSet.inlineFlag.hasFlagB then return result
          flagSet = flagSet.copy(bestMatch = true)
        case 'c' =>
          if !featureSet.inlineFlag.hasFlagC then return result
          flagSet = flagSet.copy(continue = true)
        case 'd' =>
          if !featureSet.inlineFlag.hasFlagD then return result
          flagSet = charset(flagSet).copy(hasIndices = true)
        case 'e' =>
          if !featureSet.inlineFlag.hasFlagE then return result
          flagSet = flagSet.copy(evaluate = true)
        case 'f' =>
          if !featureSet.inlineFlag.hasFlagF then return result
          flagSet = flagSet.copy(fullCase = true)
        case 'g' =>
          if !featureSet.inlineFlag.hasFlagG then return result
          flagSet = flagSet.copy(global = true)
        case 'l' =>
          if !featureSet.inlineFlag.hasFlagL then return result
          flagSet = flagSet.copy(localeLower = true)
        case 'n' =>
          if !featureSet.inlineFlag.hasFlagN then return result
          flagSet = flagSet.copy(explicitCapture = true)
        case 'o' =>
          if !featureSet.inlineFlag.hasFlagO then return result
          flagSet = flagSet.copy(once = true)
        case 'p' =>
          if !featureSet.inlineFlag.hasFlagP then return result
          flagSet = flagSet.copy(preserve = true)
        case 'r' =>
          if !featureSet.inlineFlag.hasFlagR then return result
          flagSet = flagSet.copy(nonDestructive = true)
        case 's' =>
          if !featureSet.inlineFlag.hasFlagS then return result
          flagSet = flagSet.copy(dotAll = true)
        case 'u' =>
          if !featureSet.inlineFlag.hasFlagU then return result
          flagSet = charset(flagSet).copy(unicode = true)
        case 'w' =>
          if !featureSet.inlineFlag.hasFlagW then return result
          flagSet = flagSet.copy(word = true)
        case _ =>
          return result
      validateFlag(c, sliceSource(start, flagStart), removing, resetting)
      next()
      skipFlagComment()
    result

  /** Checks whether the extended mode is on among inline flag letters, which Java's `x` among them turns on or off at
    * once (e.g. `(?x i)`), but ICU's does not.
    */
  private def isSkippingFlagComment(skipping: Boolean, removing: Boolean, flagSet: FlagSet): Boolean =
    if featureSet.inlineFlag.readsInlineFlagsInOuterMode then skipping
    else if removing then skipping && !flagSet.verbose
    else skipping || flagSet.verbose

  private val CharsetFlags = "adluL"

  private def validateFlag(c: Char, previous: String, removing: Boolean, resetting: Boolean): Unit =
    if !featureSet.inlineFlag.allowsDuplicatedInlineFlag && previous.contains(c) then fail("Duplicated flag")
    if !CharsetFlags.contains(c) then return
    if removing && featureSet.inlineFlag.rejectsCharsetInlineFlagRemoval then fail("Invalid flag")
    // Perl rejects `(?^d)` because `d` is the default character set.
    if resetting && c == 'd' then fail("Invalid flag")
    if featureSet.inlineFlag.rejectsConflictingCharsetInlineFlag && previous.exists(p =>
        CharsetFlags.contains(p) && p != c
      )
    then fail("Invalid flag")
    // Perl accepts `a` up to twice (`aa`), and the other character set flags only once.
    if featureSet.inlineFlag.limitsCharsetInlineFlagCount && previous.count(_ == c) >= (if c == 'a' then 2 else 1) then
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
    if backslash == BackslashKind.CaseCommand(CaseCommandKind.QuoteCommand) then
      parseQuote(stack, start)
      return

    val loc = SourceLocation(start, offset)
    pendingUnicodeList match
      case Some(PendingUnicodeList(values, written)) =>
        pendingUnicodeList = None
        val escapes = values.map(value => NodeData.Backslash(BackslashKind.Escape(EscapeStyle.UnicodeBracket, value)))
        stack.push(Node(NodeData.UnicodeList(escapes.map(Node(_, loc)), written), loc))
      case None => stack.push(Node(NodeData.Backslash(backslash), loc))

  /** Parses the characters after `\Q` up to `\E` or the end, which Java and PCRE read as literals. */
  private def parseQuote(stack: mutable.Stack[Node], start: Int): Unit =
    val literals = mutable.Stack.empty[Node]
    while !isEnd && !source.startsWith("\\E", offset) do parseLiteral(literals)
    val parts = literals.reverseIterator.map(node => CasePart(node, Some(source.slice(node.loc.start, node.loc.end))))
    val end = if startWith("\\E") then CaseRegionEnd.Closed else CaseRegionEnd.Unclosed
    val region = NodeData.CaseRegion(CaseCommandKind.QuoteCommand, parts.toSeq, end)
    stack.push(Node(region, SourceLocation(start, offset)))

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
      case 'm'                                                 => parseBackslashLowerM()
      case 'n'                                                 => parseBackslashLowerN()
      case 'o'                                                 => parseBackslashLowerO()
      case 'p'                                                 => parseBackslashLowerP()
      case 'q'                                                 => parseBackslashLowerQ()
      case 'r'                                                 => parseBackslashLowerR()
      case 's'                                                 => parseBackslashLowerS()
      case 't'                                                 => parseBackslashLowerT()
      case 'u'                                                 => withoutRemovedComment(parseBackslashLowerU())
      case 'v'                                                 => parseBackslashLowerV()
      case 'w'                                                 => parseBackslashLowerW()
      case 'x'                                                 => withoutRemovedComment(parseBackslashLowerX())
      case 'z'                                                 => parseBackslashLowerZ()
      case 'A'                                                 => parseBackslashUpperA()
      case 'B'                                                 => parseBackslashUpperB()
      case 'C'                                                 => parseBackslashUpperC()
      case 'D'                                                 => parseBackslashUpperD()
      case 'E'                                                 => parseBackslashUpperE()
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
      case '<' | '>'                                           => parseBackslashAngle()
      case '`' | '\''                                          => parseBackslashQuoteAnchor()
      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        parseBackslashBackReference()
      case '/' | '|' | '+' | '*' | '?' | '{' | '}' | '(' | ')' | '^' | '$' | '.' | '\\' | '[' | ']' | '&' =>
        parseBackslashEscape()
      case _ => parseBackslashUnknown()

  private def parseBackslashLowerA(): BackslashKind =
    if !featureSet.escape.hasBackslashBell then return parseBackslashUnknown()

    next()
    BackslashKind.Escape(EscapeStyle.Single('a'), 0x0007)

  private def parseBackslashLowerB(): BackslashKind =
    if context.inClass then
      assert(featureSet.escape.hasBackslashBackspace, "Invalid backslash escape")
      return parseBackslashBackspace()

    parseBackslashBoundary()

  private def parseBackslashBackspace(): BackslashKind =
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
      if featureSet.escape.hasBoundaryGModifier && startWithBoundaryGModifier() then
        boundary.break(BackslashKind.Assert(AssertKind.Boundary(Some(BoundaryModifier.GModifier))))

      if featureSet.escape.hasBoundaryModifier then
        for (name, modifier) <- BoundaryModifiers do
          if startWith(name) then boundary.break(BackslashKind.Assert(AssertKind.Boundary(Some(modifier))))

      BackslashKind.Assert(AssertKind.Boundary(None))

  /** Reads `{g}` after `\\b`, where Java skips whitespace and comments before `{` and `}` but not before `g`. */
  private def startWithBoundaryGModifier(): Boolean =
    val save = offset
    skipConstructComment()
    if startWith("{g") then
      skipConstructComment()
      if startWith("}") then return true
    reset(save)
    false

  private def parseBackslashLowerC(): BackslashKind =
    if !featureSet.escape.hasBackslashControl then return parseBackslashUnknown()

    next()
    val textStart = offset
    skipEscapeComment()
    // ICU reads `\c` at the end of the pattern as the letter.
    if featureSet.escape.readsBackslashControlAtEndAsLetter && isEnd then return BackslashKind.Unknown('c')
    if featureSet.escape.hasBackslashMetaControl && !isEnd && currentChar == '\\' then
      val value = parseMetaControlTarget()
      return escape(EscapeStyle.Control('\\'), metaControlValue(value), textStart)

    if !isControl then
      assert(featureSet.escape.allowsBrokenBackslashControl, "Invalid backslash escape")
      return BackslashKind.Unknown('c')

    val char = currentChar
    next()

    val value =
      if featureSet.escape.readsBackslashControlLowBits then char & 0x1f
      else if featureSet.escape.hasBackslashControlAny then char ^ 0x40
      else if featureSet.escape.hasBackslashMetaControl then metaControlValue(char)
      else if isAlphabetic(char) || featureSet.charClass.hasBackslashControlDigitInClass then char % 32
      else char ^ 0x40
    escape(EscapeStyle.Control(char), value, textStart)

  private def metaControlValue(value: Int): Int =
    if value == '?' then 0x7f else value & 0x9f

  /** Parses the character after Ruby's `\c`, `\C-` or `\M-`, which Ruby reads before Onigmo.
    *
    * Ruby takes only a character or an escape of a byte there (`read_escaped_byte` in `re.c`), so not `\u` (e.g.
    * `\C-\u{41}`).
    */
  private def parseMetaControlTarget(): Int =
    assert(!isEnd, "Invalid backslash escape")
    if currentChar == '\\' then
      val save = offset
      next()
      parseBackslashKind() match
        case kind @ BackslashKind.Escape(EscapeStyle.Single('\\' | 'n' | 't' | 'r' | 'f' | 'v' | 'a' | 'e'), _, _) =>
          kind.value
        case kind @ BackslashKind.Escape(
              EscapeStyle.BareOctal | EscapeStyle.Hex2 | EscapeStyle.Hex1 | EscapeStyle.Control(_) |
              EscapeStyle.ControlDash | EscapeStyle.Meta,
              _,
              _
            ) =>
          kind.value
        case _ => fail("Invalid backslash escape", save)
    else
      assert(currentChar < 0x80, "Invalid backslash escape")
      val char = currentChar
      next()
      char.toInt

  private def parseBackslashLowerD(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.Digit)

  private def parseBackslashLowerE(): BackslashKind =
    if !featureSet.escape.hasBackslashEscape then return parseBackslashUnknown()

    next()
    BackslashKind.Escape(EscapeStyle.Single('e'), 0x1b)

  private def parseBackslashLowerF(): BackslashKind =
    next()
    BackslashKind.Escape(EscapeStyle.Single('f'), 0x0c)

  private def parseBackslashLowerG(): BackslashKind =
    if featureSet.reference.hasBackslashGKBackReference then return parseBackslashGKBackReference()
    if featureSet.reference.hasBackslashGAngleBackReference then return parseBackslashGAngleBackReference()
    if !(featureSet.reference.hasBackslashGBackReference || featureSet.reference.hasBackslashGCall) then
      return parseBackslashUnknown()
    if featureSet.reference.allowsNamelessBackslashKG && !source.lift(offset + 1).exists(c => c == '<' || c == '\'')
    then return parseBackslashUnknown()

    next()
    (currentChar: @switch) match
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        assert(featureSet.reference.hasBackslashGBackReference, "Invalid backslash escape")
        val index = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        BackslashKind.EscapeBackReference(
          BackReferenceStyle.GBackReference(NameStyle.Bare),
          Reference.IndexedReference(index)
        )
      case '+' | '-' =>
        assert(featureSet.reference.hasBackslashGBackReference, "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.GBackReference(NameStyle.Bare), parseRelativeReference())
      case '{' =>
        assert(featureSet.reference.hasBackslashGBackReference, "Invalid backslash escape")
        next()
        skipBraceBlank()
        val ref = parseBaseRef('}')
        skipBraceBlank()
        expect('}', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.GBackReference(NameStyle.Curly), ref)
      case '<' | '\'' =>
        assert(featureSet.reference.hasBackslashGCall, "Invalid backslash escape")
        val style = if currentChar == '<' then NameStyle.Angle else NameStyle.Quote
        val end = if style == NameStyle.Angle then '>' else '\''
        next()
        val ref = parseBaseRef(end)
        expect(end, "Invalid backslash escape")
        BackslashKind.EscapeCall(style, ref)
      case _ =>
        fail("Invalid backslash escape")

  /** Parses python-regex's `\\g<name>` back-reference, which is the letter `g` unless a name or a number follows. */
  private def parseBackslashGAngleBackReference(): BackslashKind =
    if context.inClass then return parseBackslashUnknown()

    next()
    val save = offset
    skipConstructComment()
    if !isEnd && currentChar == '<' then
      next()
      skipConstructComment()
      val ref =
        if isDigit then Some(parseIntOption().get).filter(_ >= 1).map(Reference.IndexedReference(_))
        else Option.when(isGroupNameStart)(Reference.NamedReference(parseGroupNameBody('>')))
      skipConstructComment()
      if ref.isDefined && !isEnd && currentChar == '>' then
        next()
        return BackslashKind.EscapeBackReference(BackReferenceStyle.GBackReference(NameStyle.Angle), ref.get)
    reset(save)
    BackslashKind.Unknown('g')

  private def parseBackslashLowerH(): BackslashKind =
    if featureSet.escape.hasBackslashBlank then
      next()
      return BackslashKind.EscapeClass(EscapeClassKind.POSIXBlank)

    if !(featureSet.escape.hasBackslashHorizontalSpace || featureSet.escape.hasBackslashHexDigit) then
      return parseBackslashUnknown()

    next()
    val klass =
      if featureSet.escape.hasBackslashHorizontalSpace then EscapeClassKind.Horizontal else EscapeClassKind.HexDigit
    BackslashKind.EscapeClass(klass)

  /** Parses Boost.Regex's `\\g` or `\\k`, which are the same back-reference to a number, a relative number or a name in
    * brackets (e.g. `\\g1`, `\\k-1`, `\\g{1}` and `\\k<name>`).
    */
  private def parseBackslashGKBackReference(): BackslashKind =
    val letter = currentChar
    next()
    val (nameStyle, end) = (if isEnd then ' ' else currentChar) match
      case '{'  => (NameStyle.Curly, Some('}'))
      case '<'  => (NameStyle.Angle, Some('>'))
      case '\'' => (NameStyle.Quote, Some('\''))
      case _    => (NameStyle.Bare, None)
    if end.isDefined then next()
    val negative = !isEnd && currentChar == '-'
    if negative then next()
    val start = offset
    val n = parseTraitsNumber(10)
    val ref =
      if n < 0 then
        // A name in brackets is not a number, but a relative one is an error (e.g. `\g{-a}`).
        assert(end.isDefined && !negative, "Invalid backslash escape")
        reset(start)
        while !isEnd && currentChar != end.get do next()
        Reference.NamedReference(sliceSource(start, offset))
      else
        markNumberedReference(start)
        assert(n.isValidInt, "Invalid backslash escape", start)
        // `\g{-0}` refers to the next group (e.g. `\g{-0}(a)` is `\1(a)`).
        if negative then Reference.RelativeReference(if n == 0 then 1 else -n.toInt)
        else
          assert(n > 0, "Invalid backslash escape", start)
          Reference.IndexedReference(n.toInt)
    for c <- end do expect(c, "Invalid backslash escape")
    val style =
      if letter == 'g' then BackReferenceStyle.GBackReference(nameStyle)
      else BackReferenceStyle.KBackReference(nameStyle)
    BackslashKind.EscapeBackReference(style, ref)

  private def parseBackslashLowerK(): BackslashKind =
    if featureSet.reference.hasBackslashGKBackReference then return parseBackslashGKBackReference()
    if !featureSet.reference.hasBackslashKBackReference || context.inClass then return parseBackslashUnknown()
    if featureSet.reference.allowsNamelessBackslashKG && !source.lift(offset + 1).exists(c => c == '<' || c == '\'')
    then return parseBackslashUnknown()

    next()
    skipConstructComment()
    (currentChar: @switch) match
      case '<' =>
        next()
        val ref = parseKRef('>')
        expect('>', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.KBackReference(NameStyle.Angle), ref)
      case '\'' =>
        assert(featureSet.reference.hasBackslashKBackReferenceQuote, "Invalid backslash escape")
        next()
        val ref = parseKRef('\'')
        expect('\'', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.KBackReference(NameStyle.Quote), ref)
      case '{' =>
        assert(featureSet.reference.hasBackslashKBackReferenceCurly, "Invalid backslash escape")
        next()
        skipBraceBlank()
        val ref = parseKRef('}')
        skipBraceBlank()
        expect('}', "Invalid backslash escape")
        BackslashKind.EscapeBackReference(BackReferenceStyle.KBackReference(NameStyle.Curly), ref)
      case _ =>
        fail("Invalid backslash escape")

  private def parseKRef(end: Char): Reference =
    if featureSet.reference.allowsNonNameKBackReference then return parseRef(end)
    // .NET accepts `\k<0>`, though it rejects `(?<0>...)`.
    if featureSet.group.allowsNumberGroupName && isDigit then
      val start = offset
      parseIntOption()
      return Reference.NamedReference(sliceSource(start, offset))
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

    if !featureSet.reference.hasLeveledBackReference then return ref

    (currentChar: @switch) match
      case '+' | '-' =>
        val sign = if currentChar == '-' then -1 else 1
        next()
        val level = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        Reference.LeveledReference(ref, sign * level)
      case _ => ref

  private def parseBaseRef(end: Char): Reference.BaseReference =
    if !featureSet.reference.allowsNonNameBaseRef then return Reference.NamedReference(parseGroupNameBody(end))

    (currentChar: @switch) match
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        val start = offset
        val index = parseIntOption().getOrElse(fail("Invalid backslash escape"))
        if index != 0 then markNumberedReference(start)
        Reference.IndexedReference(index)
      case '+' =>
        if featureSet.reference.allowsForwardRelativeReference then parseRelativeReference()
        else Reference.NamedReference(parseGroupNameBody(end))
      case '-' =>
        parseRelativeReference()
      case _ =>
        Reference.NamedReference(parseGroupNameBody(end))

  private def markNumberedReference(pos: Int): Unit =
    if numberedReferenceOffset.isEmpty then numberedReferenceOffset = Some(pos)

  private def parseRelativeReference(): Reference.RelativeReference =
    assert(currentChar == '-' || featureSet.reference.allowsForwardRelativeReference, "Invalid backslash escape")
    markNumberedReference(offset)
    val sign = if currentChar == '-' then -1 else 1
    next()
    val start = offset
    val n = parseIntOption().getOrElse(fail("Invalid backslash escape"))
    assert(n != 0, "Invalid backslash escape", start)
    Reference.RelativeReference(sign * n)

  private def parseBackslashLowerL(): BackslashKind =
    if !featureSet.escape.hasBackslashCaseClass then return parseBackslashUnknown()

    next()
    BackslashKind.EscapeClass(EscapeClassKind.Lower)

  private def parseBackslashLowerM(): BackslashKind =
    if !featureSet.escape.hasBackslashWordStartEnd then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.WordStart)

  private def parseBackslashLowerN(): BackslashKind =
    next()
    BackslashKind.Escape(EscapeStyle.Single('n'), 0x0a)

  private def parseBackslashLowerO(): BackslashKind =
    if !featureSet.escape.hasBackslashOctal then return parseBackslashUnknown()

    next()
    val textStart = offset
    expect('{', "Invalid backslash escape")
    val value = parseBracedCodePoint(8)
    escape(EscapeStyle.Octal, value, textStart)

  private def parseBackslashLowerP(): BackslashKind = parseBackslashProperty(invert = false)

  private def parseBackslashProperty(invert: Boolean): BackslashKind =
    if featureSet.escape.hasPythonRegexProperty then return parsePythonRegexProperty(invert)
    if featureSet.escape.hasICUProperty then return parseICUProperty(invert)
    if featureSet.escape.hasClassNameProperty then return parseBoostRegexProperty(invert)
    if !featureSet.escape.hasBackslashUnicodeProperty then return parseBackslashUnknown()

    if source.lift(offset + 1).forall(_ != '{') && !featureSet.escape.hasBackslashUnicodePropertyBare &&
      featureSet.escape.allowsAlphabeticUnknownBackslash
    then return parseBackslashUnknown()

    next()
    skipConstructComment()
    if isEnd || currentChar != '{' then
      assert(featureSet.escape.hasBackslashUnicodePropertyBare, "Invalid backslash escape")
      assert(!isEnd, "Invalid backslash escape")
      val name = currentChar.toString
      next()
      val klass =
        if invert then EscapeClassKind.NonUnicodeBareProperty(name) else EscapeClassKind.UnicodeBareProperty(name)
      return BackslashKind.EscapeClass(klass)

    next()
    skipConstructComment()
    val (negated, name, value) =
      if featureSet.escape.allowsLoosePropertyName then parseLooseProperty() else parseStrictProperty()
    expect('}', "Invalid backslash escape")

    val klass = (invert != negated, value) match
      case (false, None)        => EscapeClassKind.UnicodeProperty(name)
      case (false, Some(value)) => EscapeClassKind.UnicodePropertyValue(name, value)
      case (true, None)         => EscapeClassKind.NonUnicodeProperty(name)
      case (true, Some(value))  => EscapeClassKind.NonUnicodePropertyValue(name, value)
    BackslashKind.EscapeClass(klass)

  /** Parses Boost.Regex's `\\p{...}` or `\\pX`, whose name the IR builder looks up (e.g. `\\pL` and `\\p{Lu}`). */
  private def parseBoostRegexProperty(invert: Boolean): BackslashKind =
    next()
    assert(!isEnd, "Invalid backslash escape")
    val kind =
      if currentChar == '{' then
        next()
        val start = offset
        while !isEnd && currentChar != '}' do next()
        val name = sliceSource(start, offset)
        expect('}', "Invalid backslash escape")
        if invert then EscapeClassKind.NonUnicodeProperty(name) else EscapeClassKind.UnicodeProperty(name)
      else
        val value = currentCodePoint
        nextCodePoint(value)
        val name = String(Character.toChars(value))
        if invert then EscapeClassKind.NonUnicodeBareProperty(name) else EscapeClassKind.UnicodeBareProperty(name)
    BackslashKind.EscapeClass(kind)

  /** Parses python-regex's `\\p{...}` or `\\pX` (`X` is one of `CLMNPSZ`), which is the letter `p` unless a property
    * follows.
    */
  private def parsePythonRegexProperty(invert: Boolean): BackslashKind =
    val letter = currentChar
    next()
    val save = offset
    skipConstructComment()
    if !isEnd && currentChar == '{' then
      next()
      skipConstructComment()
      val negated = !isEnd && currentChar == '^'
      if negated then next()
      val (name, value) = parsePythonRegexPropertyName()
      skipConstructComment()
      if !isEnd && currentChar == '}' then
        next()
        val klass = (invert != negated, value) match
          case (false, None)        => EscapeClassKind.UnicodeProperty(name)
          case (false, Some(value)) => EscapeClassKind.UnicodePropertyValue(name, value)
          case (true, None)         => EscapeClassKind.NonUnicodeProperty(name)
          case (true, Some(value))  => EscapeClassKind.NonUnicodePropertyValue(name, value)
        return BackslashKind.EscapeClass(klass)
    else if !isEnd && "CLMNPSZ".contains(currentChar) then
      val name = currentChar.toString
      next()
      val klass =
        if invert then EscapeClassKind.NonUnicodeBareProperty(name) else EscapeClassKind.UnicodeBareProperty(name)
      return BackslashKind.EscapeClass(klass)
    reset(save)
    BackslashKind.Unknown(letter)

  /** Parses a property name of python-regex and its value after `:` or `=` (`parse_property_name` of `_regex_core.py`),
    * whose loose spellings the IR builder resolves.
    */
  private def parsePythonRegexPropertyName(): (String, Option[String]) =
    def parsePart(symbols: String): String =
      val builder = new StringBuilder
      skipConstructComment()
      while isAlphabetic || isDigit || !isEnd && symbols.contains(currentChar) do
        builder.append(currentChar)
        next()
        skipConstructComment()
      builder.result()

    val name = parsePart(" &_-.")
    val save = offset
    if !isEnd && (currentChar == ':' || currentChar == '=') then
      next()
      val value = parsePart(" &_-./").trim
      if value.nonEmpty then return (name, Some(value))
    reset(save)
    (name, None)

  // Ruby (Onigmo) looks up the whole name with `=` and the value, ignoring only spaces, hyphens, underscores and cases.
  private def parseStrictProperty(): (Boolean, String, Option[String]) =
    val negated = featureSet.escape.allowsNegatedRawPropertyName && !isEnd && currentChar == '^'
    if negated then next()
    val name = parsePropertyName()
    val value = Option.when(!isEnd && currentChar == '='):
      next()
      parsePropertyName()
    (negated, name, value)

  private def parsePropertyName(): String =
    if !featureSet.escape.allowsRawPropertyName then return parsePropertyID()
    val start = offset
    while !isEnd && currentChar != '}' && currentChar != '=' do next()
    assert(start < offset, "Invalid backslash escape")
    sliceSource(start, offset)

  // Perl and PCRE ignore blanks, hyphens, underscores and cases in property names, so the name is kept as written.
  private def parseLooseProperty(): (Boolean, String, Option[String]) =
    skipBraceBlank(force = true)
    val negated = !isEnd && currentChar == '^'
    if negated then next()
    val start = offset
    while !isEnd && currentChar != '}' do next()
    val body = sliceSource(start, offset).trim
    val separator = body.indexWhere(c => c == '=' || c == ':')
    val (name, value) =
      if separator < 0 then (body, None)
      else (body.slice(0, separator).trim, Some(body.slice(separator + 1, body.length).trim))
    assert(name.nonEmpty && value.forall(_.nonEmpty), "Invalid backslash escape")
    (negated, name, value)

  private def parseBackslashLowerQ(): BackslashKind =
    if !(featureSet.charClass.hasBackslashQuoteSet && context.inClass) then return parseBackslashUnknown()

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
        val isDouble = ClassSetReservedDoublePunctuators.contains(currentChar) &&
          source.startsWith(s"$currentChar$currentChar", offset)
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

  /** Parses an escape, which fails if it has a comment that Ruby removes.
    *
    * For example, `\x(?#x)41` and `\u{4(?#x)1}` are errors in Ruby, but `\x4(?#x)1` is `\x04` and `1`.
    */
  private def withoutRemovedComment(parse: => BackslashKind): BackslashKind =
    val start = offset
    val kind = parse
    var end = offset
    while removedCommentStarts.contains(end) do end = removedCommentStarts(end)
    // Ruby reads `\x` and `\u` before removing comments.
    if removedCommentEnds.keysIterator.exists(s => start < s && s < end) then fail("Invalid backslash escape", start)
    kind

  private def parseBackslashLowerU(): BackslashKind =
    if featureSet.escape.hasBackslashCaseClass then
      next()
      return BackslashKind.EscapeClass(EscapeClassKind.Upper)
    if !featureSet.escape.hasBackslashUnicodeHex then return parseBackslashUnknown()

    val save = offset
    next()
    val textStart = offset
    skipEscapeComment()

    if featureSet.escape.hasBackslashUnicodeBracket && currentChar == '{' then
      next()
      if featureSet.escape.hasBackslashUnicodeList then return parseUnicodeList(textStart)
      val value = parseBracedCodePoint(16)
      return escape(EscapeStyle.UnicodeBracket, value, textStart)

    if !hasHexDigitsAhead(4) then
      assert(featureSet.escape.allowsBrokenBackslashHex, "Invalid backslash escape")
      reset(save)
      return parseBackslashUnknown()

    val value = validateCodePoint(parseHexDigits(4), save)

    if featureSet.escape.combinesSurrogatePairEscape && UTF16Encoding.isHighSurrogate(value) then
      val lowStart = offset
      if startWith("\\u") then
        skipEscapeComment()
        if hasHexDigitsAhead(4) then
          val lowValue = parseHexDigits(4).toInt
          if UTF16Encoding.isLowSurrogate(lowValue) then
            val pairValue = UTF16Encoding.decodeSurrogatePair(value, lowValue)
            return escape(EscapeStyle.UnicodeHex4Pair, pairValue, textStart)
      reset(lowStart)

    escape(EscapeStyle.UnicodeHex4, value, textStart)

  /** Parses the code points of Ruby's `\u{...}`, which are separated by ASCII spaces (`ISSPACE`, e.g. `\n`), with at
    * most six digits for each.
    */
  private def parseUnicodeList(textStart: Int): BackslashKind =
    def skipSpaces(): Unit = while !isEnd && " \t\n\u000b\f\r".contains(currentChar) do next()

    skipSpaces()
    val values = Seq.newBuilder[Int]
    while !isEnd && isHexDigit do
      val start = offset
      val value = parseHexDigits(6)
      assert(isEnd || !isHexDigit, "Invalid backslash escape", start)
      values.addOne(validateCodePoint(value, start))
      skipSpaces()
    expect('}', "Invalid backslash escape")
    values.result() match
      case Seq()      => fail("Invalid backslash escape")
      case Seq(value) => escape(EscapeStyle.UnicodeBracket, value, textStart)
      case values     =>
        val written = writtenUnlessPlain(textStart, PlainText.unicodeList(values))
        pendingUnicodeList = Some(PendingUnicodeList(values, written))
        BackslashKind.Escape(EscapeStyle.UnicodeBracket, values.head)

  private def parseBackslashLowerV(): BackslashKind =
    if !(featureSet.escape.hasBackslashVerticalSpace || featureSet.escape.hasBackslashVerticalTab) then
      return parseBackslashUnknown()

    next()

    if featureSet.escape.hasBackslashVerticalSpace then return BackslashKind.EscapeClass(EscapeClassKind.Vertical)

    BackslashKind.Escape(EscapeStyle.Single('v'), 0x0b)

  private def parseBackslashLowerW(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.Word)

  private def parseBackslashLowerX(): BackslashKind =
    val save = offset
    next()
    val textStart = offset
    skipEscapeComment()

    if featureSet.numberSyntax != NumberSyntax.Digits then return parseTraitsBackslashX()

    if featureSet.escape.hasBackslashXBracket && currentChar == '{' then
      next()
      val value = parseBracedCodePoint(16)
      return escape(EscapeStyle.HexBracket, value, textStart)

    if !isHexDigit then
      if featureSet.escape.allowsBackslashXWithoutDigit then return BackslashKind.Escape(EscapeStyle.Hex0, 0x00)
      assert(featureSet.escape.allowsBrokenBackslashHex, "Invalid backslash escape")
      reset(save)
      return parseBackslashUnknown()

    if hasHexDigitsAhead(2) then return escape(EscapeStyle.Hex2, parseHexDigits(2).toInt, textStart)

    if featureSet.escape.hasBackslashXHex1 then return escape(EscapeStyle.Hex1, parseHexDigits(1).toInt, textStart)

    assert(featureSet.escape.allowsBrokenBackslashHex, "Invalid backslash escape")
    reset(save)
    parseBackslashUnknown()

  /** Parses Boost.Regex's `\\x{...}` or `\\xHH`, whose number [[parseTraitsNumber]] reads (e.g. `\\x{ 41}`), and the
    * latter reads at most two characters (e.g. `\\x 41` is `\\x04` and `1`).
    */
  private def parseTraitsBackslashX(): BackslashKind =
    assert(!isEnd, "Invalid backslash escape")
    val start = offset
    if currentChar == '{' then
      next()
      val value = parseTraitsNumber(16)
      assert(value >= 0, "Invalid backslash escape", start + 1)
      // Boost.Regex's `char` is signed, so `\x{80}` is an error, though `\x80` is not.
      val max = if featureSet.escape.readsCharAsSigned then 0x7f else Int.MaxValue
      if value > max then fail("Code point too large", start + 1)
      expect('}', "Invalid backslash escape")
      return escape(EscapeStyle.HexBracket, value.toInt, start)

    val value = parseTraitsNumber(16, 2)
    assert(value >= 0, "Invalid backslash escape", start)
    val style = if offset - start == 1 then EscapeStyle.Hex1 else EscapeStyle.Hex2
    escape(style, value.toInt, start)

  private def parseBackslashLowerZ(): BackslashKind =
    if !featureSet.escape.hasBackslashEnd then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.LowerEnd)

  private def parseBackslashUpperA(): BackslashKind =
    if !featureSet.escape.hasBackslashBegin then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.Begin)

  private def parseBackslashUpperB(): BackslashKind =
    if context.inClass then return parseBackslashUnknown()

    next()

    boundary:
      if featureSet.escape.hasBoundaryModifier then
        if startWith("{g}") then
          boundary.break(BackslashKind.Assert(AssertKind.NonBoundary(Some(BoundaryModifier.GModifier))))

        for (name, modifier) <- BoundaryModifiers do
          if startWith(name) then boundary.break(BackslashKind.Assert(AssertKind.NonBoundary(Some(modifier))))

      BackslashKind.Assert(AssertKind.NonBoundary(None))

  private def parseBackslashUpperC(): BackslashKind =
    if featureSet.escape.hasBackslashCodeUnit && !context.inClass then
      next()
      return BackslashKind.EscapeClass(EscapeClassKind.CodeUnit)

    if !featureSet.escape.hasBackslashMetaControl then
      assert(featureSet.escape.allowsUnknownBackslashUpperC, "Invalid backslash escape")
      return parseBackslashUnknown()

    next()
    expect('-', "Invalid backslash escape")
    val textStart = offset
    val value = parseMetaControlTarget()
    escape(EscapeStyle.ControlDash, metaControlValue(value), textStart)

  private def parseBackslashUpperD(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonDigit)

  private def parseBackslashUpperE(): BackslashKind =
    if !featureSet.escape.hasBackslashQuoteCommand || featureSet.escape.readsUnpairedQuoteEndAsLetter then
      return parseBackslashUnknown()
    assert(featureSet.escape.allowsUnpairedQuoteEnd, "Invalid backslash escape")

    next()
    BackslashKind.CaseCommand(CaseCommandKind.EndCaseCommand)

  private def parseBackslashUpperG(): BackslashKind =
    if !featureSet.escape.hasBackslashStickyAssert then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.Sticky)

  private def parseBackslashUpperH(): BackslashKind =
    if !(featureSet.escape.hasBackslashHorizontalSpace || featureSet.escape.hasBackslashHexDigit) then
      return parseBackslashUnknown()

    next()
    val klass =
      if featureSet.escape.hasBackslashHorizontalSpace then EscapeClassKind.NonHorizontal
      else EscapeClassKind.NonHexDigit
    BackslashKind.EscapeClass(klass)

  private def parseBackslashUpperK(): BackslashKind =
    if !featureSet.escape.hasBackslashCut then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.Cut)

  private def parseBackslashUpperL(): BackslashKind =
    if featureSet.escape.hasBackslashCaseClass then
      next()
      return BackslashKind.EscapeClass(EscapeClassKind.NonLower)
    if !featureSet.escape.hasBackslashNamedList || context.inClass then return parseBackslashUnknown()

    next()
    skipConstructComment()
    expect('<', "Invalid backslash escape")
    skipConstructComment()
    val name = parseReferenceName('>')
    skipConstructComment()
    expect('>', "Invalid backslash escape")
    BackslashKind.EscapeClass(EscapeClassKind.NamedList(name))

  private def parseBackslashUpperM(): BackslashKind =
    if featureSet.escape.hasBackslashWordStartEnd then
      next()
      return BackslashKind.Assert(AssertKind.WordEnd)

    if !featureSet.escape.hasBackslashMetaControl then return parseBackslashUnknown()

    next()
    expect('-', "Invalid backslash escape")
    val textStart = offset
    val value = parseMetaControlTarget()
    escape(EscapeStyle.Meta, value | 0x80, textStart)

  private def parseBackslashUpperN(): BackslashKind =
    if !(featureSet.escape.hasBackslashNonNewline || featureSet.escape.hasBackslashCharName || featureSet.escape.hasBackslashCharNameHex)
    then return parseBackslashUnknown()
    if featureSet.escape.fallsBackBrokenBackslashToLiteral then return parseBackslashCharNameOrLetter()

    next()
    val textStart = offset
    skipConstructComment()

    val isCharName =
      !isEnd && currentChar == '{' && !(featureSet.escape.hasBackslashNonNewline && isCurlyQuantifierAhead)
    if !isCharName then
      assert(featureSet.escape.hasBackslashNonNewline, "Invalid backslash escape")
      return BackslashKind.EscapeClass(EscapeClassKind.NonNewline)

    next()
    if featureSet.escape.hasBackslashCharNameHex && startWith("U+") then
      val start = offset
      assert(isHexDigit, "Invalid backslash escape")
      val value = validateCodePoint(parseHexDigits(Int.MaxValue), start)
      expect('}', "Invalid backslash escape")
      return escape(EscapeStyle.NamedHex, value, textStart)

    assert(featureSet.escape.hasBackslashCharName, "Invalid backslash escape")
    val start = offset
    while !isEnd && currentChar != '}' do next()
    val name = sliceSource(start, offset)
    assert(name.nonEmpty, "Invalid backslash escape")
    expect('}', "Invalid backslash escape")
    BackslashKind.EscapeClass(EscapeClassKind.CharName(name))

  /** Parses python-regex's `\\N{name}`, which is the letter `N` unless ASCII letters, digits, spaces and hyphens in
    * braces follow.
    */
  private def parseBackslashCharNameOrLetter(): BackslashKind =
    next()
    val save = offset
    skipConstructComment()
    if !isEnd && currentChar == '{' then
      next()
      val start = offset
      while isAlphabetic || isDigit || !isEnd && (currentChar == ' ' || currentChar == '-') do next()
      val name = sliceSource(start, offset)
      skipConstructComment()
      if !isEnd && currentChar == '}' then
        next()
        return BackslashKind.EscapeClass(EscapeClassKind.CharName(name))
    reset(save)
    BackslashKind.Unknown('N')

  private def isCurlyQuantifierAhead: Boolean =
    val end = source.indexOf('}', offset)
    end > 0 && {
      val body = source.slice(offset + 1, end)
      body.exists(_.isDigit) && body.forall(c => c.isDigit || c == ',' || c == ' ' || c == '\t')
    }

  private def parseBackslashUpperP(): BackslashKind = parseBackslashProperty(invert = true)

  private def parseBackslashUpperQ(): BackslashKind =
    if !featureSet.escape.hasBackslashQuoteCommand then return parseBackslashUnknown()

    next()
    BackslashKind.CaseCommand(CaseCommandKind.QuoteCommand)

  private def parseBackslashUpperR(): BackslashKind =
    if !featureSet.escape.hasBackslashGeneralNewline then return parseBackslashUnknown()

    next()
    BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline)

  private def parseBackslashUpperS(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonSpace)

  private def parseBackslashUpperU(): BackslashKind =
    if featureSet.escape.hasBackslashCaseClass then
      next()
      return BackslashKind.EscapeClass(EscapeClassKind.NonUpper)
    if !featureSet.escape.hasBackslashUnicodeHex8 then return parseBackslashUnknown()

    val save = offset
    next()
    val textStart = offset

    assert(hasHexDigitsAhead(8), "Invalid backslash escape")

    escape(EscapeStyle.UnicodeHex8, validateCodePoint(parseHexDigits(8), save), textStart)

  private def parseBackslashUpperV(): BackslashKind =
    if !featureSet.escape.hasBackslashVerticalSpace then return parseBackslashUnknown()

    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonVertical)

  private def parseBackslashUpperW(): BackslashKind =
    next()
    BackslashKind.EscapeClass(EscapeClassKind.NonWord)

  private def parseBackslashUpperX(): BackslashKind =
    if !featureSet.escape.hasBackslashGraphemeCluster then return parseBackslashUnknown()

    next()
    BackslashKind.EscapeClass(EscapeClassKind.GraphemeCluster)

  private def parseBackslashUpperZ(): BackslashKind =
    if !featureSet.escape.hasBackslashUpperEnd then return parseBackslashUnknown()

    next()
    BackslashKind.Assert(AssertKind.UpperEnd)

  private def parseBackslashOctal(): BackslashKind =
    if !featureSet.escape.hasBackslashBareOctal then
      if featureSet.escape.hasBackslashZeroOctal && currentChar == '0' then return parseBackslashZeroOctal()
      if featureSet.escape.hasBackslashNull && currentChar == '0' && !source.lift(offset + 1).exists(_.isDigit) then
        next()
        return BackslashKind.Escape(EscapeStyle.Single('0'), 0x00)
      fail("Invalid backslash escape")

    if !isOctalDigit then
      assert(featureSet.escape.allowsNonOctalDigitBackslash, "Invalid backslash escape")
      return parseBackslashUnknown()

    val size = if currentChar <= '3' || !featureSet.escape.limitsOctalTo0377ByDigits then 3 else 2
    val start = offset
    val value = parseOctalDigits(size)
    if value > 0xff && !featureSet.escape.allowsOctalAbove0377 then fail("Octal escape value too large", start)
    escape(EscapeStyle.BareOctal, value, start)

  private def parseBackslashZeroOctal(): BackslashKind =
    val start = offset
    // Boost.Regex reads `0` and at most three digits after it as an octal number, where `\0` alone is NUL.
    if featureSet.escape.readsThreeOctalDigitsAfterZero then
      val value = parseTraitsNumber(8, 4)
      // Boost.Regex's `char` is signed, so `\0200` is an error.
      if featureSet.escape.readsCharAsSigned && value > 0x7f then fail("Octal escape value too large", start)
      return escape(EscapeStyle.BareOctal, value.toInt, start + 1)
    next()
    val textStart = offset
    skipEscapeComment()
    assert(isOctalDigit, "Invalid octal escape")
    val size = if currentChar <= '3' then 3 else 2
    escape(EscapeStyle.BareOctal, parseOctalDigits(size), textStart)

  /** Parses Boost.Regex's `\\<` and `\\>`, the start and the end of a word, or the character in the other dialects. */
  private def parseBackslashAngle(): BackslashKind =
    if !featureSet.escape.hasBackslashAngleWordStartEnd then return parseBackslashUnknown()

    val kind = if currentChar == '<' then AssertKind.WordStart else AssertKind.WordEnd
    next()
    BackslashKind.Assert(kind)

  /** Parses Boost.Regex's `` \\` `` and `\\'`, the begin and the end of the input, or the character in the other
    * dialects.
    */
  private def parseBackslashQuoteAnchor(): BackslashKind =
    if !featureSet.escape.hasBackslashBufferAnchor then return parseBackslashUnknown()

    val kind = if currentChar == '`' then AssertKind.Begin else AssertKind.LowerEnd
    next()
    BackslashKind.Assert(kind)

  private def parseOctalDigits(size: Int): Int =
    var value = 0
    for _ <- 1 to size; if isOctalDigit do
      value = value * 8 + (currentChar - '0')
      next()
      skipEscapeComment()
    value

  private def parseBackslashBackReference(): BackslashKind =
    if context.inClass then return parseBackslashOctal()

    val save = offset

    if featureSet.escape.readsThreeOctalDigitsAsOctal && peekChars(3).count(isOctalDigit(_)) == 3 then
      return parseBackslashOctal()

    val groupCount =
      if featureSet.reference.readsBackReferenceDigitsByPreviousGroups then previousCaptureCount
      else prescan.captureSize
    val n =
      if featureSet.escape.readsThreeOctalDigitsAsOctal then parseBoundedDigits(_ < 100)
      else if featureSet.reference.readsBackReferenceSingleDigit then parseBoundedDigits(_ => false)
      else if featureSet.reference.readsBackReferenceDigitsWhileGroupExists then parseBoundedDigits(_ <= groupCount)
      else if featureSet.reference.readsBackReferenceDigitsBelowGroupCount then parseDigitsBelow(groupCount)
      else parseSaturatedIntOption(Long.MaxValue, "Invalid backslash escape", save).get

    if featureSet.reference.fallsBackInvalidBackReferenceToEscape && !(0 < n && n <= prescan.captureSize) then
      reset(save)
      if isOctalDigit then return parseBackslashOctal()

      return parseBackslashUnknown()

    val isOctalStart = isOctalDigit(source.charAt(save))
    if featureSet.reference.fallsBackMultiDigitBackReferenceToOctal && n >= 10 && n > groupCount && isOctalStart then
      reset(save)
      return parseBackslashOctal()

    // Ruby reads `\8` or `\9` followed by digits as a literal character when no such group exists.
    if featureSet.reference.fallsBackMultiDigitBackReferenceToLiteral && n >= 10 && n > groupCount then
      reset(save)
      return parseBackslashUnknown()

    if featureSet.reference.rejectsInvalidBackReference && n > prescan.captureSize then
      fail("Invalid back-reference", save)

    markNumberedReference(save)
    BackslashKind.EscapeBackReference(BackReferenceStyle.BareBackReference, Reference.IndexedReference(n))

  /** Returns the next characters up to `count`, between which Java and python-regex skip comments in the extended mode.
    */
  private def peekChars(count: Int): String =
    if !isSkippingConstructComment then return source.slice(offset, offset + count)
    val save = offset
    val builder = new StringBuilder
    while builder.length < count && !isEnd do
      builder.append(currentChar)
      next()
      skipConstructComment()
    reset(save)
    builder.result()

  /** Parses the digits of ICU's back-reference, which takes the next digit while the number is below `groupCount` (e.g.
    * `\\11` after ten groups is the eleventh group).
    */
  private def parseDigitsBelow(groupCount: Int): Int =
    var value = currentChar - '0'
    next()
    while value < groupCount && isDigit do
      value = value * 10 + (currentChar - '0')
      next()
    value

  /** Parses the digits while they make a number that continues, where Java skips whitespace and comments only between
    * them (e.g. `\1 2`), so the ones after the number are kept.
    */
  private def parseBoundedDigits(continues: Int => Boolean): Int =
    var value = currentChar - '0'
    next()
    var end = offset
    skipConstructComment()
    while isDigit && continues(value * 10 + (currentChar - '0')) do
      value = value * 10 + (currentChar - '0')
      next()
      end = offset
      skipConstructComment()
    reset(end)
    value

  private def parseBackslashEscape(): BackslashKind =
    val value = currentChar
    if value == '&' then validateSymbolUnknownBackslash()
    next()
    BackslashKind.Escape(EscapeStyle.Single(value), value.toInt)

  private def parseBackslashUnknown(): BackslashKind =
    assert(!isEnd, "Invalid backslash escape")
    val value = currentCodePoint

    if isAlphabetic then assert(featureSet.escape.allowsAlphabeticUnknownBackslash, "Invalid backslash escape")
    else if isDotNetWord(value) then assert(featureSet.escape.allowsWordUnknownBackslash, "Invalid backslash escape")
    else validateSymbolUnknownBackslash()

    nextCodePoint(value)
    BackslashKind.Unknown(value)

  private def validateSymbolUnknownBackslash(): Unit =
    if featureSet.escape.allowsSymbolUnknownBackslash then return
    if context.inClass && currentChar == '-' then return
    if featureSet.charClass.hasBackslashClassSetReservedPunctuator && context.inClass && "&-!#%,:;<=>@`~".contains(
        currentChar
      )
    then return
    fail("Invalid backslash escape")

  private def parseClass(stack: mutable.Stack[Node]): Unit = protectContext:
    boundary:
      context = context.copy(inClass = true)

      val start = offset

      if featureSet.charClass.hasClassPOSIXWordBoundary && (startWith("[[:<:]]") || startWith("[[:>:]]")) then
        val name = source.slice(start + 3, start + 4)
        val end = offset
        val item = ClassItem(ClassItemData.ClassPOSIX(false, name), SourceLocation(start + 1, end - 1))
        stack.push(Node(NodeData.Class(false, item), SourceLocation(start, end)))
        boundary.break()

      if featureSet.charClass.hasICUClass then
        next()
        val (invert, item) = nest(start)(parseICUClassBody())
        stack.push(Node(NodeData.Class(invert, item), SourceLocation(start, offset)))
        boundary.break()

      next()

      if featureSet.charClass.hasPythonRegexClass then
        // python-regex skips no comment in a class.
        context = context.copy(skipsComment = false)
        val invert = startWith("^")
        val item = nest(start)(parsePythonRegexClassItem())
        expect(']', "Unclosed ']'")
        stack.push(Node(NodeData.Class(invert, item), SourceLocation(start, offset)))
        boundary.break()

      if featureSet.charClass.rejectsClassPOSIXOutsideClass then
        findClassPOSIXEnd(start).foreach: _ =>
          if currentChar == ':' then fail("POSIX class outside class", start)
          fail("Unsupported POSIX collating element", start)

      // PCRE and Perl skip the blanks of `xx` before `^` (e.g. `[ ^a]`), but Java reads `^` only right after `[`.
      val save = offset
      if context.verboseMore then skipBlank()
      val invert = currentChar == '^'
      if invert then next() else reset(save)

      val item = nest(start)(parseClassItem(!featureSet.charClass.allowsClassEmpty))
      expect(']', "Unclosed ']'")
      validateNegatedClassSet(invert, item, start)

      val end = offset
      stack.push(Node(NodeData.Class(invert, item), SourceLocation(start, end)))

  private def parseClassItem(first: Boolean): ClassItem =
    val start = offset

    var item = parseClassItemUnion(first)
    val operands = Seq.newBuilder[ClassItem]
    val operators = mutable.Set.empty[String]
    val savedDepth = depth

    var continue = true
    while continue do
      continue = false

      if startWithClassIntersection() then
        continue = true
        enter(offset - 2)
        if featureSet.charClass.validatesClassSetSyntax then
          assert(isEnd || currentChar != '&', "Invalid class set character")
        val right = parseClassItemUnion(false)
        val end = offset
        if !featureSet.charClass.allowsEmptyClassIntersection then
          assert(!(isEmptyClassItem(item) && isEmptyClassItem(right)), "Invalid class set operation", start)
        operands.addOne(item).addOne(right)
        operators.add("&&")
        item = ClassItem(ClassItemData.ClassIntersection(item, right), SourceLocation(start, end))

      if featureSet.charClass.allowsClassDiff && startWith("--") then
        continue = true
        enter(offset - 2)
        val right = parseClassItemUnion(false)
        val end = offset
        operands.addOne(item).addOne(right)
        operators.add("--")
        item = ClassItem(ClassItemData.ClassDiff(item, right), SourceLocation(start, end))

      if featureSet.charClass.hasClassSubtraction && startWith("-[") then
        val nestStart = offset - 1
        val invert = currentChar == '^'
        if invert then next()
        val inner = nest(nestStart)(parseClassItem(true))
        expect(']', "Unclosed ']'")
        val nested = ClassItem(ClassItemData.ClassNest(invert, inner), SourceLocation(nestStart, offset))
        assert(isEnd || currentChar == ']', "Class subtraction must be last")
        item = ClassItem(ClassItemData.ClassDiff(item, nested), SourceLocation(start, offset))

    depth = savedDepth

    if featureSet.charClass.validatesClassSetSyntax && operators.nonEmpty then
      assert(operators.size == 1, "Invalid class set operation", start)
      assert(operands.result().forall(isClassSetOperand), "Invalid class set operation", start)

    item

  /** Skips Java's whitespace and comments in a class, or the blanks (U+0009 and U+0020) of PCRE's and Perl's `xx`. */
  private def skipClassComment(): Unit =
    if context.skipsComment && featureSet.comment.skipsCommentInClass then skipComment()
    else if context.verboseMore then skipBlank()

  /** Parses the text that [[skipClassComment]] skips as a blank item, if any. */
  private def parseClassBlank(): Option[ClassItem] =
    val start = offset
    skipClassComment()
    Option.when(offset > start):
      ClassItem(ClassItemData.ClassBlank(sliceSource(start, offset)), SourceLocation(start, offset))

  /** Reads `&&`, between which Java skips whitespace and comments (e.g. `[a& &b]`). */
  private def startWithClassIntersection(): Boolean =
    if !featureSet.charClass.allowsClassIntersection || isEnd || currentChar != '&' then return false
    val save = offset
    next()
    skipConstructComment()
    if !isEnd && currentChar == '&' then
      next()
      return true
    reset(save)
    false

  private def isClassIntersectionAhead: Boolean =
    val save = offset
    val found = startWithClassIntersection()
    reset(save)
    found

  private def isEmptyClassItem(item: ClassItem): Boolean =
    item.data == ClassItemData.ClassUnion(Seq.empty) || isClassBlank(item) || isEmptyClassQuote(item)

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

  private def validateNegatedClassSet(invert: Boolean, item: ClassItem, start: Int): Unit =
    if featureSet.charClass.validatesClassSetSyntax && invert then
      assert(!mayContainStrings(item), "Negated class may contain strings", start)

  private def parseClassItemUnion(first: Boolean): ClassItem =
    val start = offset
    val seq = mutable.ListBuffer.empty[ClassItem]

    // A `]` after only blanks, empty quotes and `\E` is still the first character (e.g. `[\E]]` in PCRE).
    var empty = true

    seq.addAll(parseClassBlank())
    while (first && empty && !isEnd && currentChar == ']') || !isClassItemStop(start) do
      parseClassItemAtom(seq, start)
      if empty then empty = seq.forall(item => isClassBlank(item) || isSkippableInClassRange(item))
      seq.addAll(parseClassBlank())

    val end = offset
    val items = seq.toSeq
    if items.size == 1 then return items.head
    ClassItem(ClassItemData.ClassUnion(items), SourceLocation(start, end))

  private def isClassItemStop(start: Int): Boolean =
    isEnd ||
      currentChar == ']' ||
      isClassIntersectionAhead ||
      featureSet.charClass.allowsClassDiff && source.startsWith("--", offset) ||
      featureSet.charClass.hasClassSubtraction && start != offset && source.startsWith("-[", offset)

  private def parseClassItemAtom(seq: mutable.Builder[ClassItem, Seq[ClassItem]], unionStart: Int): Unit =
    val start = offset
    val item = parseClassItemNonRange()
    if isSkippableInClassRange(item) then seq.addOne(item)
    else parseClassRange(seq, unionStart, start, item)

  /** Parses a range from the item if `-` follows it, and adds the items to the union. */
  private def parseClassRange(
      seq: mutable.Builder[ClassItem, Seq[ClassItem]],
      unionStart: Int,
      start: Int,
      item: ClassItem
  ): Unit =
    val skippedBeforeHyphen = parseClassBlank().toSeq ++ parseClassItemsSkippableInRange(unionStart)
    if isClassItemStop(unionStart) || currentChar != '-' then
      seq.addOne(item).addAll(skippedBeforeHyphen)
      return

    val save = offset
    next()

    // PCRE and Perl skip the blanks of `xx` before `]` (e.g. `[a- ]`), but Java reads `]` only right after `-`.
    if context.verboseMore then skipBlank()
    if isClassItemStop(unionStart) then
      reset(save)
      seq.addOne(item).addAll(skippedBeforeHyphen)
      return

    val (beforeBegin, begin) = (quotedValue(item), classRangeItems(item)) match
      case (Some(value), _)                      => (Seq.empty, value)
      case (None, Some(items)) if items.nonEmpty => (items.init, items.last)
      case _                                     => (Seq.empty, item)

    skipClassComment()
    var endItem = parseClassItemNonRange(rangeEnd = true)
    val skippedAfterHyphen = mutable.ListBuffer.empty[ClassItem]
    while isSkippableInClassRange(endItem) && !isClassItemStop(unionStart) do
      skippedAfterHyphen.addOne(endItem)
      skipClassComment()
      endItem = parseClassItemNonRange(rangeEnd = true)
    if isSkippableInClassRange(endItem) then
      reset(save)
      seq.addOne(item).addAll(skippedBeforeHyphen)
      return

    val (end, afterEnd) = (quotedValue(endItem), classRangeItems(endItem)) match
      case (Some(value), _)                      => (value, Seq.empty)
      case (None, Some(items)) if items.nonEmpty => (items.head, items.tail)
      case _                                     => (endItem, Seq.empty)

    (begin, end) match
      case (begin: ClassItem.Value, end: ClassItem.Value) =>
        assert(begin.data.value <= end.data.value, "Out of order class range", begin.loc.start)
        val range = ClassItem(ClassItemData.ClassRange(begin, end), SourceLocation(begin.loc.start, end.loc.end))
        (item.data, endItem.data) match
          // The last escape of Ruby's `\u{...}` at the end of a range may begin another (e.g. `B-z` of `[0-\u{41 42}-z]`).
          case (_, _: ClassItemData.ClassUnicodeList) =>
            val list = joinClassUnicodeLists(item, beforeBegin ++ (range +: afterEnd), endItem)
            parseClassRange(seq, unionStart, start, list)
          case (_: ClassItemData.ClassUnicodeList, _) =>
            seq.addOne(joinClassUnicodeLists(item, beforeBegin :+ range, endItem))
          case _ =>
            seq.addAll(beforeBegin)
            seq.addOne(range)
            // The `\E` inside a range is kept after it, and the blanks and the empty quotes are dropped.
            seq.addAll(
              (skippedBeforeHyphen ++ skippedAfterHyphen).filterNot(i => isClassBlank(i) || isEmptyClassQuote(i))
            )
            seq.addAll(afterEnd)
            // Boost.Regex reads a `-` after a range as a character only before `]` (e.g. `[a-c-]`, but not `[a-c-e]`).
            if featureSet.charClass.rejectsHyphenAfterClassRange && startWith("-") then
              assert(!isEnd && currentChar == ']', "Invalid class range")
              reset(offset - 1)
      case _ =>
        // Boost.Regex's range of a collating element (e.g. `[a-[.c.]]`) is kept as its items, which are unsupported.
        val isCollatingRange = Seq(begin, end).exists(_.data.isInstanceOf[ClassItemData.ClassCollatingElement])
        if !isCollatingRange then
          if !isValueClassItem(begin) then
            if !isClassNestItem(begin) then
              assert(featureSet.charClass.allowsBrokenClassRangeBegin, "Invalid class range", start)
          else if !isClassNestItem(end) then
            assert(featureSet.charClass.allowsBrokenClassRangeEnd, "Invalid class range", start)

        seq.addOne(item).addAll(skippedBeforeHyphen)
        if featureSet.charClass.consumesBrokenClassRange || isCollatingRange then
          seq.addOne(ClassItem.Value(ClassItemData.ClassLiteral('-'), SourceLocation(save, save + 1)))
          seq.addAll(skippedAfterHyphen)
          seq.addOne(endItem)
        else reset(save)

  /** Parses the empty quotes, `\E` and blanks between a range begin and its `-`, which do not break the range. */
  private def parseClassItemsSkippableInRange(unionStart: Int): Seq[ClassItem] =
    val items = mutable.ListBuffer.empty[ClassItem]
    if !featureSet.escape.hasBackslashQuoteCommand then return items.toSeq
    boundary:
      while !isClassItemStop(unionStart) && currentChar == '\\' do
        val save = offset
        val item = parseClassItemNonRange()
        if !isSkippableInClassRange(item) then
          reset(save)
          boundary.break()
        items.addOne(item)
        items.addAll(parseClassBlank())
    items.toSeq

  private def isSkippableInClassRange(item: ClassItem): Boolean =
    isEmptyClassQuote(item) || item.data.isInstanceOf[ClassItemData.ClassCaseCommand]

  private def isClassBlank(item: ClassItem): Boolean =
    item.data.isInstanceOf[ClassItemData.ClassBlank]

  /** Returns the literals of a `\\Q...\\E` class item.
    *
    * Java and PCRE read them as unquoted for a range, so an empty `\\Q\\E` is ignored (e.g. `[a-\\Q\\Ez]` is `[a-z]`).
    */
  private def classQuoteLiterals(item: ClassItem): Option[Seq[ClassItem]] =
    item.data match
      case ClassItemData.ClassCaseRegion(_, parts, _) => Some(parts.map(_.value))
      case _                                          => None

  /** Returns the items that a range takes an end from: the literals of a quote, or the items of Ruby's `\u{...}`, which
    * Ruby expands before reading the class (e.g. `[\u{41 42}-z]` is `[\x41\x42-z]`).
    */
  private def classRangeItems(item: ClassItem): Option[Seq[ClassItem]] = item.data match
    case ClassItemData.ClassUnicodeList(items, _) => Some(items)
    case _                                        => classQuoteLiterals(item)

  /** Returns Ruby's `\u{...}` of the items made of a range between `left` and `right`, one of which is a list at least.
    */
  private def joinClassUnicodeLists(left: ClassItem, items: Seq[ClassItem], right: ClassItem): ClassItem =
    val texts = Seq(left.data, right.data).collect:
      case ClassItemData.ClassUnicodeList(items, written) => written.getOrElse(PlainText.classUnicodeList(items))
    val text = texts.mkString("-\\u")
    val written = Option.when(text != PlainText.classUnicodeList(items))(text)
    ClassItem(ClassItemData.ClassUnicodeList(items, written), SourceLocation(items.head.loc.start, items.last.loc.end))

  /** Returns the value of a quote of one character, with the quote as its form at an end of a range (e.g. `\Qa\E`). */
  private def quotedValue(item: ClassItem): Option[ClassItem.Value] = item.data match
    case ClassItemData.ClassCaseRegion(kind, Seq(CasePart(value: ClassItem.Value, Some(written))), end) =>
      Some(value.copy(loc = item.loc, caseForm = Some(CaseForm(kind, written, end))))
    case _ => None

  private def isEmptyClassQuote(item: ClassItem): Boolean =
    classQuoteLiterals(item).exists(_.isEmpty)

  /** Parses an item of a class but a range, where `rangeEnd` tells whether it is the end of a range. */
  private def parseClassItemNonRange(rangeEnd: Boolean = false): ClassItem =
    val start = offset

    if featureSet.charClass.allowsClassPOSIX && currentChar == '[' then
      val item =
        if featureSet.charClass.hasClassCollatingElement then parseBoostRegexClassBracket() else parseClassPOSIXOption()
      item match
        case Some(item) => return item
        case None       => ()

    if featureSet.charClass.allowsClassNest && currentChar == '[' then
      next()
      val invert = currentChar == '^'
      if invert then next()
      val item = nest(start)(parseClassItem(!featureSet.charClass.allowsClassEmpty))
      expect(']', "Unclosed ']'")
      validateNegatedClassSet(invert, item, start)
      val end = offset
      return ClassItem(ClassItemData.ClassNest(invert, item), SourceLocation(start, end))

    if currentChar == '\\' then
      return if featureSet.charClass.readsClassBackslashByLetter then parseBoostRegexClassBackslash(start, rangeEnd)
      else parseClassBackslash(start)

    parseClassItemLiteral()

  /** Parses an escape in Boost.Regex's class, whose backslash is at `start`.
    *
    * It is a class of its letter (e.g. `\\d` and `\\L`), but the letter itself at the end of a range (e.g. `[a-\\d]` is
    * `[a-d]`), or a character (e.g. `\\v` is VT, and `\\p` is `p`).
    */
  private def parseBoostRegexClassBackslash(start: Int, rangeEnd: Boolean): ClassItem =
    next()
    assert(!isEnd, "Invalid backslash escape", start)
    BoostRegexClassEscapes.get(currentChar).filter(_ => !rangeEnd) match
      case Some(kind) =>
        next()
        ClassItem(ClassItemData.ClassBackslashClass(kind), SourceLocation(start, offset))
      case None =>
        val backslash = (currentChar: @switch) match
          case 'v' =>
            next()
            BackslashKind.Escape(EscapeStyle.Single('v'), 0x0b)
          case 'a' | 'b' | 'c' | 'e' | 'f' | 'n' | 'r' | 't' | 'x' | 'N' | '0' | '1' | '2' | '3' | '4' | '5' | '6' |
              '7' | '8' | '9' =>
            parseBackslashKind()
          case _ => parseBackslashUnknown()
        backslash match
          case kind: BackslashKind.BackslashValue =>
            ClassItem.Value(ClassItemData.ClassBackslashValue(kind), SourceLocation(start, offset))
          case BackslashKind.EscapeClass(kind) =>
            ClassItem(ClassItemData.ClassBackslashClass(kind), SourceLocation(start, offset))
          // The escapes of the characters above are values or `\\N{...}`.
          // $COVERAGE-OFF$
          case kind => throw new IllegalStateException(s"Unexpected escape in a class: $kind")
        // $COVERAGE-ON$

  /** The escape classes of Boost.Regex's class, which are the escapes whose letters are the names of classes. */
  private val BoostRegexClassEscapes: Map[Char, EscapeClassKind] = Map(
    'd' -> EscapeClassKind.Digit,
    'D' -> EscapeClassKind.NonDigit,
    'h' -> EscapeClassKind.Horizontal,
    'H' -> EscapeClassKind.NonHorizontal,
    'l' -> EscapeClassKind.Lower,
    'L' -> EscapeClassKind.NonLower,
    's' -> EscapeClassKind.Space,
    'S' -> EscapeClassKind.NonSpace,
    'u' -> EscapeClassKind.Upper,
    'U' -> EscapeClassKind.NonUpper,
    'V' -> EscapeClassKind.NonVertical,
    'w' -> EscapeClassKind.Word,
    'W' -> EscapeClassKind.NonWord
  )

  /** Parses Boost.Regex's `[:name:]`, `[.name.]` or `[=name=]` in a class, whose name has at least one character and
    * any characters, or returns `None` for another `[`.
    *
    * The name of `[:name:]` is looked up by the IR builder (e.g. `[:Lu:]` of `u32regex`), and an unclosed one is an
    * error (e.g. `[[:alpha]]`).
    */
  private def parseBoostRegexClassBracket(): Option[ClassItem] =
    val start = offset
    val terminator = source
      .lift(offset + 1)
      .filter(c => c == ':' || c == '.' || c == '=')
    terminator.map: t =>
      reset(offset + 2)
      val nameStart = offset
      assert(!isEnd, "Unclosed ']'")
      next()
      while !isEnd && currentChar != t do next()
      val name = sliceSource(nameStart, offset)
      expect(t, "Unclosed ']'")
      expect(']', "Unclosed ']'")
      val data = t match
        case ':' =>
          val invert = name.startsWith("^")
          ClassItemData.ClassPOSIX(invert, if invert then name.drop(1) else name)
        case '.' => ClassItemData.ClassCollatingElement(name)
        case _   => ClassItemData.ClassEquivalence(name)
      ClassItem(data, SourceLocation(start, offset))

  /** Parses an escape in a class, whose backslash is at `start`. */
  private def parseClassBackslash(start: Int): ClassItem =
    next()
    if isLiteralBackslashInClass then
      val kind = BackslashKind.Unknown(currentChar.toInt)
      next()
      return ClassItem.Value(ClassItemData.ClassBackslashValue(kind), SourceLocation(start, offset))
    val backslash = parseBackslashKind()
    val end = offset
    pendingUnicodeList match
      case Some(PendingUnicodeList(values, written)) =>
        pendingUnicodeList = None
        val items = values.map: value =>
          val escape = BackslashKind.Escape(EscapeStyle.UnicodeBracket, value)
          ClassItem.Value(ClassItemData.ClassBackslashValue(escape), SourceLocation(start, end))
        return ClassItem(ClassItemData.ClassUnicodeList(items, written), SourceLocation(start, end))
      case None => ()
    backslash match
      case kind: BackslashKind.BackslashValue =>
        ClassItem.Value(ClassItemData.ClassBackslashValue(kind), SourceLocation(start, end))
      case BackslashKind.EscapeClass(
            EscapeClassKind.GeneralNewline | EscapeClassKind.GraphemeCluster | EscapeClassKind.NonNewline
          ) =>
        fail("Invalid escape sequence", start)
      case BackslashKind.EscapeClass(klass) =>
        ClassItem(ClassItemData.ClassBackslashClass(klass), SourceLocation(start, end))
      case BackslashKind.CaseCommand(CaseCommandKind.QuoteCommand) =>
        parseClassQuote(start)
      case BackslashKind.CaseCommand(kind) =>
        ClassItem(ClassItemData.ClassCaseCommand(kind), SourceLocation(start, end))
      case _ =>
        fail("Invalid escape sequence", start)

  /** Parses the items of python-regex's class after `[` and `^`, following `parse_set` of `_regex_core.py`.
    *
    * `V1` has the set operators, whose precedence is `||` < `~~` < `&&` < `--` < an implicit union, and nested classes.
    */
  private def parsePythonRegexClassItem(): ClassItem =
    if featureSet.charClass.hasClassSetOperator then parsePythonRegexClassUnion()
    else parsePythonRegexClassImplicitUnion()

  private def parsePythonRegexClassUnion(): ClassItem =
    parsePythonRegexClassOperation("||", parsePythonRegexClassSymmetricDiff, ClassItemData.ClassExplicitUnion(_, _))

  private def parsePythonRegexClassSymmetricDiff(): ClassItem =
    parsePythonRegexClassOperation("~~", parsePythonRegexClassIntersection, ClassItemData.ClassSymmetricDiff(_, _))

  private def parsePythonRegexClassIntersection(): ClassItem =
    parsePythonRegexClassOperation("&&", parsePythonRegexClassDiff, ClassItemData.ClassIntersection(_, _))

  private def parsePythonRegexClassDiff(): ClassItem =
    parsePythonRegexClassOperation("--", parsePythonRegexClassImplicitUnion, ClassItemData.ClassDiff(_, _))

  /** Parses the operands of a left-associative operator of python-regex's class. */
  private def parsePythonRegexClassOperation(
      operator: String,
      parseOperand: () => ClassItem,
      build: (ClassItem, ClassItem) => ClassItemData
  ): ClassItem =
    val start = offset
    var item = parseOperand()
    while startWith(operator) do
      val right = parseOperand()
      item = ClassItem(build(item, right), SourceLocation(start, offset))
    item

  private def parsePythonRegexClassImplicitUnion(): ClassItem =
    val start = offset
    val items = mutable.ListBuffer(parsePythonRegexClassMember())
    while (isEnd || currentChar != ']') && !isPythonRegexClassOperatorAhead do
      items.addOne(parsePythonRegexClassMember())
    if items.size == 1 then items.head
    else ClassItem(ClassItemData.ClassUnion(items.toSeq), SourceLocation(start, offset))

  private def isPythonRegexClassOperatorAhead: Boolean =
    featureSet.charClass.hasClassSetOperator && Seq("||", "~~", "&&", "--").exists(source.startsWith(_, offset))

  /** Parses a member of python-regex's class, which is an item or a range of characters. */
  private def parsePythonRegexClassMember(): ClassItem =
    val begin = parsePythonRegexClassAtom()
    val afterBegin = offset
    val beginValue = pythonRegexRangeEnd(begin) match
      case Some(value) => value
      case None        => return begin
    if !startWith("-") then return begin
    val afterHyphen = offset
    // `V1` reads `--` after a character as the difference.
    if featureSet.charClass.hasClassSetOperator && !isEnd && currentChar == '-' then
      reset(afterBegin)
      return begin
    val hyphen = ClassItem.Value(ClassItemData.ClassLiteral('-'), SourceLocation(afterBegin, afterHyphen))
    if !isEnd && currentChar == ']' then
      return ClassItem(ClassItemData.ClassUnion(Seq(begin, hyphen)), SourceLocation(begin.loc.start, afterHyphen))
    val end = parsePythonRegexClassAtom()
    pythonRegexRangeEnd(end) match
      case Some(endValue) =>
        assert(beginValue.data.value <= endValue.data.value, "Out of order class range", begin.loc.start)
        ClassItem(ClassItemData.ClassRange(beginValue, endValue), SourceLocation(begin.loc.start, end.loc.end))
      case None =>
        ClassItem(ClassItemData.ClassUnion(Seq(begin, hyphen, end)), SourceLocation(begin.loc.start, end.loc.end))

  /** Returns the value of an item of python-regex's class as an end of a range, which a nested class of one character
    * also is (e.g. `[c]` of `[a-[c]]`).
    */
  private def pythonRegexRangeEnd(item: ClassItem): Option[ClassItem.Value] = item match
    case value: ClassItem.Value                                                        => Some(value)
    case ClassItem.NonValue(ClassItemData.ClassNest(false, value: ClassItem.Value), _) => Some(value)
    case _                                                                             => None

  /** Parses an item of python-regex's class: an escape, a POSIX class, a nested class (`V1`) or a character. */
  private def parsePythonRegexClassAtom(): ClassItem =
    val start = offset
    if !isEnd && currentChar == '\\' then return parseClassBackslash(start)

    if startWith("[:") then
      val invert = startWith("^")
      val (name, value) = parsePythonRegexPropertyName()
      if startWith(":]") then
        val text = value.fold(name)(v => s"$name=$v")
        return ClassItem(ClassItemData.ClassPOSIX(invert, text), SourceLocation(start, offset))
      reset(start)

    if featureSet.charClass.hasClassSetOperator && startWith("[") then
      val invert = startWith("^")
      val item = nest(start)(parsePythonRegexClassUnion())
      expect(']', "Unclosed ']'")
      return ClassItem(ClassItemData.ClassNest(invert, item), SourceLocation(start, offset))

    assert(!isEnd, "Unclosed ']'")
    parseClassItemLiteral()

  /** Parses ICU's `\\p{...}` or `\\P{...}`, whose body is a property expression of ICU's `UnicodeSet` (e.g. `gc=L`),
    * which the IR builder resolves.
    */
  private def parseICUProperty(invert: Boolean): BackslashKind =
    BackslashKind.EscapeClass(parseICUPropertyClass(invert))

  private def parseICUPropertyClass(invert: Boolean): EscapeClassKind =
    next()
    skipConstructComment()
    expect('{', "Invalid backslash escape")
    val start = offset
    while !isEnd && currentChar != '}' do next()
    val name = sliceSource(start, offset)
    expect('}', "Invalid backslash escape")
    if invert then EscapeClassKind.NonUnicodeProperty(name) else EscapeClassKind.UnicodeProperty(name)

  /** Parses the items of ICU's class after `[` up to its `]`, following the set states of ICU4C's `regexcst.txt`, and
    * returns whether it is negated and the items.
    *
    * `[:name:]` is a property. The operators `--` and `&&` bind looser than an implicit union and a `-` or `&` between
    * classes (e.g. `[[a-z]-[aeiou]]`), and the ones of the same precedence go from left to right.
    */
  private def parseICUClassBody(): (Boolean, ClassItem) =
    val property = peekICUClassToken(':').flatMap: start =>
      reset(start)
      parseICUClassProperty()
    property match
      case Some(item) =>
        expect(']', "Unclosed ']'")
        (false, item)
      case None =>
        val invert = peekICUClassToken('^').isDefined
        // A `]` right after `[` or `[^` is a character.
        val first = peekICUClassToken(']').map(start =>
          ClassItem.Value(ClassItemData.ClassLiteral(']'), SourceLocation(start, offset))
        )
        val item = parseICUClassOperation(first)
        expect(']', "Unclosed ']'")
        (invert, item)

  /** Reads the next token of ICU's class if it is the character `c` out of a quote, and returns its offset, or reads
    * nothing.
    */
  private def peekICUClassToken(c: Char): Option[Int] =
    val save = offset
    readICUClassToken() match
      case ICUClassToken.Character(item, false) if item.data.value == c => Some(item.loc.start)
      case _                                                            =>
        reset(save)
        None

  /** Parses ICU's `[:name:]` at its first `:` up to its last `:`, or returns `None` for another class starting with `:`
    * (e.g. `[:a]`).
    */
  private def parseICUClassProperty(): Option[ClassItem] =
    val start = offset
    next()
    skipClassComment()
    val invert = !isEnd && currentChar == '^'
    if invert then next()
    val name = new java.lang.StringBuilder
    boundary:
      while !isEnd && currentChar != '\\' do
        name.append(currentChar)
        next()
        skipClassComment()
        if !isEnd && currentChar == ':' then
          next()
          if isEnd || currentChar != ']' then boundary.break()
          return Some(ClassItem(ClassItemData.ClassPOSIX(invert, name.toString), SourceLocation(start, offset)))
    reset(start)
    None

  /** Parses the terms of ICU's class between `--` and `&&`, where `first` is the `]` right after `[`. */
  private def parseICUClassOperation(first: Option[ClassItem.Value]): ClassItem =
    val start = first.fold(offset)(_.loc.start)
    val savedDepth = depth
    var (item, operator) = parseICUClassTerm(first, ICUClassState.Start)
    while operator.isDefined do
      enter(offset - 2)
      val (right, next) = parseICUClassTerm(None, ICUClassState.Operator)
      val data =
        if operator.contains("--") then ClassItemData.ClassDiff(item, right)
        else ClassItemData.ClassIntersection(item, right)
      item = ClassItem(data, SourceLocation(start, offset))
      operator = next
    depth = savedDepth
    item

  /** Parses a term of ICU's class up to `]` or an operator `--` or `&&`, and returns it with the operator.
    *
    * A term is items in a row, where a `-` or `&` between a class or a property and a nested class is the difference or
    * the intersection of the items before it and the nested class. The blanks of the extended mode between the items
    * are items, but the ones in a range are dropped.
    */
  private def parseICUClassTerm(first: Option[ClassItem.Value], initial: ICUClassState): (ClassItem, Option[String]) =
    val start = first.fold(offset)(_.loc.start)
    val items = mutable.ListBuffer.empty[ClassItem]
    items ++= first
    var state = first.fold(initial)(ICUClassState.Literal(_))
    var operator: Option[String] = None
    var continues = true
    val savedDepth = depth

    def add(item: ClassItem, after: ICUClassState): Unit =
      items += item
      state = after

    def addClass(kind: EscapeClassKind, itemStart: Int, after: ICUClassState): Unit =
      add(ClassItem(ClassItemData.ClassBackslashClass(kind), SourceLocation(itemStart, offset)), after)

    def union: ClassItem =
      if items.size == 1 then items.head
      else ClassItem(ClassItemData.ClassUnion(items.toSeq), SourceLocation(start, offset))

    def addNested(nestStart: Int, operation: Option[(ClassItem, ClassItem) => ClassItemData]): Unit =
      val (invert, item) = nest(nestStart)(parseICUClassBody())
      val nested = ClassItem(ClassItemData.ClassNest(invert, item), SourceLocation(nestStart, offset))
      operation match
        case Some(build) =>
          enter(nestStart - 1)
          val left = union
          items.clear()
          add(ClassItem(build(left, nested), SourceLocation(left.loc.start, offset)), ICUClassState.Set)
        case None => add(nested, ICUClassState.Set)

    while continues do
      items ++= parseClassBlank()
      val tokenStart = offset
      readICUClassToken() match
        case ICUClassToken.End         => fail("Unclosed ']'")
        case ICUClassToken.Quote(item) =>
          add(item, ICUClassState.Literal(item))
        case ICUClassToken.Character(item, quoted) if quoted || !isICUClassSyntax(item) =>
          add(item, ICUClassState.Literal(item))
        case ICUClassToken.Character(item, _) if item.data.value == ']' =>
          assert(state != ICUClassState.Operator, "Invalid class set operation", item.loc.start)
          reset(item.loc.start)
          continues = false
        case ICUClassToken.Character(item, _) if item.data.value == '[' => addNested(tokenStart, None)
        case ICUClassToken.Character(item, _)                           =>
          val c = item.data.value.toChar
          val save = offset
          val next = readICUClassToken()
          def isNext(d: Char): Boolean = next match
            case ICUClassToken.Character(nextItem, false) => nextItem.data.value == d
            case _                                        => false

          def addCharacter(): Unit =
            reset(save)
            add(item, ICUClassState.Literal(item))

          state match
            case ICUClassState.Start if isNext(c)             => fail("Invalid class set operation", tokenStart)
            case ICUClassState.Start | ICUClassState.Operator => addCharacter()
            case _ if isNext(c)                               =>
              operator = Some(s"$c$c")
              continues = false
            case ICUClassState.Set if isNext('[') =>
              val build: (ClassItem, ClassItem) => ClassItemData =
                if c == '-' then ClassItemData.ClassNestDiff(_, _) else ClassItemData.ClassNestIntersection(_, _)
              addNested(offset - 1, Some(build))
            case ICUClassState.Literal(beginItem) if c == '-' && !isNext('[') && !isNext(']') =>
              items.dropRightInPlace(items.reverseIterator.takeWhile(isClassBlank).size + 1)
              val (beforeBegin, begin) = icuClassRangeEnd(beginItem, last = true)
              items ++= beforeBegin
              items ++= parseICUClassRange(begin, next, save)
              state = ICUClassState.Range
            case _ => addCharacter()
        case ICUClassToken.Escape(letter) =>
          letter match
            case 'p' | 'P' => addClass(parseICUPropertyClass(letter == 'P'), tokenStart, ICUClassState.Set)
            case 'N'       =>
              val BackslashKind.EscapeClass(kind) = parseBackslashUpperN(): @unchecked
              addClass(kind, tokenStart, ICUClassState.Range)
            case _ if ICUClassEscapes.contains(letter) =>
              next()
              addClass(ICUClassEscapes(letter), tokenStart, ICUClassState.Range)
            case _ =>
              val item = parseICUClassLetter(tokenStart)
              add(item, ICUClassState.Literal(item))
    depth = savedDepth
    (union, operator)

  /** Parses the range of ICU's class from `begin` to the token, whose escape starts at `start`, and returns the items
    * of the range.
    *
    * A range to an escape class or a property is an error (e.g. `[a-\w]`), and a range to `\N{...}` is `begin` and the
    * character of `\N{...}`, which recheck does not model.
    */
  private def parseICUClassRange(begin: ClassItem.Value, token: ICUClassToken, start: Int): Seq[ClassItem] =
    def range(last: ClassItem.Value): ClassItem =
      assert(begin.data.value <= last.data.value, "Out of order class range", begin.loc.start)
      ClassItem(ClassItemData.ClassRange(begin, last), SourceLocation(begin.loc.start, last.loc.end))

    token match
      case ICUClassToken.Character(item, _) => Seq(range(item))
      case ICUClassToken.Quote(item)        =>
        val (afterEnd, end) = icuClassRangeEnd(item, last = false)
        range(end) +: afterEnd
      case ICUClassToken.End         => fail("Unclosed ']'")
      case ICUClassToken.Escape('N') =>
        val BackslashKind.EscapeClass(kind) = parseBackslashUpperN(): @unchecked
        Seq(begin, ClassItem(ClassItemData.ClassBackslashClass(kind), SourceLocation(start, offset)))
      case ICUClassToken.Escape(letter) =>
        assert(!ICUClassEscapes.contains(letter) && letter != 'p' && letter != 'P', "Invalid class range", start)
        Seq(range(parseICUClassLetter(start)))

  /** Returns the value of a character or a quote of ICU's class at an end of a range, and the other characters of the
    * quote, which are before the value at the begin (`last`) and after it at the end.
    *
    * A quote of one character is the value with the quote as its form (e.g. `\Qc\E` of `[a-\Qc\E]`), and the other
    * characters of a longer quote are out of it (e.g. `[\Qab\E-c]` is `[ab-c]`), as in [[parseClassRange]].
    */
  private def icuClassRangeEnd(item: ClassItem, last: Boolean): (Seq[ClassItem], ClassItem.Value) =
    quotedValue(item) match
      case Some(value) => (Seq.empty, value)
      case None        =>
        val literals = classQuoteLiterals(item).getOrElse(Seq(item))
        val (others, endItem) = if last then (literals.init, literals.last) else (literals.tail, literals.head)
        endItem match
          case value: ClassItem.Value => (others, value)
          // $COVERAGE-OFF$
          case _ => throw new IllegalStateException(s"Not a character: $endItem")
          // $COVERAGE-ON$

  /** Parses the character of an escape in ICU's class that is the character itself (e.g. `\b`), whose backslash is at
    * `start`.
    */
  private def parseICUClassLetter(start: Int): ClassItem.Value =
    val value = currentCodePoint
    nextCodePoint(value)
    ClassItem.Value(ClassItemData.ClassBackslashValue(BackslashKind.Unknown(value)), SourceLocation(start, offset))

  /** The escape classes of ICU's class, which are the escapes that a `-` after them reads as a character. */
  private val ICUClassEscapes: Map[Char, EscapeClassKind] = Map(
    'd' -> EscapeClassKind.Digit,
    'D' -> EscapeClassKind.NonDigit,
    'h' -> EscapeClassKind.Horizontal,
    'H' -> EscapeClassKind.NonHorizontal,
    's' -> EscapeClassKind.Space,
    'S' -> EscapeClassKind.NonSpace,
    'v' -> EscapeClassKind.Vertical,
    'V' -> EscapeClassKind.NonVertical,
    'w' -> EscapeClassKind.Word,
    'W' -> EscapeClassKind.NonWord
  )

  /** Checks whether the character of ICU's class is its syntax, unless an escape or `\Q...\E` quotes it. */
  private def isICUClassSyntax(item: ClassItem.Value): Boolean =
    val c = item.data.value
    c == '[' || c == ']' || c == '-' || c == '&'

  /** Reads a token of ICU's class, where the escapes of characters that ICU's scanner reads are characters (e.g.
    * `\x41`), and `\Q...\E` quotes the characters in it.
    *
    * An empty `\Q\E` is skipped as the blanks before a token are, which the token does not keep.
    */
  private def readICUClassToken(): ICUClassToken =
    var token: Option[ICUClassToken] = None
    while token.isEmpty do
      skipClassComment()
      if isEnd then token = Some(ICUClassToken.End)
      else if currentChar != '\\' then token = Some(ICUClassToken.Character(parseICUClassLiteral(), quoted = false))
      else
        val start = offset
        next()
        assert(!isEnd, "Invalid backslash escape", start)
        if currentChar == 'Q' then
          next()
          token = parseICUClassQuote(start)
        else if "acefnrtuUx0".contains(currentChar) then
          val (kind: BackslashKind.BackslashValue) = parseBackslashKind(): @unchecked
          val item = ClassItem.Value(ClassItemData.ClassBackslashValue(kind), SourceLocation(start, offset))
          token = Some(ICUClassToken.Character(item, quoted = true))
        else token = Some(ICUClassToken.Escape(currentChar))
    token.get

  /** Parses the characters of ICU's `\Q...\E` in a class after `\Q`, whose backslash is at `start`, and returns its
    * token, or `None` for an empty one.
    */
  private def parseICUClassQuote(start: Int): Option[ICUClassToken] =
    val parts = Seq.newBuilder[CasePart[ClassItem]]
    while !isEnd && !source.startsWith("\\E", offset) do
      val item = parseICUClassLiteral()
      parts.addOne(CasePart(item, Some(source.slice(item.loc.start, item.loc.end))))
    if isEnd then return Some(ICUClassToken.End)
    next()
    next()
    val region = ClassItemData.ClassCaseRegion(CaseCommandKind.QuoteCommand, parts.result(), CaseRegionEnd.Closed)
    Option.when(region.parts.nonEmpty)(ICUClassToken.Quote(ClassItem(region, SourceLocation(start, offset))))

  private def parseICUClassLiteral(): ClassItem.Value =
    val start = offset
    val value = currentCodePoint
    nextCodePoint(value)
    ClassItem.Value(ClassItemData.ClassLiteral(value), SourceLocation(start, offset))

  private val ClassPOSIXNames: Set[String] = Set(
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
  private def parseClassPOSIXOption(): Option[ClassItem] =
    val start = offset
    val bodyStart = offset + 2
    val bodyEnd = findClassPOSIXEnd(start) match
      case Some(end) => end
      case None      => return None

    if source.charAt(start + 1) != ':' then fail("Unsupported POSIX collating element", start)

    val body = source.slice(bodyStart, bodyEnd)
    val invert = body.startsWith("^")
    val name = if invert then body.drop(1) else body

    if ClassPOSIXNames.contains(name) then
      reset(bodyEnd + 2)
      return Some(ClassItem(ClassItemData.ClassPOSIX(invert, name), SourceLocation(start, offset)))

    if !featureSet.charClass.classPOSIXNameRule.looksLikeName(name, ClassPOSIXNames) then
      return Some(parseClassItemLiteral())

    fail("Unknown POSIX class", start)

  private def findClassPOSIXEnd(start: Int): Option[Int] =
    if start + 1 >= source.length then return None
    val terminator = source.charAt(start + 1)
    if !(terminator == ':' || featureSet.charClass.rejectsClassPOSIXCollating && (terminator == '.' || terminator == '='))
    then return None

    var i = start + 2
    while i + 1 < source.length do
      val c = source.charAt(i)
      val d = source.charAt(i + 1)
      if c == '\\' && (d == ']' || d == '\\') then i += 1
      else if c == '[' && d == terminator || c == ']' then return None
      else if c == terminator && d == ']' then return Some(i)
      i += 1
    None

  private val ClassSetSyntaxCharacters: Set[Char] = "()[]{}/-\\|".toSet

  private val ClassSetReservedDoublePunctuators: Set[Char] = "&!#$%*+,.:;<=>?@^`~".toSet

  private def isLiteralBackslashInClass: Boolean =
    !isEnd && ((currentChar: @switch) match
      case 'A' | 'z' | 'Z' | 'G' | 'K' | 'R' | 'X' => featureSet.charClass.treatsNonClassBackslashInClassAsLiteral
      case 'g' | 'k'                               => featureSet.charClass.treatsBackReferenceBackslashInClassAsLiteral
      case 'C'                                     => featureSet.charClass.treatsBackslashUpperCInClassAsLiteral
      case _                                       => false)

  private def parseClassQuote(start: Int): ClassItem =
    val parts = Seq.newBuilder[CasePart[ClassItem]]
    while !isEnd && !source.startsWith("\\E", offset) do
      val item = parseClassItemLiteral()
      parts.addOne(CasePart(item, Some(source.slice(item.loc.start, item.loc.end))))
    startWith("\\E")
    val region = ClassItemData.ClassCaseRegion(CaseCommandKind.QuoteCommand, parts.result(), CaseRegionEnd.Closed)
    ClassItem(region, SourceLocation(start, offset))

  private def parseClassItemLiteral(): ClassItem =
    if featureSet.charClass.validatesClassSetSyntax then
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
    assert(featureSet.quantifier.allowsBrokenCloseCurly, "Incomplete quantifier")
    val start = offset
    next()
    val end = offset
    stack.push(Node(NodeData.Literal('}'), SourceLocation(start, end)))

  private def parseCloseBracket(stack: mutable.Stack[Node]): Unit =
    assert(featureSet.charClass.allowsBrokenCloseBracket, "Unclosed ']'")
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

  /** Parses the body one level deeper. */
  private def nest[A](start: Int)(body: => A): A =
    enter(start)
    val value = body
    depth -= 1
    value

  private def enter(start: Int): Unit =
    if depth >= Parser.MaxNestingDepth then throw new NestingLimitException(Parser.MaxNestingDepth, start)
    depth += 1

  /** Validates the depth of a repeat of the node, where a repeat of a repeat is one deeper. */
  private def validateRepeatNesting(node: Node): Unit =
    @tailrec def loop(current: Node, repeatDepth: Int): Unit =
      current.data match
        case NodeData.Repeat(inner, _, _) =>
          if repeatDepth >= Parser.MaxNestingDepth then
            throw new NestingLimitException(Parser.MaxNestingDepth, node.loc.end)
          loop(inner, repeatDepth + 1)
        case _ => ()

    loop(node, depth)

  private def protectContext[A](body: => A): A =
    val savedContext = context
    val value = body
    context = savedContext
    value

  private def applyFlagSetDiff(diff: FlagSetDiff): Unit =
    // A single `x` turns `xx` off, and `-x` turns both off (Perl 5.44.0, PCRE2 10.49).
    if diff.added.verbose then context = context.copy(skipsComment = true, verboseMore = diff.added.verboseMore)
    if diff.removed.exists(_.verbose) then context = context.copy(skipsComment = false, verboseMore = false)
    if featureSet.inlineFlag.hasUnixLinesFlag && diff.added.hasIndices then context = context.copy(unixLines = true)
    if featureSet.inlineFlag.hasUnixLinesFlag && diff.removed.exists(_.hasIndices) then
      context = context.copy(unixLines = false)
    if diff.added.dupNames then context = context.copy(allowsDuplicatedGroupName = true)
    if diff.removed.exists(_.dupNames) then context = context.copy(allowsDuplicatedGroupName = false)
    if diff.added.explicitCapture then context = context.copy(explicitCapture = true)
    if diff.removed.exists(_.explicitCapture) then context = context.copy(explicitCapture = false)

  private def applyFlagSetDiffWith(diff: FlagSetDiff)(body: => Unit): Unit =
    protectContext:
      applyFlagSetDiff(diff)
      body

  private def applyResetFlag(flagSet: FlagSet): Unit =
    context = context.copy(skipsComment = flagSet.verbose, verboseMore = flagSet.verboseMore)
    if flagSet.dupNames then context = context.copy(allowsDuplicatedGroupName = true)
    context = context.copy(explicitCapture = flagSet.explicitCapture)

  private def applyResetFlagWith(flagSet: FlagSet)(body: => Unit): Unit =
    protectContext:
      applyResetFlag(flagSet)
      body

  /** Parses a group name of a reference, or a group number if the dialect allows one (e.g. python-regex's `(?P=1)`). */
  private def parseReferenceName(end: Char = ')'): String =
    if !featureSet.reference.allowsNumberReferenceName || !isDigit then return parseGroupNameBody(end)
    val start = offset
    val n = parseIntOption().get
    assert(n >= 1, "Invalid identifier", start)
    sliceSource(start, offset)

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

    // Boost.Regex takes any characters before the end as a name, even none (e.g. `(?<1-a>x)` and `(?<>x)`).
    if featureSet.group.allowsAnyGroupName then
      while !isEnd && currentChar != end do next()
      return sliceSource(start, offset)

    if featureSet.group.allowsLooseGroupName then
      assert(
        !isEnd && currentChar != end && currentChar != '-' && !Character.isDigit(currentCodePoint),
        "Invalid identifier"
      )
      next()
      while !isEnd && currentChar != end && !(levelable && (currentChar == '+' || currentChar == '-')) do
        assert(currentChar != ')', "Invalid identifier")
        next()
      return sliceSource(start, offset)

    if featureSet.group.allowsNumberGroupName && isDigit then
      val n = parseIntOption().get
      assert(n != 0, "Invalid identifier", start)
      return sliceSource(start, offset)

    val name = new java.lang.StringBuilder
    boundary:
      while !isEnd do
        skipConstructComment()
        val save = offset
        val c =
          if featureSet.group.allowsEscapeInGroupName && currentChar == '\\' then parseGroupNameEscape()
          else
            val c = currentCodePoint
            nextCodePoint(c)
            c
        if c < 0 || !(if name.length == 0 then isGroupNameStart(c) else isGroupNameContinue(c)) then
          reset(save)
          boundary.break()
        name.appendCodePoint(c)

    assert(name.length > 0, "Invalid identifier", start)
    if featureSet.group.limitsGroupNameLength then assert(offset - start <= 128, "Invalid identifier", start)
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
    if UTF16Encoding.isHighSurrogate(high) && source.startsWith("\\u", offset) then
      val save = offset
      reset(offset + 2)
      val low = parseHex4()
      if UTF16Encoding.isLowSurrogate(low) then return UTF16Encoding.decodeSurrogatePair(high, low)
      reset(save)
    high

  /** Tests whether `count` hex digits follow, between which Java skips whitespace and comments (e.g. `\\u00 41`). */
  private def hasHexDigitsAhead(count: Int): Boolean =
    // `next` skips the comments that Ruby removes, but Ruby reads `\x` and `\u` before removing them.
    if !isSkippingEscapeComment then
      return offset + count <= source.length && source.slice(offset, offset + count).forall(isHexDigit(_))
    val save = offset
    var found = 0
    while found < count && isHexDigit do
      next()
      skipComment()
      found += 1
    reset(save)
    found == count

  private def parseHex4(): Int =
    val start = offset
    var i = 0
    while i < 4 do
      if !isHexDigit then return -1
      next()
      i += 1
    Integer.parseInt(sliceSource(start, offset), 16)

  private def isGroupNameStart: Boolean =
    !isEnd && isGroupNameStart(currentCodePoint)

  private def isGroupNameStart(c: Int): Boolean =
    if 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' then true
    else if c == '_' then featureSet.group.allowsUnderscoreInGroupName
    else if c == '$' then featureSet.group.allowsDollarInGroupName
    else if c < 0x80 then false
    else if featureSet.group.allowsUnicodeIdentifierGroupName then Character.isUnicodeIdentifierStart(c)
    else if featureSet.group.allowsUnicodeWordGroupName then
      (c < 0x10000 || featureSet.readsAsUnicode) && Character.isLetter(c)
    else if featureSet.group.allowsWordClassGroupName then isDotNetWord(c)
    else if featureSet.group.allowsWordIdentifierGroupName then Parser.PerlGroupNameStart.contains(c)
    else false

  private def isGroupNameContinue(c: Int): Boolean =
    if c < 0x80 then isDigit(c) || isGroupNameStart(c)
    else if featureSet.group.allowsUnicodeIdentifierGroupName then Character.isUnicodeIdentifierPart(c)
    else if featureSet.group.allowsUnicodeWordGroupName then
      (c < 0x10000 || featureSet.readsAsUnicode) && Character.isLetterOrDigit(c)
    else if featureSet.group.allowsWordClassGroupName then isDotNetWord(c)
    else if featureSet.group.allowsWordIdentifierGroupName then Parser.PerlGroupNameContinue.contains(c)
    else false

  /** Parses a name or a value of JavaScript's `\\p{...}`, which is an ASCII letter or `_` followed by them and digits.
    */
  private def parsePropertyID(): String =
    val start = offset
    assert(isAlphabetic || !isEnd && currentChar == '_', "Invalid identifier")
    next()
    while isAlphabetic || isDigit || !isEnd && currentChar == '_' do next()
    sliceSource(start, offset)

  /** Tests whether .NET's `RegexCharClass.IsBoundaryWordChar` takes the character, which reads a UTF-16 unit, so a
    * supplementary character is not one.
    */
  private def isDotNetWord(c: Int): Boolean =
    (c < 0x10000 || featureSet.readsAsUnicode) && DotNetCharClass.BoundaryWord.contains(c)

  private def parseIntOption(): Option[Int] =
    if !isDigit then return None

    val start = offset
    while isDigit do next()
    val end = offset

    val str = sliceSource(start, end)
    str.toIntOption.orElse(fail("Invalid integer", start))

  /** Reads a number of `radix` in at most `maxLength` characters as `toi` of Boost.Regex's traits does, and returns it,
    * or -1 without moving if none.
    *
    * [[NumberSyntax.Stream]] reads none out of `std::intmax_t`, and stops before `,`, which is the thousands separator
    * of the C locale (e.g. `a{1,2}`). [[NumberSyntax.UnicodeDigits]] returns -1 after the digit that makes the number
    * above `INTMAX_MAX / radix`.
    */
  private def parseTraitsNumber(radix: Int, maxLength: Int = Int.MaxValue): Long =
    val start = offset
    var length = 0
    def has(p: Int => Boolean): Boolean =
      length < maxLength && !isEnd && currentChar != ',' && p(currentCodePoint)
    def take(): Unit =
      nextCodePoint(currentCodePoint)
      length += 1

    if featureSet.numberSyntax == NumberSyntax.UnicodeDigits then
      if !has(unicodeDigit(_, radix) >= 0) then return -1
      var value = 0L
      while has(unicodeDigit(_, radix) >= 0) do
        value = value * radix + unicodeDigit(currentCodePoint, radix)
        take()
        if value > Long.MaxValue / radix then return -1
      return value

    while has(c => c == ' ' || '\t' <= c && c <= '\r') do take()
    val negative = has(_ == '-')
    if negative || has(_ == '+') then take()
    if radix == 16 && has(_ == '0') && source.lift(offset + 1).exists(c => c == 'x' || c == 'X') then
      take()
      take()
    val digitStart = offset
    var value = 0L
    while has(Character.digit(_, radix) >= 0) do
      if value > (Long.MaxValue - Character.digit(currentChar, radix)) / radix then
        reset(start)
        return -1
      value = value * radix + Character.digit(currentChar, radix)
      take()
    if offset == digitStart then
      reset(start)
      return -1
    if negative then -value else value

  /** Returns the value of a digit of ICU's `u_digit` in `radix`, or -1 if none.
    *
    * It is a decimal digit of Unicode, or an ASCII or fullwidth Latin letter for 10 and above.
    */
  private def unicodeDigit(c: Int, radix: Int): Int =
    val value =
      if 'a' <= c && c <= 'z' then c - 'a' + 10
      else if 'A' <= c && c <= 'Z' then c - 'A' + 10
      else if 0xff41 <= c && c <= 0xff5a then c - 0xff41 + 10
      else if 0xff21 <= c && c <= 0xff3a then c - 0xff21 + 10
      else
        Parser.DecimalDigits.intervals
          .find((start, end) => start <= c && c < end)
          .fold(-1)((start, _) => (c - start) % 10)
    if value < radix then value else -1

  private def parseSaturatedIntOption(max: Long, message: String, pos: Int): Option[Int] =
    if !isDigit then return None

    var value = 0L
    while isDigit do
      value = if value > (Long.MaxValue - 9) / 10 then Long.MaxValue else value * 10 + (currentChar - '0')
      next()
      skipConstructComment()

    if value > max then fail(message, pos)
    Some(value.min(Int.MaxValue).toInt)

  private def parseHexDigits(size: Int): Long =
    var value = 0L
    var count = 0
    while count < size && isHexDigit do
      value = (value * 16 + Character.digit(currentChar, 16)).min(Long.MaxValue / 16)
      count += 1
      next()
      skipEscapeComment()
    value

  private def parseBracedCodePoint(radix: Int): Int =
    skipBraceBlank()
    skipEscapeComment()
    val start = offset
    var value = 0L
    var count = 0
    while !isEnd && (if radix == 8 then isOctalDigit else isHexDigit) do
      value = (value * radix + Character.digit(currentChar, radix)).min(Long.MaxValue / 16)
      count += 1
      next()
      skipEscapeComment()

    // Perl reads `\x{}` as `\x{0}`, but rejects `\o{}`.
    if featureSet.escape.allowsLooseBraceEscape && (count > 0 || radix == 16 || currentChar != '}') then
      while !isEnd && currentChar != '}' do next()
    else
      assert(count > 0, "Invalid backslash escape")
      skipBraceBlank()
    expect('}', "Invalid backslash escape")
    validateCodePoint(value, start)

  private def validateCodePoint(value: Long, pos: Int): Int =
    val max: Long =
      if featureSet.escape.allowsCodePointAbove10FFFF then Long.MaxValue
      else if featureSet.limitsCodePointTo0xFF then 0xff
      else 0x10ffff
    if value > max then fail("Code point too large", pos)
    // Ruby's `\u` escapes are Unicode, which an ASCII-8BIT regexp cannot have except ASCII.
    if featureSet.escape.rejectsNonASCIIUnicodeEscape && value >= 0x80 then fail("Incompatible character encoding", pos)
    if !featureSet.escape.allowsSurrogateEscape && value.isValidInt && UTF16Encoding.isSurrogate(value.toInt) then
      fail("Invalid Unicode escape", pos)
    value.min(Int.MaxValue).toInt

  /** Skips the whitespace and `#` comments of the extended mode (`x`), and returns whether it skipped any. */
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
        skipLineComment()
        continue = true
        skipped = true

    skipped

  /** Skips a `#` comment up to its line terminator, which Java keeps (e.g. U+2028 after it is a literal). */
  private def skipLineComment(): Unit =
    val terminators = if context.unixLines then LineTerminatorSet.LF else featureSet.comment.commentTerminators
    val hasCRLF = terminators.crlf != LineTerminatorSet.CRLF.Ignored
    def isCRLF: Boolean = hasCRLF && source.startsWith("\r\n", offset)
    // Java's pattern buffer ends with NUL, so a NUL also ends a comment.
    def isTerminator: Boolean =
      terminators.chars.contains(
        currentChar.toInt
      ) || isCRLF || featureSet.comment.endsCommentAtNUL && currentChar == '\u0000'

    while !isEnd && !isTerminator do
      if featureSet.comment.processBackslashInLineComment && currentChar == '\\' then
        val start = offset
        next()
        assert(!isEnd, "Invalid backslash escape", start)
        next()
      else next()
    if !isEnd && !featureSet.comment.keepsCommentEnd then reset(offset + (if isCRLF then 2 else 1))

  /** Skips the whitespace and comments that Java also skips inside a construct (e.g. `(?<n a>x)` and `a{1 ,2}`). */
  private def skipConstructComment(): Unit =
    if isSkippingConstructComment then skipComment()

  private def isSkippingConstructComment: Boolean =
    context.skipsComment && featureSet.comment.skipsCommentInConstruct

  /** Skips the whitespace and comments that Java also skips inside an escape (e.g. `\\x4 1`), unlike ICU. */
  private def skipEscapeComment(): Unit =
    if isSkippingEscapeComment then skipComment()

  private def isSkippingEscapeComment: Boolean =
    context.skipsComment && featureSet.comment.skipsCommentInEscape

  /** Returns the source text in the given range without the comments Ruby removes. */
  private def sliceSource(start: Int, end: Int): String =
    if removedCommentEnds.isEmpty then return source.slice(start, end)
    val builder = new java.lang.StringBuilder
    var i = start
    while i < end do
      removedCommentEnds.get(i) match
        case Some(commentEnd) => i = commentEnd
        case None             =>
          builder.append(source.charAt(i))
          i += 1
    builder.toString

  /** Returns the source text from the position to here, without the comments that Ruby removes at its end. */
  private def writtenText(start: Int): String =
    var end = offset
    while removedCommentStarts.contains(end) do end = removedCommentStarts(end)
    source.slice(start, end)

  /** Returns the source text from the position to here, or `None` if it is the plain text. */
  private def writtenUnlessPlain(start: Int, plain: String): Option[String] =
    val text = writtenText(start)
    Option.when(text != plain)(text)

  /** Returns the escape whose text after its letters starts at the position. */
  private def escape(style: EscapeStyle, value: Int, textStart: Int): BackslashKind.Escape =
    BackslashKind.Escape(style, value, writtenUnlessPlain(textStart, PlainText.escape(style, value, featureSet)))

  private def next(): Unit = reset(offset + 1)

  private def nextCodePoint(codePoint: Int): Unit =
    reset(if codePoint >= 0x10000 then offset + 2 else offset + 1)

  private def reset(save: Int): Unit =
    offset = save
    while removedCommentEnds.contains(offset) do offset = removedCommentEnds(offset)
    currentChar = if offset >= source.length then '\u0000' else source.charAt(offset)

  private def currentCodePoint: Int =
    val codePoint1 = currentChar.toInt
    if UTF16Encoding.isHighSurrogate(codePoint1) && offset + 1 < source.length then
      val codePoint2 = source.charAt(offset + 1).toInt
      if UTF16Encoding.isLowSurrogate(codePoint2) then UTF16Encoding.decodeSurrogatePair(codePoint1, codePoint2)
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
    isEnd || currentChar == '|' || currentChar == ')' || isNewlineAlternation

  /** Checks whether the current character is a line feed that Boost.Regex's `N` (`newline_alt`) reads as `|`. */
  private def isNewlineAlternation: Boolean =
    featureSet.readsNewlineAsAlternation && !isEnd && currentChar == '\n'

  /** Tests whether the current character is whitespace of the extended mode (`x`).
    *
    * PCRE and Perl skip Pattern_White_Space, whose U+0085 is also a byte of their byte modes. python-regex skips the
    * characters of Python's `str.isspace`, which a `bytes` pattern reads as Latin-1.
    */
  private def isSpace: Boolean =
    !isEnd && ((currentChar: @switch) match
      case '\t' | '\f' | '\r' | ' '       => true
      case '\n'                           => !featureSet.readsNewlineAsAlternation
      case '\u000b'                       => featureSet.comment.skipsVerticalTab
      case '\u0085' | '\u2028' | '\u2029' =>
        featureSet.comment.skipsPatternWhiteSpace || featureSet.comment.skipsPythonSpace
      case '\u200e' | '\u200f'                                  => featureSet.comment.skipsPatternWhiteSpace
      case '\u001c' | '\u001d' | '\u001e' | '\u001f'            => featureSet.comment.skipsPythonSpace
      case '\u00a0' | '\u1680' | '\u202f' | '\u205f' | '\u3000' => featureSet.comment.skipsPythonSpace
      case '\u2000' | '\u2001' | '\u2002' | '\u2003' | '\u2004' | '\u2005' | '\u2006' | '\u2007' | '\u2008' | '\u2009' |
          '\u200a' =>
        featureSet.comment.skipsPythonSpace
      case _ => false)

  private def isDigit: Boolean =
    !isEnd && isDigit(currentChar)

  private def isDigit(c: Int): Boolean =
    '0' <= c && c <= '9'

  private def isHexDigit: Boolean =
    !isEnd && isHexDigit(currentChar)

  private def isHexDigit(c: Int): Boolean =
    isDigit(c) || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F'

  private def isOctalDigit: Boolean =
    !isEnd && isOctalDigit(currentChar)

  private def isOctalDigit(c: Int): Boolean =
    '0' <= c && c <= '7'

  private def isControl: Boolean =
    if isEnd then false
    else if featureSet.escape.hasBackslashControlAny then true
    else if featureSet.escape.hasBackslashControlPrintable then
      0x20 <= currentChar && currentChar <= 0x7e && (currentChar != '{' || featureSet.escape.allowsBackslashControlCurly)
    else
      isAlphabetic(currentChar) ||
      featureSet.escape.hasBackslashControlExtra && '@' <= currentChar && currentChar <= '_' ||
      featureSet.charClass.hasBackslashControlDigitInClass && context.inClass && (isDigit || currentChar == '_')

  private def isAlphabetic: Boolean =
    !isEnd && isAlphabetic(currentChar)

  private def isAlphabetic(c: Int): Boolean =
    'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'

  private def assert(condition: Boolean, message: String, pos: Int = offset): Unit =
    if !condition then fail(message, pos)

  private def fail(message: String, pos: Int = offset): Nothing =
    throw new ParsingException(message, Some(pos))
