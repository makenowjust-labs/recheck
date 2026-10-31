package codes.quine.labs.resyntax.parser

import scala.collection.mutable
import scala.util.boundary

import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.FullCaseFolding
import codes.quine.labs.resyntax.charset.FullCaseMapping
import codes.quine.labs.resyntax.charset.Properties
import codes.quine.labs.resyntax.charset.UString

/** PerlLiteralLexer rewrites the text of a Perl regex literal as Perl's lexer does before the regex compiler reads it.
  *
  * The lexer applies the case modification and quoting commands (`\U`, `\L`, `\u`, `\l`, `\F`, `\Q` and `\E`) to the
  * text: each command wraps the text up to its `\E` (or the end) in a call of `uc`, `lc`, `ucfirst`, `lcfirst`, `fc` or
  * `quotemeta`, regardless of the regex syntax (e.g. `[A-\LZ]` is `[A-z]`, and `\U\d` is `\D`). It follows
  * `S_scan_const` and `yyl_interpcasemod` of perl 5.44's `toke.c`.
  */
private[parser] object PerlLiteralLexer:

  /** Rewritten is a rewritten text, where each code unit keeps the range of the source that it comes from. */
  final class Rewritten(val text: String, starts: Array[Int], ends: Array[Int], sourceLength: Int):

    /** Returns the source position of a position in the text. */
    def position(pos: Int): Int = if pos >= text.length then sourceLength else starts(pos)

    /** Returns the source location of a location in the text.
      *
      * The removed commands at the start and the end belong to the nodes there, and the others to no node.
      */
    def location(loc: SourceLocation): SourceLocation =
      if loc.start < 0 then loc
      else
        val start = if loc.start == 0 then 0 else position(loc.start)
        val end =
          if loc.end >= text.length then sourceLength else if loc.end > loc.start then ends(loc.end - 1) else start
        SourceLocation(start, end)

    /** Returns the node whose locations are moved to the source. */
    def relocate(node: Node): Node = Node(relocateData(node.data), location(node.loc))

    private def relocateData(data: NodeData): NodeData = data match
      case NodeData.Disjunction(nodes)  => NodeData.Disjunction(nodes.map(relocate))
      case NodeData.Sequence(nodes)     => NodeData.Sequence(nodes.map(relocate))
      case NodeData.Repeat(node, q)     => NodeData.Repeat(relocate(node), q)
      case NodeData.Group(kind, node)   => NodeData.Group(kind, relocate(node))
      case NodeData.Command(kind)       => NodeData.Command(relocateCommand(kind))
      case NodeData.Class(invert, item) => NodeData.Class(invert, relocateItem(item))
      case data                         => data

    private def relocateCommand(kind: CommandKind): CommandKind = kind match
      case CommandKind.BranchReset(nodes)         => CommandKind.BranchReset(nodes.map(relocate))
      case CommandKind.Conditional(test, yes, no) =>
        CommandKind.Conditional(relocateTest(test), relocate(yes), no.map(relocate))
      case CommandKind.AbsenceExpression(absent, exp) => CommandKind.AbsenceExpression(relocate(absent), relocate(exp))
      case CommandKind.AbsenceStopper(absent)         => CommandKind.AbsenceStopper(relocate(absent))
      case kind                                       => kind

    private def relocateTest(test: ConditionalTest): ConditionalTest = test match
      case test: ConditionalTest.LookAround           => relocateLookAround(test)
      case ConditionalTest.CalloutLookAround(c, test) => ConditionalTest.CalloutLookAround(c, relocateLookAround(test))
      case test                                       => test

    private def relocateLookAround(test: ConditionalTest.LookAround): ConditionalTest.LookAround =
      test.copy(node = relocate(test.node))

    private def relocateItem(item: ClassItem): ClassItem = item match
      case item: ClassItem.Value         => relocateValue(item)
      case ClassItem.NonValue(data, loc) => ClassItem.NonValue(relocateItemData(data), location(loc))

    private def relocateValue(item: ClassItem.Value): ClassItem.Value = item.copy(loc = location(item.loc))

    private def relocateItemData(data: ClassItemData): ClassItemData = data match
      case ClassItemData.ClassUnion(items)       => ClassItemData.ClassUnion(items.map(relocateItem))
      case ClassItemData.ClassIntersection(l, r) => ClassItemData.ClassIntersection(relocateItem(l), relocateItem(r))
      case ClassItemData.ClassDiff(l, r)         => ClassItemData.ClassDiff(relocateItem(l), relocateItem(r))
      case ClassItemData.ClassNest(invert, item) => ClassItemData.ClassNest(invert, relocateItem(item))
      case ClassItemData.ClassRange(begin, end)  => ClassItemData.ClassRange(relocateValue(begin), relocateValue(end))
      case data                                  => data

  /** Rewrites the regex literal, or returns `None` if it has no command.
    *
    * @param extended
    *   whether the literal has the `x` flag, where the lexer skips `#` comments
    */
  def rewrite(source: String, extended: Boolean): Option[Rewritten] =
    if !source.contains('\\') then None else new PerlLiteralLexer(source, extended).run()

  /** The non-ASCII characters that `quotemeta` quotes in a UTF-8 string (perlfunc `quotemeta`). */
  private lazy val QuotedNonAscii: CharSet =
    val names = Seq("Pattern_Syntax", "Pattern_White_Space", "White_Space", "Default_Ignorable_Code_Point")
    CharSet.union(names.map(Properties.binary(_).get.set) :+ Properties.GeneralCategory.value("Cc").get.set)

  private def isQuoted(c: Int): Boolean =
    if c < 0x80 then !(Character.isLetterOrDigit(c) || c == '_') else QuotedNonAscii.contains(c)

  private def isCommand(c: Char): Boolean = "lLuUEQF".indexOf(c.toInt) >= 0

  private def isLUF(c: Char): Boolean = c == 'L' || c == 'U' || c == 'F'

private final class PerlLiteralLexer(source: String, extended: Boolean):
  import PerlLiteralLexer.*

  /** The source, where the lexer swaps `\L\u` and `\U\l` in place. */
  private val src: Array[Char] = source.toCharArray

  private val n: Int = src.length

  private var pos: Int = 0

  private var changed: Boolean = false

  /** Frame is an open command (or the whole text) and its text so far. */
  private final class Frame(val command: Char):
    val text: java.lang.StringBuilder = new java.lang.StringBuilder
    val starts: mutable.ArrayBuilder.ofInt = new mutable.ArrayBuilder.ofInt
    val ends: mutable.ArrayBuilder.ofInt = new mutable.ArrayBuilder.ofInt

    /** Whether the command has any argument, without which Perl's parser reports a syntax error on its closing. */
    var hasContent: Boolean = false

    def add(unit: Char, start: Int, end: Int): Unit =
      text.append(unit)
      starts.addOne(start)
      ends.addOne(end)

    def addCodePoints(cs: UString, start: Int, end: Int): Unit =
      for c <- cs.iterator do
        if Character.isBmpCodePoint(c) then add(c.toChar, start, end)
        else
          add(Character.highSurrogate(c), start, end)
          add(Character.lowSurrogate(c), start, end)

  private val root: Frame = new Frame('\u0000')

  private var stack: List[Frame] = Nil

  private def top: Frame = stack.headOption.getOrElse(root)

  def run(): Option[Rewritten] =
    var inCaseMod = false
    boundary:
      while true do
        if inCaseMod then inCaseMod = caseModifier()
        else if pos < n then
          scanChunk()
          inCaseMod = pos < n
        else if stack.nonEmpty then inCaseMod = true
        else boundary.break()
    if !changed then None
    else Some(new Rewritten(root.text.toString, root.starts.result(), root.ends.result(), n))

  /** Processes a command at `\`, or the end, and returns whether the next is a command again. */
  private def caseModifier(): Boolean =
    changed = true
    if pos >= n || src(pos + 1) == 'E' then
      if stack.nonEmpty then
        val command = pop()
        if pos < n && "LUQF".indexOf(command.toInt) >= 0 then
          pos += 2
          false
        else pos < n || stack.nonEmpty
      else
        if pos < n then pos += 2
        false
    else
      val s = pos + 1
      if s + 2 < n && src(s + 1) == '\\' && src(s + 2) == 'E' then
        pos = s + 3
        return false
      if s + 2 < n && src(s + 1) == '\\' && (src(s) == 'L' && src(s + 2) == 'u' || src(s) == 'U' && src(s + 2) == 'l')
      then
        val c = src(s)
        src(s) = src(s + 2)
        src(s + 2) = c
      val command = src(s)
      if isLUF(command) && stack.exists(f => isLUF(f.command)) then
        if !stack.head.hasContent then throw new ParsingException("Syntax error", Some(pos))
        pop()
        true
      else
        top.hasContent = true
        stack = new Frame(command) :: stack
        pos = s + 1
        false

  /** Closes the innermost command, and adds its result to the outer one. */
  private def pop(): Char =
    val frame = stack.head
    stack = stack.tail
    val text = frame.text.toString
    val starts = frame.starts.result()
    val ends = frame.ends.result()
    val out = top
    var i = 0
    while i < text.length do
      val c = text.codePointAt(i)
      val len = Character.charCount(c)
      val (start, end) = (starts(i), ends(i + len - 1))
      frame.command match
        case 'Q' =>
          if isQuoted(c) then out.add('\\', start, end)
          out.addCodePoints(UString(c), start, end)
        case 'U' => out.addCodePoints(FullCaseMapping.upper(c), start, end)
        case 'L' => out.addCodePoints(FullCaseMapping.lower(c), start, end)
        case 'F' => out.addCodePoints(FullCaseFolding(c), start, end)
        case 'u' => out.addCodePoints(if i == 0 then FullCaseMapping.title(c) else UString(c), start, end)
        case 'l' => out.addCodePoints(if i == 0 then FullCaseMapping.lower(c) else UString(c), start, end)
        case _   => throw new IllegalStateException(s"Unknown command: ${frame.command}")
      i += len
    frame.command

  /** Copies the text up to the next command or the end, as `S_scan_const` does for a pattern. */
  private def scanChunk(): Unit =
    val frame = top
    val chunkStart = pos
    val before = frame.text.length
    var inClass = false
    boundary:
      while pos < n do
        val c = src(pos)
        // `toke.c` tracks a class only to find comments, and ends it at the first `]` (e.g. in `[]` or `[[:a:]`).
        if c == '[' && !inClass then inClass = !isEscaped(chunkStart)
        else if c == ']' && inClass then inClass = isEscaped(chunkStart)
        else if c == '(' && !inClass && pos + 2 < n && src(pos + 1) == '?' && src(pos + 2) == '#' then
          while pos + 1 < n && src(pos) != ')' do copyUnit(frame)
        else if c == '#' && extended && !inClass then
          while pos < n && src(pos) != '\n' do copyUnit(frame)
          if pos >= n then boundary.break()
        if src(pos) == '\\' && pos + 1 < n then
          if isCommand(src(pos + 1)) then boundary.break()
          if src(pos + 1) == 'N' && pos + 2 < n && src(pos + 2) == '{' && !isCurlyQuantifier(pos + 2) then
            // The lexer reads `\N{...}` whole (and converts a name to `\N{U+...}`, which is not done here).
            val end = source.indexOf('}', pos + 3)
            val stop = if end < 0 then n else end + 1
            while pos < stop do copyUnit(frame)
          else
            copyUnit(frame)
            copyCodePoint(frame)
        else copyCodePoint(frame)
    if frame.text.length > before then frame.hasContent = true

  private def isEscaped(chunkStart: Int): Boolean =
    var i = pos - 1
    while i >= chunkStart && src(i) == '\\' do i -= 1
    (pos - 1 - i) % 2 == 1

  /** Checks whether the `{` at the position starts a curly quantifier, as `regcurly` does. */
  private def isCurlyQuantifier(open: Int): Boolean =
    var i = open + 1
    var digits = false
    def skipBlank(): Unit = while i < n && (src(i) == ' ' || src(i) == '\t') do i += 1
    def skipDigits(): Unit = while i < n && src(i).isDigit do
      digits = true
      i += 1
    skipBlank()
    skipDigits()
    skipBlank()
    if i < n && src(i) == ',' then
      i += 1
      skipBlank()
      skipDigits()
      skipBlank()
    digits && i < n && src(i) == '}'

  private def copyUnit(frame: Frame): Unit =
    frame.add(src(pos), pos, pos + 1)
    pos += 1

  private def copyCodePoint(frame: Frame): Unit =
    if pos + 1 < n && src(pos).isHighSurrogate && src(pos + 1).isLowSurrogate then
      frame.add(src(pos), pos, pos + 2)
      frame.add(src(pos + 1), pos, pos + 2)
      pos += 2
    else copyUnit(frame)
