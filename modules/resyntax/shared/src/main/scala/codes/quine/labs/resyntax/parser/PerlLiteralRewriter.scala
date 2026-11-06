package codes.quine.labs.resyntax.parser

import scala.collection.mutable
import scala.util.boundary

import codes.quine.labs.resyntax.ast.CaseCommandKind
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.FullCaseFolding
import codes.quine.labs.resyntax.charset.FullCaseMapping
import codes.quine.labs.resyntax.charset.PropertyTable
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UTF16Encoding

/** PerlLiteralRewriter rewrites the text of a Perl regex literal as Perl's lexer does before the regex compiler reads
  * it.
  *
  * The lexer applies the case modification and quoting commands (`\U`, `\L`, `\u`, `\l`, `\F`, `\Q` and `\E`) to the
  * text: each command wraps the text up to its `\E` (or the end) in a call of `uc`, `lc`, `ucfirst`, `lcfirst`, `fc` or
  * `quotemeta`, regardless of the regex syntax (e.g. `[A-\LZ]` is `[A-z]`, and `\U\d` is `\D`). It follows
  * `S_scan_const` and `yyl_interpcasemod` of perl 5.44's `toke.c`.
  */
private[parser] object PerlLiteralRewriter:

  /** Rewrites the regex literal, or returns `None` if it has no command.
    *
    * With `extended` (the `x` flag), the lexer skips `#` comments. With `bytes`, the literal is a byte string, whose
    * non-ASCII bytes the case commands do not change.
    */
  def rewrite(source: String, extended: Boolean, bytes: Boolean): Option[Rewritten] =
    if !source.contains('\\') then None else new PerlLiteralRewriter(source, extended, bytes).run()

  /** The non-ASCII characters that `quotemeta` quotes in a UTF-8 string (perlfunc `quotemeta`). */
  private lazy val QuotedNonASCII: CharSet =
    val names = Seq("Pattern_Syntax", "Pattern_White_Space", "White_Space", "Default_Ignorable_Code_Point")
    CharSet.union(names.map(PropertyTable.binary(_).get.set) :+ PropertyTable.GeneralCategory.value("Cc").get.set)

  private def isQuoted(c: Int): Boolean =
    if c < 0x80 then !(Character.isLetterOrDigit(c) || c == '_') else QuotedNonASCII.contains(c)

  private def isCommand(c: Char): Boolean = "lLuUEQF".indexOf(c.toInt) >= 0

  private def isLUF(c: Char): Boolean = c == 'L' || c == 'U' || c == 'F'

  private def commandKind(c: Char): CaseCommandKind = c match
    case 'Q' => CaseCommandKind.QuoteCommand
    case 'U' => CaseCommandKind.UpperCommand
    case 'L' => CaseCommandKind.LowerCommand
    case 'F' => CaseCommandKind.FoldCommand
    case 'u' => CaseCommandKind.UpperFirstCommand
    case 'l' => CaseCommandKind.LowerFirstCommand
    // A frame has one of the commands above.
    // $COVERAGE-OFF$
    case _ => throw new IllegalStateException(s"Unknown command: $c")
    // $COVERAGE-ON$

private final class PerlLiteralRewriter(source: String, extended: Boolean, bytes: Boolean):

  import PerlLiteralRewriter.*

  /** The source, where the lexer swaps `\L\u` and `\U\l` in place. */
  private val src: Array[Char] = source.toCharArray

  private val n: Int = src.length

  private var pos: Int = 0

  private var changed: Boolean = false

  /** Frame is an open command at `start` (or the whole text), its text so far, and the commands closed in it. */
  private final class Frame(val command: Char, val start: Int):

    val text: java.lang.StringBuilder = new java.lang.StringBuilder
    val starts: mutable.ArrayBuilder.ofInt = new mutable.ArrayBuilder.ofInt
    val ends: mutable.ArrayBuilder.ofInt = new mutable.ArrayBuilder.ofInt
    val inner: mutable.Builder[PerlCaseCommand, Seq[PerlCaseCommand]] = Seq.newBuilder

    /** Whether the command has any argument, without which Perl's parser reports a syntax error on its closing. */
    var hasContent: Boolean = false

    def add(unit: Char, start: Int, end: Int): Unit =
      text.append(unit)
      starts.addOne(start)
      ends.addOne(end)

    def addCodePoints(cs: UString, start: Int, end: Int): Unit =
      for c <- cs.iterator; u <- UTF16Encoding.encode(c) do add(u.toChar, start, end)

  private val root: Frame = new Frame('\u0000', -1)

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
    else Some(new Rewritten(root.text.toString, root.starts.result(), root.ends.result(), source, root.inner.result()))

  /** Processes a command at `\`, or the end, and returns whether the next is a command again. */
  private def caseModifier(): Boolean =
    changed = true
    if pos >= n || src(pos + 1) == 'E' then
      if stack.nonEmpty then
        // The `\E` closes `\L`, `\U`, `\Q` or `\F`, or `\u` or `\l` without a command around it.
        val ownsEnd = pos < n && ("LUQF".indexOf(stack.head.command.toInt) >= 0 || stack.tail.isEmpty)
        val command = pop(if ownsEnd then pos + 2 else pos)
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
        pop(pos)
        true
      else
        top.hasContent = true
        stack = new Frame(command, pos) :: stack
        pos = s + 1
        false

  /** Closes the innermost command here, which ends at `closeEnd` after its own `\E` or here without one, and adds its
    * result to the outer one.
    */
  private def pop(closeEnd: Int): Char =
    val frame = stack.head
    stack = stack.tail
    val text = frame.text.toString
    val starts = frame.starts.result()
    val ends = frame.ends.result()
    val out = top
    out.inner.addAll(closedCommands(frame, closeEnd, ends))
    var i = 0
    while i < text.length do
      val c = text.codePointAt(i)
      val len = Character.charCount(c)
      val (start, end) = (starts(i), ends(i + len - 1))
      frame.command match
        case 'Q' =>
          if isQuoted(c) then out.add('\\', start, end)
          out.addCodePoints(UString(c), start, end)
        case _ if bytes && c >= 0x80 => out.addCodePoints(UString(c), start, end)
        case 'U'                     => out.addCodePoints(FullCaseMapping.upper(c), start, end)
        case 'L'                     => out.addCodePoints(FullCaseMapping.lower(c), start, end)
        case 'F'                     => out.addCodePoints(FullCaseFolding(c), start, end)
        case 'u' => out.addCodePoints(if i == 0 then FullCaseMapping.title(c) else UString(c), start, end)
        case 'l' => out.addCodePoints(if i == 0 then FullCaseMapping.lower(c) else UString(c), start, end)
        // A frame has one of the commands above.
        // $COVERAGE-OFF$
        case _ => throw new IllegalStateException(s"Unknown command: ${frame.command}")
      // $COVERAGE-ON$
      i += len
    frame.command

  /** Returns the command of the frame closed here, and the commands in it to be in the outer one.
    *
    * `\u` and `\l` change only the next character, so without their own `\E` their range is the first character that
    * they make, unless a command in them crosses its end, and the commands after it are in the outer one.
    */
  private def closedCommands(frame: Frame, closeEnd: Int, ends: Array[Int]): Seq[PerlCaseCommand] =
    val kind = commandKind(source.charAt(frame.start + 1))
    val inner = frame.inner.result()
    val nextEnd = ends.headOption.filter(_ => "ul".indexOf(frame.command.toInt) >= 0 && closeEnd == pos)
    nextEnd.filter(end => inner.forall(command => command.end <= end || end <= command.start)) match
      case Some(end) =>
        val (within, after) = inner.partition(_.end <= end)
        PerlCaseCommand(kind, frame.start, frame.start + 2, end, end, within, nextCharacter = true) +: after
      case None =>
        Seq(PerlCaseCommand(kind, frame.start, frame.start + 2, pos, closeEnd, inner, nextCharacter = false))

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
    if pos + 1 < n && UTF16Encoding.isHighSurrogate(src(pos)) && UTF16Encoding.isLowSurrogate(src(pos + 1)) then
      frame.add(src(pos), pos, pos + 2)
      frame.add(src(pos + 1), pos, pos + 2)
      pos += 2
    else copyUnit(frame)
