package codes.quine.labs.resyntax.printer

import codes.quine.labs.resyntax.ast.AssertKind.*
import codes.quine.labs.resyntax.ast.AssertNameStyle.*
import codes.quine.labs.resyntax.ast.BackReferenceStyle.*
import codes.quine.labs.resyntax.ast.BackslashKind.*
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.CaseCommandKind.*
import codes.quine.labs.resyntax.ast.CaseForm
import codes.quine.labs.resyntax.ast.CasePart
import codes.quine.labs.resyntax.ast.CaseRegionEnd.*
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData.*
import codes.quine.labs.resyntax.ast.CommandKind.*
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.EscapeClassKind.*
import codes.quine.labs.resyntax.ast.EscapeStyle.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.NameStyle
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.Quantifier.*
import codes.quine.labs.resyntax.ast.Reference.*
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.Dialect.*
import codes.quine.labs.resyntax.parser.Parser

class PrinterSuite extends munit.FunSuite:

  def print(data: NodeData, flags: String, dialect: Dialect): String =
    val flagSet = FlagSet.parse(flags, dialect)
    Printer.print(Node(data), flagSet, dialect, Parser.determineUnitMode("", flagSet, dialect))

  def check(s: String, flags: String, dialects: Dialect*)(expected: String)(using loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"Printer.print: /$s/$flags in $dialect"):
        val flagSet = FlagSet.parse(flags, dialect)
        val mode = Parser.determineUnitMode(s, flagSet, dialect)
        val node = Parser.parse(s, flagSet, dialect, mode)
        val printed = Printer.print(node, flagSet, dialect, mode)
        assertEquals(printed, expected)
        assert(Parser.parse(printed, flagSet, dialect, mode).equalsWithoutLoc(node), printed)

  /** Asserts that the node is printed as the text, which the parser reads as the node. */
  def checkPlain(data: NodeData, flags: String, dialect: Dialect)(expected: String)(using munit.Location): Unit =
    val flagSet = FlagSet.parse(flags, dialect)
    val mode = Parser.determineUnitMode(expected, flagSet, dialect)
    val printed = Printer.print(Node(data), flagSet, dialect, mode)
    assertEquals(printed, expected)
    val reparsed = Parser.parse(printed, flagSet, dialect, mode)
    assert(reparsed.equalsWithoutLoc(Node(data)), reparsed)

  def part(data: NodeData, written: String): CasePart[Node] = CasePart(Node(data), Some(written))

  val ref1: NodeData = Backslash(EscapeBackReference(BareBackReference, IndexedReference(1)))

  val verbose: NodeData = Command(InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)))

  test("Printer.print: a space keeps the tokens apart in the extended mode"):
    assertEquals(print(Sequence(ref1, Literal('1')), "x", PCRE), "\\1 1")
    assertEquals(print(Sequence(ref1, Literal('1')), "", PCRE), "\\11")
    val relative = Backslash(EscapeBackReference(GBackReference(NameStyle.Bare), RelativeReference(-1)))
    assertEquals(print(Sequence(relative, Literal('2')), "x", PCRE), "\\g-1 2")
    assertEquals(print(Sequence(Backslash(Escape(Hex1, 4)), Literal('1')), "x", PCRE), "\\x4 1")
    assertEquals(print(Repeat(Backslash(Escape(Hex0, 0)), Exact(2, Greedy)), "x", Perl()), "\\x {2}")
    assertEquals(print(Sequence(Backslash(Unknown('k')), Literal('<')), "x", Ruby()), "\\k <")
    assertEquals(print(Sequence(Backslash(EscapeClass(NonNewline)), Literal('{')), "x", PCRE), "\\N {")
    assertEquals(print(Sequence(Backslash(Assert(Boundary(None))), Literal('{')), "x", Perl()), "\\b {")
    assertEquals(print(Sequence(Backslash(Assert(NonBoundary(None))), Literal('{')), "x", Perl()), "\\B {")
    assertEquals(print(Repeat(Repeat(Literal('a'), Plus(Greedy)), Question(Greedy)), "x", Ruby()), "a+ ?")
    assertEquals(print(Repeat(Repeat(Literal('a'), Plus(Greedy)), Question(Greedy)), "", Ruby()), "a+?")
    val lazyPlus = Repeat(Node(Repeat(Literal('a'), Plus(Greedy))), Question(Greedy), Some(" ?"))
    assertEquals(print(lazyPlus, "x", Ruby()), "a+ ?")
    // Ruby reads `?` after `{2}`, and `+` after `{2}` and `{2,3}`, as another repeat.
    assertEquals(print(Repeat(Repeat(Literal('a'), Exact(2, Greedy)), Question(Greedy)), "x", Ruby()), "a{2}?")
    assertEquals(print(Repeat(Repeat(Literal('a'), Exact(2, Greedy)), Plus(Greedy)), "x", Ruby()), "a{2}+")
    assertEquals(print(Repeat(Repeat(Literal('a'), Bounded(2, 3, Greedy)), Plus(Greedy)), "x", Ruby()), "a{2,3}+")
    assertEquals(print(Repeat(Repeat(Literal('a'), Bounded(2, 3, Greedy)), Question(Greedy)), "x", Ruby()), "a{2,3} ?")
    assertEquals(print(Repeat(Repeat(Literal('a'), Plus(Lazy)), Question(Greedy)), "x", Ruby()), "a+??")
    assertEquals(print(Sequence(Literal(0xd83d), Literal(0xde00)), "x", Perl()), "\ud83d \ude00")
    assertEquals(print(Sequence(Literal(0xd83d), Literal(0xde00)), "x", DotNet), "\ud83d\ude00")

  test("Printer.print: a space keeps the tokens apart in a class where the extended mode skips it"):
    val klass = Class(false, ClassUnion(ClassBackslashValue(Escape(Hex1, 4)), ClassLiteral('1')))
    val verboseMore = Command(InlineFlag(FlagSetDiff(FlagSet(verbose = true, verboseMore = true), None)))
    assertEquals(print(Sequence(verboseMore, klass), "", PCRE), "(?xx)[\\x4 1]")
    assertEquals(print(Sequence(verbose, klass), "", PCRE), "(?x)[\\x41]")

  test("Printer.print: the inline flags change the extended mode where the parser reads them"):
    val sequence = Sequence(ref1, Literal('1'))
    val verboseA = Sequence(verbose, Literal('a'))
    val off = Command(InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(verbose = true)))))
    assertEquals(print(Sequence(verbose, sequence, off, sequence), "", PCRE), "(?x)\\1 1(?-x)\\11")
    val group = Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)), sequence)
    assertEquals(print(Sequence(group, sequence), "", PCRE), "(?x:\\1 1)\\11")
    assertEquals(print(Sequence(Command(ResetFlag(FlagSet(verbose = true))), sequence), "", PCRE), "(?^x)\\1 1")
    val reset = Group(GroupKind.ResetFlag(FlagSet()), sequence)
    assertEquals(print(Sequence(reset, sequence), "x", PCRE), "(?^:\\11)\\1 1")
    assertEquals(print(Disjunction(verboseA, sequence), "", PCRE), "(?x)a|\\1 1")
    assertEquals(print(Sequence(Command(BranchReset(verboseA, sequence)), sequence), "", PCRE), "(?|(?x)a|\\1 1)\\11")
    val conditional = Conditional(ConditionalTest.Indexed(1), Node(Sequence(verbose, sequence)), Some(Node(sequence)))
    assertEquals(print(Command(conditional), "", PCRE), "(?(1)(?x)\\1 1|\\11)")
    val lookAround = ConditionalTest.LookAround(GroupKind.PositiveLookAhead(Symbolic), verboseA)
    assertEquals(print(Command(Conditional(lookAround, sequence)), "", PCRE), "(?(?=(?x)a)\\11)")
    assertEquals(print(Command(AbsenceExpression(verboseA, sequence)), "", Ruby()), "(?~|(?x)a|\\11)")
    assertEquals(print(Sequence(Command(AbsenceStopper(verboseA)), sequence), "", Ruby()), "(?~|(?x)a)\\11")

  test("Printer.print: a node without written texts is written in the plain text"):
    checkPlain(Backslash(Escape(Hex2, 0x7f)), "", PCRE)("\\x7F")
    checkPlain(Backslash(Escape(UnicodeHex4, 0x0d)), "", Java)("\\u000D")
    checkPlain(Backslash(Escape(UnicodeHex4Pair, 0x1f600)), "", Java)("\\uD83D\\uDE00")
    checkPlain(Backslash(Escape(UnicodeHex8, 0x1f600)), "", Python())("\\U0001F600")
    checkPlain(Backslash(Escape(UnicodeBracket, 0x41)), "", Ruby())("\\u{41}")
    checkPlain(Backslash(Escape(HexBracket, 0x41)), "", PCRE)("\\x{41}")
    checkPlain(Backslash(Escape(Octal, 0x41)), "", PCRE)("\\o{101}")
    checkPlain(Backslash(Escape(NamedHex, 0x41)), "", Perl())("\\N{U+41}")
    checkPlain(Backslash(Escape(Control('A'), 0x01)), "", PCRE)("\\cA")
    checkPlain(Sequence(Backslash(Escape(BareOctal, 1)), Literal('1')), "", PCRE)("\\0011")
    checkPlain(Sequence(Backslash(Escape(BareOctal, 1)), Literal('1')), "", Java)("\\00011")
    val ruby = Sequence(
      Backslash(Escape(ControlDash, 0x7f)),
      Backslash(Escape(Control('\\'), 0x81)),
      Backslash(Escape(Meta, 0xe1)),
      Backslash(Escape(Meta, 0x81))
    )
    checkPlain(ruby, "", Ruby())("\\C-\\?\\c\\x81\\M-\\x61\\M-\\x01")
    checkPlain(Repeat(Literal('a'), Bounded(1, 3, Lazy)), "", PCRE)("a{1,3}?")
    checkPlain(Repeat(Literal('a'), Unbounded(2, Possessive)), "", PCRE)("a{2,}+")
    val diff = FlagSetDiff(FlagSet(ignoreCase = true, multiline = true), Some(FlagSet(verbose = true)))
    checkPlain(Command(InlineFlag(diff)), "", PCRE)("(?im-x)")
    checkPlain(Command(ResetFlag(FlagSet(ascii = true, asciiRestrict = true))), "", Perl())("(?^aa)")
    checkPlain(Group(GroupKind.ResetFlag(FlagSet(ignoreCase = true)), Literal('a')), "", PCRE)("(?^i:a)")
    // .NET rejects `(?)`, and Java reads `(?:` as a non-capture group.
    checkPlain(Command(InlineFlag(FlagSetDiff(FlagSet(), None))), "", DotNet)("(?+)")
    checkPlain(Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), None)), Literal('a')), "x", Java)("(? :a)")

  test("Printer.print: bytes that are not UTF-8 throw"):
    interceptMessage[IllegalArgumentException]("Invalid UTF-8 bytes at 0")(print(Literal(0xc3), "", PCRE))
    interceptMessage[IllegalArgumentException]("Invalid UTF-8 bytes at 0")(print(Literal(0xa9), "", PCRE))

  test("Printer.print: a group in a style that it does not have throws"):
    interceptMessage[IllegalArgumentException]("No Symbolic name of the group: script_run"):
      print(Group(GroupKind.ScriptRun(Symbolic), Literal('a')), "", PCRE)
    interceptMessage[IllegalArgumentException]("No Symbolic name of the group: atomic_script_run"):
      print(Group(GroupKind.AtomicScriptRun(Symbolic), Literal('a')), "", PCRE)
    interceptMessage[IllegalArgumentException]("No Symbolic name of the group: scan_substring"):
      print(Group(GroupKind.ScanSubstring(Symbolic, Seq(IndexedReference(1))), Literal('a')), "", PCRE)
    interceptMessage[IllegalArgumentException]("No Abbrev name of the group: atomic"):
      print(Group(GroupKind.Atomic(Abbrev), Literal('a')), "", PCRE)

  test("Printer.print: a union in a union is written as its items without a construct making one"):
    val klass = Class(false, ClassUnion(ClassLiteral('a'), ClassUnion(ClassLiteral('b'), ClassLiteral('c'))))
    assertEquals(print(klass, "", Ruby()), "[abc]")

  test("Printer.print: Ruby's `\\u{...}` of several code points is written with the quantifiers after it"):
    val escapes = Seq(0x41, 0x42).map(value => Escape(UnicodeBracket, value))
    val nodes = escapes.map(escape => Node(Backslash(escape)))
    checkPlain(UnicodeList(nodes.init :+ Node(Repeat(nodes.last, Star(Lazy)))), "", Ruby())("\\u{41 42}*?")
    val items = escapes.map(escape => ClassItem.Value(ClassBackslashValue(escape)))
    checkPlain(Class(false, ClassUnicodeList(items)), "", Ruby())("[\\u{41 42}]")
    val message = "A Unicode list has a node other than an escape: Node(Dot,SourceLocation(-1,-1))"
    interceptMessage[IllegalArgumentException](message)(print(UnicodeList(Seq(Node(Dot), nodes.last)), "", Ruby()))

  // The extended mode keeps its blanks, but not the ones in a construct.
  check(" a # b\n\tc", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(" a # b\n\tc")
  check("( ?:a ) *", "x", Java)("(?:a ) *")

  test("Printer.print: a part of a case region without a text is written out of the commands"):
    val digit = CasePart(Node(Backslash(EscapeClass(Digit))), None)
    val quote = CaseRegion(QuoteCommand, Seq(part(Literal('a'), "a"), digit, part(Literal('b'), "b")), Closed)
    assertEquals(print(quote, "", Java), "\\Qa\\E\\d\\Qb\\E")
    val dot = part(Backslash(Escape(Single('.'), '.')), ".")
    val inner = CasePart(Node(CaseRegion(QuoteCommand, Seq(dot, digit, part(Literal('B'), "b")), Closed)), None)
    assertEquals(print(CaseRegion(UpperCommand, Seq(inner), Closed), "", Perl()), "\\U\\Q.\\E\\E\\d\\U\\Qb\\E\\E")
    assertEquals(print(CaseRegion(QuoteCommand, Seq(digit), Unclosed), "", Java), "\\d")
    val star = CasePart(Node(Repeat(Literal('b'), Star(Greedy))), None)
    val repeated = CaseRegion(QuoteCommand, Seq(part(Literal('a'), "a"), star), ClosedBeforeQuantifier)
    assertEquals(print(repeated, "", Java), "\\Qa\\Eb*")
    val classDigit = CasePart(ClassItem(ClassBackslashClass(Digit)), None)
    val classA = CasePart(ClassItem(ClassLiteral('a')), Some("a"))
    val classQuote = ClassCaseRegion(QuoteCommand, Seq(classA, classDigit), Closed)
    assertEquals(print(Class(false, classQuote), "", Java), "[\\Qa\\E\\d]")
    val classFirst = ClassCaseRegion(UpperFirstCommand, Seq(classA, classDigit), NextCharacter)
    assertEquals(print(Class(false, classFirst), "", Perl()), "[\\ua\\E\\d]")

  test("Printer.print: the inline flags in a case region change the extended mode"):
    val verbose = Command(InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)))
    val reset = Command(ResetFlag(FlagSet(verbose = true)))
    for (flag, text) <- Seq((verbose, "(?X)"), (reset, "(?^X)")) do
      val region = CaseRegion(LowerCommand, Seq(part(flag, text)), Closed)
      assertEquals(print(Sequence(region, ref1, Literal('1')), "", Perl()), s"\\L$text\\E\\1 1")

  test("Printer.print: a case region that its commands cannot make throws"):
    interceptMessage[IllegalArgumentException]("A case region of \\E"):
      print(CaseRegion(EndCaseCommand, Seq(), Closed), "", PCRE)
    interceptMessage[IllegalArgumentException]("A case region without \\E is followed by a node"):
      print(Sequence(CaseRegion(QuoteCommand, Seq(part(Literal('a'), "a")), Unclosed), Literal('b')), "", Java)
    interceptMessage[IllegalArgumentException]("A case region closed before a quantifier has no repeat"):
      print(CaseRegion(QuoteCommand, Seq(part(Literal('a'), "a")), ClosedBeforeQuantifier), "", Java)
    interceptMessage[IllegalArgumentException]("A case region of UpperFirstCommand is opened again"):
      val parts = Seq(CasePart(Node(Literal('x')), None), part(Literal('A'), "a"))
      print(CaseRegion(UpperFirstCommand, parts, NextCharacter), "", Perl())
    interceptMessage[IllegalArgumentException]("A case region in a class ends with \\E or the next character"):
      val parts = Seq(CasePart(ClassItem(ClassLiteral('a')), Some("a")))
      print(Class(false, ClassCaseRegion(QuoteCommand, parts, Unclosed)), "", PCRE)
    interceptMessage[IllegalArgumentException]("A case form ends with \\E or the next character: Unclosed"):
      val end = ClassItem.Value(ClassLiteral('z'), SourceLocation.Invalid, Some(CaseForm(LowerCommand, "Z", Unclosed)))
      print(Class(false, ClassRange(ClassItem.Value(ClassLiteral('a')), end)), "", Perl())

  // A quote in a class is written as it is, and a class without one has none.
  check("[\\Q]\\E]", "", Java, PCRE)("[\\Q]\\E]")
  check("[\\Q\\\\E]", "", Java, PCRE)("[\\Q\\\\E]")
  check("[\\Q^a\\E]", "", Java, PCRE)("[\\Q^a\\E]")
  check("[a^]", "", Java, PCRE)("[a^]")
  check("[\\Qa-b\\E]", "", Java, PCRE)("[\\Qa-b\\E]")
  check("[+-]", "", Java, PCRE)("[+-]")
  check("[\\Q[a\\E]", "", Java)("[\\Q[a\\E]")
  check("[[a]", "", PCRE)("[[a]")
  check("[\\Q[:\\E]", "", PCRE)("[\\Q[:\\E]")
  check("[\\Qa&&b\\E]", "", Java)("[\\Qa&&b\\E]")
  check("[a&&b]", "", PCRE)("[a&&b]")
  check("[\\Qa b\\E]", "x", Java)("[\\Qa b\\E]")
  check("[a b]", "", Java)("[a b]")

  // Ruby's `\u{...}` with several code points is written as it is.
  check("[a\\u{ 41 42}]", "", Ruby())("[a\\u{ 41 42}]")
  // The commands of a Perl regex literal are written as they are, except the ones that are not case regions.
  check("\\Uab\\E\\Q.\\E", "", Perl())("\\Uab\\E\\Q.\\E")
  check("x\\Ua|b\\Ey", "", Perl())("xA|By")
