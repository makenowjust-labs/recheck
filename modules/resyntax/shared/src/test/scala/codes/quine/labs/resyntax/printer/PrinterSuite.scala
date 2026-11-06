package codes.quine.labs.resyntax.printer

import codes.quine.labs.resyntax.ast.AssertKind.*
import codes.quine.labs.resyntax.ast.AssertNameStyle.*
import codes.quine.labs.resyntax.ast.BackReferenceStyle.*
import codes.quine.labs.resyntax.ast.BackslashKind.*
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
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

  test("Printer.print: the byte 0x85 that the extended mode skips completes a character"):
    assertEquals(print(Repeat(Literal(0xc3), Star(Greedy)), "x", PCRE), "\u00c5*")
    val subscript = Sequence(Literal(0xe2), Repeat(Literal(0x82), Star(Greedy)))
    assertEquals(print(subscript, "x", Perl(utf8 = false)), "\u2085*")

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

  // The extended mode keeps its blanks, but not the ones in a construct or before a quantifier.
  check(" a # b\n\tc", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(" a # b\n\tc")
  check("( ?:a ) *", "x", Java)("(?:a )*")
  check("(?#c\n:a)", "x", Java)("(? :a)")

  // A union of literals is written in `\Q...\E` if it has a character that the class reads otherwise.
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

  // Ruby's `\u{...}` with several code points is a union in a class.
  check("[a\\u{41 42}]", "", Ruby())("[a\\u{41 42}]")
  // Ruby's control escapes are written with `\xHH`, which keeps the value.
  check("\\C-?\\c\\M-a\\M-\\C-a", "", Ruby())("\\C-\\?\\c\\x81\\M-\\x01")
  // The octal escapes are written in three digits.
  check("[\\1]\\0", "", PCRE)("[\\001]\\000")
  check("\\01", "", Java)("\\0001")
  // A Perl regex literal is printed as Perl's lexer rewrites it.
  check("\\Uab\\E\\Q.\\E", "", Perl())("AB\\.")
