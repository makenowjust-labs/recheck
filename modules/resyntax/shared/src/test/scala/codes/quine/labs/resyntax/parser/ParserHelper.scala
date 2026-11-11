package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.CasePart
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.Dialect.*
import codes.quine.labs.resyntax.dialect.PerlSource
import codes.quine.labs.resyntax.printer.Printer

trait ParserHelper:
  self: munit.FunSuite =>

  /** Checks the node of a pattern, and that the printed node is the pattern. */
  def check(s: String, flags: String, dialects: Dialect*)(expected: NodeData)(using loc: munit.Location): Unit =
    checkPrinted(s, s, flags, dialects*)(expected)

  /** Checks the node of a pattern, and that the printed node is `printed`, without the text that it does not keep. */
  def checkPrinted(s: String, printed: String, flags: String, dialects: Dialect*)(expected: NodeData)(using
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"Parser.parse: /$s/$flags in $dialect"):
        val flagSet = FlagSet.parse(flags, dialect)
        val mode = Parser.determineUnitMode(s, flagSet, dialect)
        val result = Parser.parse(s, flagSet, dialect, mode)
        assert(result.equalsWithoutLoc(Node(expected)), result)
        assertPrinted(result, printed, flagSet, dialect, mode)

  def error(s: String, flags: String, dialects: Dialect*)(message: String)(using loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"Parser.parse: /$s/$flags in $dialect with error \"$message\""):
        val flagSet = FlagSet.parse(flags, dialect)
        interceptMessage[ParsingException](message):
          Parser.parse(s, flagSet, dialect, Parser.determineUnitMode(s, flagSet, dialect))

  /** Checks a Perl pattern from a runtime string, which Perl's lexer does not rewrite. */
  def checkPerlString(s: String, flags: String)(expected: NodeData)(using loc: munit.Location): Unit =
    test(s"Parser.parse: /$s/$flags in Perl from a string"):
      val flagSet = FlagSet.parse(flags, PerlString)
      val mode = Parser.determineUnitMode(s, flagSet, PerlString)
      val result = Parser.parse(s, flagSet, PerlString, mode)
      assert(result.equalsWithoutLoc(Node(expected)), result)
      assertPrinted(result, s, flagSet, PerlString, mode)

  def errorPerlString(s: String, flags: String)(message: String)(using loc: munit.Location): Unit =
    test(s"Parser.parse: /$s/$flags in Perl from a string with error \"$message\""):
      interceptMessage[ParsingException](message):
        parsePerlString(s, flags)

  /** Asserts that the printed node is the text, and that the parser reads it as the same node. */
  def assertPrinted(node: Node, text: String, flagSet: FlagSet, dialect: Dialect, mode: UnitMode)(using
      munit.Location
  ): Unit =
    val printed = Printer.print(node, flagSet, dialect, mode)
    assertEquals(printed, text)
    assert(Parser.parse(printed, flagSet, dialect, mode).equalsWithoutLoc(node), printed)

  /** Returns a part of a case region, made from the text. */
  def part(data: NodeData, written: String): CasePart[Node] = CasePart(Node(data), Some(written))

  /** Returns a part of a case region in a class, made from the text. */
  def classPart(data: ClassItemData, written: String): CasePart[ClassItem] = CasePart(ClassItem(data), Some(written))

  val PerlString: Dialect = Perl(source = PerlSource.String)

  def parsePerlString(s: String, flags: String): Node =
    val flagSet = FlagSet.parse(flags, PerlString)
    Parser.parse(s, flagSet, PerlString, Parser.determineUnitMode(s, flagSet, PerlString))

  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl(), Python(), Ruby())
