package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.BackslashKind.*
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.ClassItemData.*
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.EscapeClassKind
import codes.quine.labs.resyntax.ast.EscapeStyle
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.Quantifier.*
import codes.quine.labs.resyntax.ast.SourceLocation

/** Tests of the source encodings, where an engine reads a pattern by bytes. */
class ParserEncodingSuite extends munit.FunSuite:

  def parse(s: String, flags: String, dialect: Dialect): Node =
    Parser.parse(s, FlagSet.parse(flags, dialect), dialect)

  def check(s: String, flags: String, dialect: Dialect)(expected: NodeData)(implicit loc: munit.Location): Unit =
    test(s"Parser.parse: /$s/$flags in $dialect"):
      val result = parse(s, flags, dialect)
      assert(result.equalsWithoutLoc(Node(expected)), result)

  def error(s: String, flags: String, dialect: Dialect)(message: String)(implicit loc: munit.Location): Unit =
    test(s"Parser.parse: /$s/$flags in $dialect with error \"$message\""):
      interceptMessage[ParsingException](message)(parse(s, flags, dialect))

  def bytes(s: String): Seq[NodeData] = s.getBytes("UTF-8").toSeq.map(b => Literal(b & 0xff))

  // Python's bytes pattern has no Unicode escape, and its characters are bytes of Latin-1.
  val PythonBytes: Dialect = Python(Python.Type.Bytes)
  error("\\u0041", "", PythonBytes)("Invalid backslash escape at 1")
  error("[\\u0041]", "", PythonBytes)("Invalid backslash escape at 2")
  error("\\U00000041", "", PythonBytes)("Invalid backslash escape at 1")
  error("\\N{DIGIT ONE}", "", PythonBytes)("Invalid backslash escape at 1")
  error("a\u0101", "", PythonBytes)("Non-Latin-1 character in bytes pattern at 1")
  check("\u00e9+", "", PythonBytes)(Repeat(Literal(0xe9), Plus(Greedy)))
  check("\\x41", "", PythonBytes)(Backslash(Escape(EscapeStyle.Hex2, 0x41)))
  check("\\u0041", "", Python())(Backslash(Escape(EscapeStyle.UnicodeHex4, 0x41)))
  check("\u0101", "", Python())(Literal(0x101))

  // PCRE reads UTF-8 by `u` or `(*UTF)`, and bytes otherwise.
  check("\u00e9+", "", PCRE)(Sequence(Literal(0xc3), Repeat(Literal(0xa9), Plus(Greedy))))
  check("\u00e9+", "u", PCRE)(Repeat(Literal(0xe9), Plus(Greedy)))
  check("[\uD834\uDF06-\uD834\uDF08a-z]", "", PCRE)(
    Class(
      false,
      ClassUnion(
        ClassLiteral(0xf0),
        ClassLiteral(0x9d),
        ClassLiteral(0x8c),
        ClassRange(ClassLiteral(0x86), ClassLiteral(0xf0)),
        ClassLiteral(0x9d),
        ClassLiteral(0x8c),
        ClassLiteral(0x88),
        ClassRange(ClassLiteral('a'), ClassLiteral('z'))
      )
    )
  )
  check("[\uD834\uDF06-\uD834\uDF08a-z]", "u", PCRE)(
    Class(
      false,
      ClassUnion(
        ClassRange(ClassLiteral(0x1d306), ClassLiteral(0x1d308)),
        ClassRange(ClassLiteral('a'), ClassLiteral('z'))
      )
    )
  )
  check("(*UTF)\u00e9", "", PCRE)(Sequence(Command(CommandKind.StartOption("UTF", None)), Literal(0xe9)))
  check("(*CRLF)(*UTF8)\u00e9", "", PCRE)(
    Sequence(
      Command(CommandKind.StartOption("CRLF", None)),
      Command(CommandKind.StartOption("UTF8", None)),
      Literal(0xe9)
    )
  )
  error("\\x{100}", "", PCRE)("Code point too large at 3")
  check("(*UTF)\\x{100}", "", PCRE)(
    Sequence(Command(CommandKind.StartOption("UTF", None)), Backslash(Escape(EscapeStyle.HexBracket, 0x100)))
  )
  error("(?<\u00e9>a)", "", PCRE)("Invalid identifier at 3")
  error("\\C", "u", PCRE)("Invalid backslash escape at 1")
  check("(*UTF)\\C", "", PCRE)(
    Sequence(Command(CommandKind.StartOption("UTF", None)), Backslash(EscapeClass(EscapeClassKind.CodeUnit)))
  )
  // `(*UTF)` is an option only at the start.
  error("a(*UTF)\\x{100}", "", PCRE)("Invalid group at 3")

  test("Parser.parse: the locations of bytes point to their characters"):
    val node = parse("a\u00e9+", "", PCRE)
    val Sequence(Seq(a, c3, repeat)) = node.data: @unchecked
    assertEquals(a.loc, SourceLocation(0, 1))
    assertEquals(c3.loc, SourceLocation(1, 2))
    assertEquals(repeat.loc, SourceLocation(1, 3))
    interceptMessage[ParsingException]("Code point too large at 5")(parse("\u00e9\u00e9\\x{100}", "", PCRE))

  // Perl's byte strings are UTF-8 bytes.
  val PerlBytes: Dialect = Perl(utf8 = false)
  check("\u00e9+", "", PerlBytes)(Sequence(Literal(0xc3), Repeat(Literal(0xa9), Plus(Greedy))))
  check("\u00e9+", "", Perl())(Repeat(Literal(0xe9), Plus(Greedy)))
  check("[\u00e9-\u00e0]", "", PerlBytes)(
    Class(false, ClassUnion(ClassLiteral(0xc3), ClassRange(ClassLiteral(0xa9), ClassLiteral(0xc3)), ClassLiteral(0xa0)))
  )
  error("[\u00e9-\u00e0]", "", Perl())("Out of order class range at 1")
  // The case commands do not change non-ASCII bytes.
  check("\\U\u00e9a", "", PerlBytes)(Sequence((bytes("\u00e9") :+ Literal('A'))*))

  // Ruby's ASCII-8BIT regexp reads the UTF-8 bytes of the source (as `Regexp.new(source.b, Regexp::NOENCODING)`).
  val RubyBytes: Dialect = Ruby(Ruby.Encoding.Ascii8Bit)
  for (flags, dialect) <- Seq(("n", Ruby()), ("", RubyBytes)) do
    check("\u00e9+", flags, dialect)(Sequence(Literal(0xc3), Repeat(Literal(0xa9), Plus(Greedy))))
    check("[\u00e9-\u00e0]", flags, dialect)(
      Class(
        false,
        ClassUnion(ClassLiteral(0xc3), ClassRange(ClassLiteral(0xa9), ClassLiteral(0xc3)), ClassLiteral(0xa0))
      )
    )
    error("\\u00e9", flags, dialect)("Incompatible character encoding at 1")
    error("\\u{41 e9}", flags, dialect)("Incompatible character encoding at 6")
    check("\\u0041", flags, dialect)(Backslash(Escape(EscapeStyle.UnicodeHex4, 0x41)))
    check("\\xff", flags, dialect)(Backslash(Escape(EscapeStyle.Hex2, 0xff)))
  check("a\u00e9", "", Ruby())(Sequence(Literal('a'), Literal(0xe9)))
  check("\\u00e9", "", Ruby())(Backslash(Escape(EscapeStyle.UnicodeHex4, 0xe9)))
