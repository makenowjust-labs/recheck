package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.PerlSource

trait ParserHelper:
  self: munit.FunSuite =>

  def check(s: String, flags: String, dialects: Dialect*)(expected: NodeData)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"Parser.parse: /$s/$flags in $dialect"):
        val flagSet = FlagSet.parse(flags, dialect)
        val result = Parser.parse(s, flagSet, dialect)
        assert(result.equalsWithoutLoc(Node(expected)))

  def error(s: String, flags: String, dialects: Dialect*)(message: String)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"Parser.parse: /$s/$flags in $dialect with error \"$message\""):
        val flagSet = FlagSet.parse(flags, dialect)
        interceptMessage[ParsingException](message):
          Parser.parse(s, flagSet, dialect)

  /** Checks a Perl pattern from a runtime string, which Perl's lexer does not rewrite. */
  def checkPerlString(s: String, flags: String)(expected: NodeData)(implicit loc: munit.Location): Unit =
    test(s"Parser.parse: /$s/$flags in Perl from a string"):
      val result = Parser.parse(s, FlagSet.parse(flags, PerlString), PerlString)
      assert(result.equalsWithoutLoc(Node(expected)), result)

  def errorPerlString(s: String, flags: String)(message: String)(implicit loc: munit.Location): Unit =
    test(s"Parser.parse: /$s/$flags in Perl from a string with error \"$message\""):
      interceptMessage[ParsingException](message):
        Parser.parse(s, FlagSet.parse(flags, PerlString), PerlString)

  val PerlString: Dialect = Perl(source = PerlSource.String)

  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl(), Python(), Ruby())
