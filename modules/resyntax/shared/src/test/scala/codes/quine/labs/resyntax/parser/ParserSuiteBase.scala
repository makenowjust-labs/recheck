package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData

abstract class ParserSuiteBase extends munit.FunSuite:
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

  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)
