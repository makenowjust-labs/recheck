package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect._
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ir.IRNodeData._
import codes.quine.labs.resyntax.parser.Parser

class IRBuilderSuite extends munit.FunSuite {
  def check(s: String, flags: String, dialects: Dialect*)(expected: IRNodeData)(implicit loc: munit.Location): Unit = {
    for (dialect <- dialects) {
      test(s"IRBuilder.build: /$s/$flags in $dialect") {
        val flagSet = FlagSet.parse(flags, dialect)
        val node = Parser.parse(s, flagSet, dialect)
        val result = IRBuilder.build(node, flagSet, dialect)
        assert(result.equalsWithoutLoc(IRNode(expected)))
      }
    }
  }

  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)

  // Group
  check("(?m:^)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(Assert(IRAssertKind.LineBegin))
  check("(?-m:^)", "m", DotNet, Java, PCRE, Perl, Python)(Assert(IRAssertKind.TextBegin))
  check("(?-m:^)", "m", Ruby)(Assert(IRAssertKind.LineBegin))
  check("(?m:$)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(Assert(IRAssertKind.LineEnd))
  check("(?-m:$)", "m", DotNet, Java, PCRE, Perl, Python)(Assert(IRAssertKind.ChompTextEnd))
  check("(?-m:$)", "m", Ruby)(Assert(IRAssertKind.LineEnd))

  // Caret
  check("^", "", DotNet, Java, JavaScript, PCRE, Perl, Python)(Assert(IRAssertKind.TextBegin))
  check("^", "", Ruby)(Assert(IRAssertKind.LineBegin))
  check("^", "m", All: _*)(Assert(IRAssertKind.LineBegin))

  // Dollar
  check("$", "", JavaScript)(Assert(IRAssertKind.TextEnd))
  check("$", "", DotNet, Java, PCRE, Perl, Python)(Assert(IRAssertKind.ChompTextEnd))
  check("$", "", Ruby)(Assert(IRAssertKind.LineEnd))
  check("$", "m", All: _*)(Assert(IRAssertKind.LineEnd))
}
