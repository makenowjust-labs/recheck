package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect._
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.NameStyle
import codes.quine.labs.resyntax.ast.NodeData
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
  check("(?<-x>)", "", DotNet)(
    Unsupported(NodeData.Group(GroupKind.Balance(NameStyle.Angle, None, "x"), NodeData.Sequence()))
  )
  check("(?>)", "", DotNet, Java, PCRE, Perl, Ruby)(
    Unsupported(NodeData.Group(GroupKind.Atomic(AssertNameStyle.Symbolic), NodeData.Sequence()))
  )
  check("(?*)", "", PCRE)(
    Unsupported(NodeData.Group(GroupKind.NonAtomicPositiveLookAhead(AssertNameStyle.Symbolic), NodeData.Sequence()))
  )
  check("(?<*)", "", PCRE)(
    Unsupported(NodeData.Group(GroupKind.NonAtomicPositiveLookBehind(AssertNameStyle.Symbolic), NodeData.Sequence()))
  )
  check("(*script_run:)", "", PCRE, Perl)(
    Unsupported(NodeData.Group(GroupKind.ScriptRun(AssertNameStyle.Alphabetic), NodeData.Sequence()))
  )
  check("(*atomic_script_run:)", "", PCRE, Perl)(
    Unsupported(NodeData.Group(GroupKind.AtomicScriptRun(AssertNameStyle.Alphabetic), NodeData.Sequence()))
  )
  check("(?m:^)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(Assert(IRAssertKind.LineBegin))
  check("(?-m:^)", "m", DotNet, Java, PCRE, Perl, Python)(Assert(IRAssertKind.TextBegin))
  check("(?-m:^)", "m", Ruby)(Assert(IRAssertKind.LineBegin))
  check("(?^m:^)", "", PCRE, Perl)(Assert(IRAssertKind.LineBegin))
  check("(?^:^)", "m", PCRE, Perl)(Assert(IRAssertKind.TextBegin))
  check("(?~)", "", Ruby)(Unsupported(NodeData.Group(GroupKind.Absence, NodeData.Sequence())))

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
