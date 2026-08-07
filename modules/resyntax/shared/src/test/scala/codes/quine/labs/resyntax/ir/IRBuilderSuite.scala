package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BacktrackControlKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.NameStyle
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ir.IRNodeData.*
import codes.quine.labs.resyntax.ir.IRQuantifier.*
import codes.quine.labs.resyntax.parser.Parser

class IRBuilderSuite extends munit.FunSuite:
  def check(s: String, flags: String, dialects: Dialect*)(expected: IRNodeData)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        val flagSet = FlagSet.parse(flags, dialect)
        val node = Parser.parse(s, flagSet, dialect)
        val result = IRBuilder.build(node, flagSet, dialect)
        assert(result.equalsWithoutLoc(IRNode(expected)))

  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)

  // Disjunction
  check("|", "", All*)(Disjunction(Empty, Empty))

  // Sequence
  check("", "", All*)(Empty)
  check("(?:)(?:)", "", All*)(Sequence(Empty, Empty))

  // Repeat
  check("(?:)*", "", All*)(Repeat(Empty, Unbounded(0, Greedy)))
  check("(?:)*?", "", All*)(Repeat(Empty, Unbounded(0, Lazy)))
  check("(?:)*+", "", Java, PCRE, Perl, Ruby)(Repeat(Empty, Unbounded(0, Possessive)))
  check("(?:)+", "", All*)(Repeat(Empty, Unbounded(1, Greedy)))
  check("(?:)+?", "", All*)(Repeat(Empty, Unbounded(1, Lazy)))
  check("(?:)++", "", Java, PCRE, Perl, Ruby)(Repeat(Empty, Unbounded(1, Possessive)))
  check("(?:)?", "", All*)(Repeat(Empty, Bounded(0, 1, Greedy)))
  check("(?:)??", "", All*)(Repeat(Empty, Bounded(0, 1, Lazy)))
  check("(?:)?+", "", Java, PCRE, Perl, Ruby)(Repeat(Empty, Bounded(0, 1, Possessive)))
  check("(?:){4}", "", All*)(Repeat(Empty, Exact(4)))
  check("(?:){4}?", "", DotNet, Java, JavaScript, PCRE, Perl, Python)(Repeat(Empty, Exact(4)))
  check("(?:){4}?", "", Ruby)(Repeat(Repeat(Empty, Exact(4)), Bounded(0, 1, Greedy)))
  check("(?:){4}+", "", Java, PCRE, Perl)(Repeat(Empty, Exact(4)))
  check("(?:){4}+", "", Ruby)(Repeat(Repeat(Empty, Exact(4)), Unbounded(1, Greedy)))
  check("(?:){2,3}", "", All*)(Repeat(Empty, Bounded(2, 3, Greedy)))
  check("(?:){2,2}", "", All*)(Repeat(Empty, Exact(2)))
  check("(?:){2,3}?", "", All*)(Repeat(Empty, Bounded(2, 3, Lazy)))
  check("(?:){2,2}?", "", All*)(Repeat(Empty, Exact(2)))
  check("(?:){2,3}+", "", Java, PCRE, Perl, Ruby)(Repeat(Empty, Bounded(2, 3, Possessive)))
  check("(?:){2,2}+", "", Java, PCRE, Perl, Ruby)(Repeat(Empty, Exact(2)))
  check("(?:){,1}", "", Perl, Python, Ruby)(Repeat(Empty, Bounded(0, 1, Greedy)))
  check("(?:){,0}", "", Perl, Python, Ruby)(Repeat(Empty, Exact(0)))
  check("(?:){,1}?", "", Perl, Python, Ruby)(Repeat(Empty, Bounded(0, 1, Lazy)))
  check("(?:){,0}?", "", Perl, Python, Ruby)(Repeat(Empty, Exact(0)))
  check("(?:){,1}+", "", Perl, Ruby)(Repeat(Empty, Bounded(0, 1, Possessive)))
  check("(?:){,0}+", "", Perl, Ruby)(Repeat(Empty, Exact(0)))
  check("(?:){2,}", "", All*)(Repeat(Empty, Unbounded(2, Greedy)))
  check("(?:){2,}?", "", All*)(Repeat(Empty, Unbounded(2, Lazy)))
  check("(?:){2,}+", "", Java, PCRE, Perl, Ruby)(Repeat(Empty, Unbounded(2, Possessive)))

  // Command
  check("(?R)", "", PCRE, Perl)(Unsupported(NodeData.Command(CommandKind.RCall)))
  check("(?1)", "", PCRE, Perl)(Unsupported(NodeData.Command(CommandKind.IndexedCall(1))))
  check("(?-1)", "", PCRE, Perl)(Unsupported(NodeData.Command(CommandKind.RelativeCall(-1))))
  check("(?&x)", "", PCRE, Perl)(Unsupported(NodeData.Command(CommandKind.NamedCall("x"))))
  check("(?P>x)", "", PCRE, Perl)(Unsupported(NodeData.Command(CommandKind.PNamedCall("x"))))
  check("(?#)", "", DotNet, PCRE, Perl, Python, Ruby)(Empty)
  check("(?{})", "", Perl)(Unsupported(NodeData.Command(CommandKind.InlineCode(""))))
  check("(??{})", "", Perl)(Unsupported(NodeData.Command(CommandKind.EmbedCode(""))))
  check("(?C)", "", PCRE)(Unsupported(NodeData.Command(CommandKind.Callout)))
  check("(?C{x})", "", PCRE)(Unsupported(NodeData.Command(CommandKind.CalloutString('{', '}', "x"))))
  check("(?C1)", "", PCRE)(Unsupported(NodeData.Command(CommandKind.CalloutInt(1))))
  check("(?|)", "", PCRE, Perl)(Unsupported(NodeData.Command(CommandKind.BranchReset(Seq(Node(NodeData.Sequence()))))))
  check("(?(1)|)", "", DotNet, PCRE, Perl, Python, Ruby)(
    Unsupported(NodeData.Command(CommandKind.Conditional(ConditionalTest.Indexed(1), NodeData.Sequence())))
  )
  check("(*ACCEPT)", "", PCRE, Perl)(
    Unsupported(NodeData.Command(CommandKind.BacktrackControl(Some(BacktrackControlKind.Accept), None)))
  )

  // Group
  check("()", "", All*)(Capture(1, None, Empty))
  check("(?:)", "", All*)(Empty)
  check("(?<x>)", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(Capture(1, Some("x"), Empty))
  check("(?'x')", "", DotNet, PCRE, Perl, Ruby)(Capture(1, Some("x"), Empty))
  check("(?P<x>)", "", PCRE, Perl, Python)(Capture(1, Some("x"), Empty))
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
  check("^", "m", All*)(Assert(IRAssertKind.LineBegin))

  // Dollar
  check("$", "", JavaScript)(Assert(IRAssertKind.TextEnd))
  check("$", "", DotNet, Java, PCRE, Perl, Python)(Assert(IRAssertKind.ChompTextEnd))
  check("$", "", Ruby)(Assert(IRAssertKind.LineEnd))
  check("$", "m", All*)(Assert(IRAssertKind.LineEnd))
