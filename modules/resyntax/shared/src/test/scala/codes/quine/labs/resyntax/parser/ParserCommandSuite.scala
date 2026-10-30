package codes.quine.labs.resyntax.parser
import codes.quine.labs.resyntax.ast.AssertNameStyle.*
import codes.quine.labs.resyntax.ast.BacktrackControlKind.*
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.ClassItemData.*
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.CommandKind.*
import codes.quine.labs.resyntax.ast.ConditionalTest.*
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.GroupKind.*
import codes.quine.labs.resyntax.ast.NameStyle.*
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.Quantifier.*
import codes.quine.labs.resyntax.ast.Reference.*

class ParserCommandSuite extends ParserSuiteBase:
  check("(?(1)a)", "", DotNet, PCRE, Perl, Python, Ruby)(Command(Conditional(Indexed(1), Literal('a'))))
  check("(?(1)a|b)", "", DotNet, PCRE, Perl, Python, Ruby)(Command(Conditional(Indexed(1), Literal('a'), Literal('b'))))
  check("(?(<x>)a)", "", PCRE, Perl, Ruby)(Command(Conditional(Named(Angle, "x"), Literal('a'))))
  check("(?('x')a)", "", PCRE, Perl, Ruby)(Command(Conditional(Named(Quote, "x"), Literal('a'))))
  check("(?(<x>)a)", "", DotNet):
    Command(Conditional(LookAround(Sequence(Literal('<'), Literal('x'), Literal('>'))), Literal('a')))
  check("(?(<1>)a|b)", "", Ruby):
    Command(Conditional(Referenced(Angle, IndexedReference(1)), Literal('a'), Literal('b')))
  check("(?('-1')a)", "", Ruby)(Command(Conditional(Referenced(Quote, RelativeReference(-1)), Literal('a'))))
  check("(?(<x+0>)a)", "", Ruby):
    Command(Conditional(Referenced(Angle, LeveledReference(NamedReference("x"), 0)), Literal('a')))
  check("(?(<+1>)a)", "", Ruby)(Command(Conditional(Named(Angle, "+1"), Literal('a'))))
  error("(?(<1>)a)", "", PCRE, Perl)("Invalid identifier at 4")
  check("(?<n>a)(?(<1>)b)", "", Ruby):
    Sequence(
      Group(NamedCapture(Angle, "n"), Literal('a')),
      Command(Conditional(Referenced(Angle, IndexedReference(1)), Literal('b')))
    )
  check("(?(DEFINE)a)", "", PCRE, Perl)(Command(Conditional(Define, Literal('a'))))
  check("(?(VERSION>=1.2)a)", "", PCRE)(Command(Conditional(Version(lt = true, 1, 2), Literal('a'))))
  check("(?(VERSION=1.2)a)", "", PCRE)(Command(Conditional(Version(lt = false, 1, 2), Literal('a'))))
  check("(?(R)a)", "", PCRE, Perl)(Command(Conditional(RRecursion, Literal('a'))))
  check("(?(R1)a)", "", PCRE, Perl)(Command(Conditional(IndexedRecursion(1), Literal('a'))))
  check("(?(R&x)a)", "", PCRE, Perl)(Command(Conditional(NamedRecursion("x"), Literal('a'))))
  check("(?(x)a)", "", PCRE, Python)(Command(Conditional(Named(Bare, "x"), Literal('a'))))
  check("(?(+1)a)", "", PCRE)(Command(Conditional(Relative(1), Literal('a'))))
  check("(?(-1)a)", "", PCRE)(Command(Conditional(Relative(-1), Literal('a'))))
  check("(?(?=a)b)", "", DotNet, PCRE, Perl):
    Command(Conditional(LookAround(PositiveLookAhead(Symbolic), Literal('a')), Literal('b')))
  check("(?(*positive_lookahead:a)b)", "", PCRE, Perl):
    Command(Conditional(LookAround(PositiveLookAhead(Alphabetic), Literal('a')), Literal('b')))
  check("(?(*pla:a)b)", "", PCRE, Perl):
    Command(Conditional(LookAround(PositiveLookAhead(Abbrev), Literal('a')), Literal('b')))
  check("(?(?!a)b)", "", DotNet, PCRE, Perl):
    Command(Conditional(LookAround(NegativeLookAhead(Symbolic), Literal('a')), Literal('b')))
  check("(?(*negative_lookahead:a)b)", "", PCRE, Perl):
    Command(Conditional(LookAround(NegativeLookAhead(Alphabetic), Literal('a')), Literal('b')))
  check("(?(*nla:a)b)", "", PCRE, Perl):
    Command(Conditional(LookAround(NegativeLookAhead(Abbrev), Literal('a')), Literal('b')))
  check("(?(?<=a)b)", "", DotNet, PCRE, Perl):
    Command(Conditional(LookAround(PositiveLookBehind(Symbolic), Literal('a')), Literal('b')))
  check("(?(*positive_lookbehind:a)b)", "", PCRE, Perl):
    Command(Conditional(LookAround(PositiveLookBehind(Alphabetic), Literal('a')), Literal('b')))
  check("(?(*plb:a)b)", "", PCRE, Perl):
    Command(Conditional(LookAround(PositiveLookBehind(Abbrev), Literal('a')), Literal('b')))
  check("(?(?<!a)b)", "", DotNet, PCRE, Perl):
    Command(Conditional(LookAround(NegativeLookBehind(Symbolic), Literal('a')), Literal('b')))
  check("(?(*negative_lookbehind:a)b)", "", PCRE, Perl):
    Command(Conditional(LookAround(NegativeLookBehind(Alphabetic), Literal('a')), Literal('b')))
  check("(?(*nlb:a)b)", "", PCRE, Perl):
    Command(Conditional(LookAround(NegativeLookBehind(Abbrev), Literal('a')), Literal('b')))
  check("(?(.)b)", "", DotNet)(Command(Conditional(LookAround(Dot), Literal('b'))))
  check("(?(?C1)(?=a)b)", "", PCRE):
    Command(
      Conditional(
        CalloutLookAround(CommandKind.CalloutInt(1), LookAround(PositiveLookAhead(Symbolic), Literal('a'))),
        Literal('b')
      )
    )
  check("(?(?C{x})(*nlb:a)b|c)", "", PCRE):
    Command(
      Conditional(
        CalloutLookAround(
          CommandKind.CalloutString('{', '}', "x"),
          LookAround(NegativeLookBehind(Abbrev), Literal('a'))
        ),
        Literal('b'),
        Literal('c')
      )
    )
  check("(?(?C) (?!a)b)", "x", PCRE):
    Command(
      Conditional(
        CalloutLookAround(CommandKind.Callout, LookAround(NegativeLookAhead(Symbolic), Literal('a'))),
        Literal('b')
      )
    )
  error("(?(?C1) (?=a)b)", "", PCRE)("Invalid condition at 7")
  error("(?(?C1)(?:a)b)", "", PCRE)("Invalid condition at 8")
  error("(?(?C1)(?=a)b)", "", Perl)("Invalid condition at 3")
  check("(?(?:.)b)", "", DotNet)(Command(Conditional(LookAround(Dot), Literal('b'))))
  error("(?(.)b)", "", PCRE, Perl)("Invalid condition at 3")
  check("(?(0)a)", "", DotNet)(Command(Conditional(Indexed(0), Literal('a'))))
  error("(?(0)a)", "", PCRE, Perl, Python, Ruby)("Invalid condition at 3")
  check("(?(01)a)", "", DotNet, PCRE, Python, Ruby)(Command(Conditional(Indexed(1), Literal('a'))))
  error("(?(01)a)", "", Perl)("Invalid condition at 3")
  error("(?(DEFINE)a|b)", "", PCRE, Perl)("Invalid condition at 11")
  check("(?(DEFINEx)a)", "", PCRE)(Command(Conditional(Named(Bare, "DEFINEx"), Literal('a'))))
  check("(?(Rx)a)", "", PCRE)(Command(Conditional(Named(Bare, "Rx"), Literal('a'))))
  error("(?(Rx)a)", "", Perl)("Invalid condition at 3")
  check("(?(VERSION>=10)a)", "", PCRE)(Command(Conditional(Version(lt = true, 10, 0), Literal('a'))))
  error("(?(VERSION>=10.)a)", "", PCRE)("Invalid condition at 15")
  check("(?'x'a)", "", DotNet, PCRE, Perl, Ruby)(Group(NamedCapture(Quote, "x"), Literal('a')))
  check("(?'x-y'a)", "", DotNet)(Group(Balance(Quote, Some("x"), "y"), Literal('a')))
  check("(?'-y'a)", "", DotNet)(Group(Balance(Quote, None, "y"), Literal('a')))
  check("(?P<x>a)", "", PCRE, Perl, Python)(Group(PNamedCapture("x"), Literal('a')))
  check("(?P=x)", "", PCRE, Perl, Python)(Command(PBackReference("x")))
  check("(?P>x)", "", PCRE, Perl)(Command(PNamedCall("x")))
  check("(?R)", "", PCRE, Perl)(Command(RCall))
  check("(?&x)", "", PCRE, Perl)(Command(NamedCall("x")))
  check("(?1)", "", PCRE, Perl)(Command(IndexedCall(1)))
  check("(?+1)", "", PCRE, Perl)(Command(RelativeCall(1)))
  check("(?-1)", "", PCRE, Perl)(Command(RelativeCall(-1)))
  check("(?-x)", "", DotNet, Java, PCRE, Perl, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(verbose = true)))))
  error("(?-x)", "", Python)("Invalid group at 0")
  check("(?-x:a)", "", DotNet, Java, PCRE, Perl, Python, Ruby):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(verbose = true)))), Literal('a'))
  check("(?-i:a)", "", JavaScript):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(ignoreCase = true)))), Literal('a'))
  error("(?-i)", "", JavaScript)("Invalid group at 0")
  check("(?^x)", "", PCRE, Perl)(Command(CommandKind.ResetFlag(FlagSet(verbose = true))))
  check("(?^x:a)", "", PCRE, Perl)(Group(GroupKind.ResetFlag(FlagSet(verbose = true)), Literal('a')))
  check("(?^)", "", PCRE, Perl)(Command(CommandKind.ResetFlag(FlagSet())))
  check("(?^aa)", "", PCRE, Perl)(Command(CommandKind.ResetFlag(FlagSet(ascii = true))))
  error("(?^d)", "", Perl)("Invalid flag at 3")
  error("(?^-i)", "", PCRE, Perl)("Invalid group at 3")
  check("(?#x)", "", DotNet, PCRE, Perl, Python, Ruby)(Command(Comment("x")))
  check("(?#x\\)y)", "", Python, Ruby)(Command(Comment("x\\)y")))
  check("(?#x)(?#y)a", "", DotNet, PCRE, Perl, Python, Ruby):
    Sequence(Command(Comment("x")), Command(Comment("y")), Literal('a'))
  check("a(?#x)|(?#y)", "", DotNet, PCRE, Perl, Python, Ruby):
    Disjunction(Sequence(Literal('a'), Command(Comment("x"))), Command(Comment("y")))
  check("((?#x))", "", DotNet, PCRE, Perl, Python, Ruby)(Group(IndexedCapture, Command(Comment("x"))))
  error("(?#x", "", DotNet, PCRE, Perl, Python, Ruby)("Invalid group at 4")
  check("(?(?#x)a)", "", Ruby)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), None))))
  check("(?i(?#x)m)a", "", Ruby):
    Sequence(
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true, multiline = true), None))),
      Literal('a')
    )
  check("a{1(?#x),2}", "", Ruby)(Repeat(Literal('a'), Bounded(1, 2, Greedy)))
  check("(?<a(?#x)b>c)", "", Ruby)(Group(NamedCapture(Angle, "ab"), Literal('c')))
  check("[(?#x)]", "", Ruby):
    Class(
      false,
      ClassUnion(ClassLiteral('('), ClassLiteral('?'), ClassLiteral('#'), ClassLiteral('x'), ClassLiteral(')'))
    )
  check("[#(?#x)]", "x", Ruby):
    Class(
      false,
      ClassUnion(
        ClassLiteral('#'),
        ClassLiteral('('),
        ClassLiteral('?'),
        ClassLiteral('#'),
        ClassLiteral('x'),
        ClassLiteral(')')
      )
    )
  error("\\(?#x)", "", Ruby)("Unmatched ')' at 5")
  error("(?x:#(?#\n))", "", Ruby)("Unmatched ')' at 10")
  check("(?x:#\n)(?#y)", "", Ruby):
    Sequence(Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)), Sequence()), Command(Comment("y")))
  check("(?{x})", "", Perl)(Command(InlineCode("x")))
  check("(??{x})", "", Perl)(Command(EmbedCode("x")))
  check("(?C)", "", PCRE)(Command(Callout))
  check("(?C1)", "", PCRE)(Command(CalloutInt(1)))
  error("(?C256)", "", PCRE)("Invalid group at 6")
  error("a(?C1)*", "", PCRE)("Nothing to repeat at 6")
  check("(?C{x})", "", PCRE)(Command(CalloutString('{', '}', "x")))
  check("(?C{x{{y}}z})", "", PCRE)(Command(CalloutString('{', '}', "x{{y}}z")))
  for delim <- Seq('`', '\'', '"', '^', '%', '#', '$') do
    check(s"(?C${delim}x$delim)", "", PCRE)(Command(CalloutString(delim, delim, "x")))

  check("(*ACCEPT)", "", PCRE, Perl)(Command(BacktrackControl(Some(Accept), None)))
  check("(*ACCEPT:x)", "", PCRE, Perl)(Command(BacktrackControl(Some(Accept), Some("x"))))
  check("(*FAIL)", "", PCRE, Perl)(Command(BacktrackControl(Some(Fail), None)))
  check("(*FAIL:x)", "", PCRE, Perl)(Command(BacktrackControl(Some(Fail), Some("x"))))
  check("(*MARK:x)", "", PCRE, Perl)(Command(BacktrackControl(Some(Mark), Some("x"))))
  check("(*COMMIT)", "", PCRE, Perl)(Command(BacktrackControl(Some(Commit), None)))
  check("(*COMMIT:x)", "", PCRE, Perl)(Command(BacktrackControl(Some(Commit), Some("x"))))
  check("(*PRUNE)", "", PCRE, Perl)(Command(BacktrackControl(Some(Prune), None)))
  check("(*PRUNE:x)", "", PCRE, Perl)(Command(BacktrackControl(Some(Prune), Some("x"))))
  check("(*SKIP)", "", PCRE, Perl)(Command(BacktrackControl(Some(Skip), None)))
  check("(*SKIP:x)", "", PCRE, Perl)(Command(BacktrackControl(Some(Skip), Some("x"))))
  check("(*THEN)", "", PCRE, Perl)(Command(BacktrackControl(Some(Then), None)))
  check("(*THEN:x)", "", PCRE, Perl)(Command(BacktrackControl(Some(Then), Some("x"))))
  check("(*positive_lookahead:a)", "", PCRE, Perl)(Group(PositiveLookAhead(Alphabetic), Literal('a')))
  check("(*pla:a)", "", PCRE, Perl)(Group(PositiveLookAhead(Abbrev), Literal('a')))
  check("(*negative_lookahead:a)", "", PCRE, Perl)(Group(NegativeLookAhead(Alphabetic), Literal('a')))
  check("(*nla:a)", "", PCRE, Perl)(Group(NegativeLookAhead(Abbrev), Literal('a')))
  check("(*positive_lookbehind:a)", "", PCRE, Perl)(Group(PositiveLookBehind(Alphabetic), Literal('a')))
  check("(*plb:a)", "", PCRE, Perl)(Group(PositiveLookBehind(Abbrev), Literal('a')))
  check("(*negative_lookbehind:a)", "", PCRE, Perl)(Group(NegativeLookBehind(Alphabetic), Literal('a')))
  check("(*nlb:a)", "", PCRE, Perl)(Group(NegativeLookBehind(Abbrev), Literal('a')))
  check("(*atomic:a)", "", PCRE, Perl)(Group(Atomic(Alphabetic), Literal('a')))
  check("(*script_run:a)", "", PCRE, Perl)(Group(ScriptRun(Alphabetic), Literal('a')))
  check("(*sr:a)", "", PCRE, Perl)(Group(ScriptRun(Abbrev), Literal('a')))
  check("(*atomic_script_run:a)", "", PCRE, Perl)(Group(AtomicScriptRun(Alphabetic), Literal('a')))
  check("(*asr:a)", "", PCRE, Perl)(Group(AtomicScriptRun(Abbrev), Literal('a')))
  check("(*non_atomic_positive_lookahead:a)", "", PCRE)(Group(NonAtomicPositiveLookAhead(Alphabetic), Literal('a')))
  check("(*napla:a)", "", PCRE)(Group(NonAtomicPositiveLookAhead(Abbrev), Literal('a')))
  check("(*non_atomic_positive_lookbehind:a)", "", PCRE)(Group(NonAtomicPositiveLookBehind(Alphabetic), Literal('a')))
  check("(*naplb:a)", "", PCRE)(Group(NonAtomicPositiveLookBehind(Abbrev), Literal('a')))
  check("(*:x)", "", PCRE, Perl)(Command(BacktrackControl(None, Some("x"))))
  check("(*scan_substring:(1)a)", "", PCRE)(Group(ScanSubstring(Alphabetic, Seq(IndexedReference(1))), Literal('a')))
  check("(*scs:(-1,<x>,'y')a)", "", PCRE):
    Group(
      ScanSubstring(Abbrev, Seq(RelativeReference(-1), NamedReference("x"), NamedReference("y"))),
      Literal('a')
    )
  error("(*scs:(x)a)", "", PCRE)("Invalid group at 7")
  error("(*scs:(1,)a)", "", PCRE)("Invalid group at 9")
  error("(*scs:(1)a)", "", Perl)("Invalid group at 2")
  check("(*:a b-c)", "", PCRE, Perl)(Command(BacktrackControl(None, Some("a b-c"))))
  error("(*:)", "", PCRE, Perl)("Invalid group at 3")
  error("(*MARK)", "", PCRE, Perl)("Invalid group at 6")
  error("(*MARK:)", "", PCRE, Perl)("Invalid group at 7")
  check("(*F)", "", PCRE, Perl)(Command(BacktrackControl(Some(F), None)))
  check("(*COMMIT:)", "", PCRE, Perl)(Command(BacktrackControl(Some(Commit), Some(""))))
  check("(*ACCEPT)?", "", PCRE, Perl)(Repeat(Command(BacktrackControl(Some(Accept), None)), Question(Greedy)))
  check("(*COMMIT)?", "", Perl)(Repeat(Command(BacktrackControl(Some(Commit), None)), Question(Greedy)))
  error("(*COMMIT)?", "", PCRE)("Nothing to repeat at 9")
  check("(*UTF)(*LIMIT_MATCH=10)a", "", PCRE):
    Sequence(Command(StartOption("UTF", None)), Command(StartOption("LIMIT_MATCH", Some(10))), Literal('a'))
  error("(*UTF)", "", Perl)("Invalid group at 2")
  error("a(*UTF)", "", PCRE)("Invalid group at 3")
  error("(?i)(*UTF)", "", PCRE)("Invalid group at 6")
  error("(*UTF)*", "", PCRE)("Nothing to repeat at 6")
  check("(*TURKISH_CASING)(*UTF)", "", PCRE):
    Sequence(Command(StartOption("TURKISH_CASING", None)), Command(StartOption("UTF", None)))
  error("(*TURKISH_CASING)", "", PCRE)("Invalid group at 2")
