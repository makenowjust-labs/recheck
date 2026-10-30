package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.AssertKind.*
import codes.quine.labs.resyntax.ast.AssertNameStyle.*
import codes.quine.labs.resyntax.ast.BackReferenceStyle.*
import codes.quine.labs.resyntax.ast.BackslashKind.*
import codes.quine.labs.resyntax.ast.BacktrackControlKind.*
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.BoundaryModifier.*
import codes.quine.labs.resyntax.ast.CaseCommandKind.*
import codes.quine.labs.resyntax.ast.ClassItemData.*
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.CommandKind.*
import codes.quine.labs.resyntax.ast.ConditionalTest.*
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.EscapeClassKind.*
import codes.quine.labs.resyntax.ast.EscapeStyle.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.GroupKind.*
import codes.quine.labs.resyntax.ast.NameStyle.*
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.Quantifier.*
import codes.quine.labs.resyntax.ast.QuoteLiteral.*
import codes.quine.labs.resyntax.ast.Reference.*

class ParserSuite extends munit.FunSuite:
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

  // Top
  error(")", "", All*)("Unmatched ')' at 0")

  // Comment
  check(" ", "x", DotNet, Java, PCRE, Perl, Python, Ruby)(Sequence())
  check("# foo", "x", DotNet, Java, PCRE, Perl, Python, Ruby)(Sequence())
  check("# \\\nfoo", "x", Python)(Sequence())

  // Disjunction:
  check("a|b", "", All*)(Disjunction(Literal('a'), Literal('b')))

  // Sequence:
  check("ab", "", All*)(Sequence(Literal('a'), Literal('b')))
  check("\uD83D\uDE00", "", Java, Perl, Python, Ruby)(Literal(0x1f600))
  check("\uD83D\uDE00", "", DotNet, JavaScript, PCRE)(Sequence(Literal(0xd83d), Literal(0xde00)))

  // Repeat:
  check("a*", "", All*)(Repeat(Literal('a'), Star(Greedy)))
  check("a*?", "", All*)(Repeat(Literal('a'), Star(Lazy)))
  check("a*+", "", Java, PCRE, Perl, Python, Ruby)(Repeat(Literal('a'), Star(Possessive)))
  check("a+", "", All*)(Repeat(Literal('a'), Plus(Greedy)))
  check("a+?", "", All*)(Repeat(Literal('a'), Plus(Lazy)))
  check("a++", "", Java, PCRE, Perl, Python, Ruby)(Repeat(Literal('a'), Plus(Possessive)))
  check("a?", "", All*)(Repeat(Literal('a'), Question(Greedy)))
  check("a??", "", All*)(Repeat(Literal('a'), Question(Lazy)))
  check("a?+", "", Java, PCRE, Perl, Python, Ruby)(Repeat(Literal('a'), Question(Possessive)))
  check("a{2}", "", All*)(Repeat(Literal('a'), Exact(2, Greedy)))
  check("a{2}?", "", DotNet, Java, JavaScript, PCRE, Perl, Python)(Repeat(Literal('a'), Exact(2, Lazy)))
  check("a{2}?", "", Ruby)(Repeat(Repeat(Literal('a'), Exact(2, Greedy)), Question(Greedy)))
  check("a{2}+", "", Java, PCRE, Perl, Python)(Repeat(Literal('a'), Exact(2, Possessive)))
  check("a{2}+", "", Ruby)(Repeat(Repeat(Literal('a'), Exact(2, Greedy)), Plus(Greedy)))
  check("a{2,}", "", All*)(Repeat(Literal('a'), Unbounded(2, Greedy)))
  check("a{2,}?", "", All*)(Repeat(Literal('a'), Unbounded(2, Lazy)))
  check("a{2,}+", "", Java, PCRE, Perl, Python, Ruby)(Repeat(Literal('a'), Unbounded(2, Possessive)))
  check("a{2,3}", "", All*)(Repeat(Literal('a'), Bounded(2, 3, Greedy)))
  check("a{2,3}?", "", All*)(Repeat(Literal('a'), Bounded(2, 3, Lazy)))
  check("a{2,3}+", "", Java, PCRE, Perl, Python, Ruby)(Repeat(Literal('a'), Bounded(2, 3, Possessive)))
  check("a{65535}", "", All*)(Repeat(Literal('a'), Exact(65535, Greedy)))
  error("a{65536}", "", PCRE)("Number too big in {} quantifier at 2")
  check("a{,100000}", "", Ruby)(Repeat(Literal('a'), MaxBounded(100000, Greedy)))
  error("a{,100001}", "", Ruby)("Number too big in {} quantifier at 3")
  check("a{2147483646}", "", DotNet, Java, JavaScript, Perl, Python)(Repeat(Literal('a'), Exact(2147483646, Greedy)))
  error("a{2147483647}", "", Perl)("Number too big in {} quantifier at 2")
  check("a{1,2147483647}", "", DotNet, Java, JavaScript, Python)(Repeat(Literal('a'), Bounded(1, 2147483647, Greedy)))
  error("a{1,2147483648}", "", DotNet, Java)("Number too big in {} quantifier at 4")
  check("a{4294967294}", "", JavaScript, Python)(Repeat(Literal('a'), Exact(Int.MaxValue, Greedy)))
  error("a{4294967295}", "", Python)("Number too big in {} quantifier at 2")
  check("a{99999999999999999999}", "", JavaScript)(Repeat(Literal('a'), Exact(Int.MaxValue, Greedy)))
  check("a{3,2}", "", Perl)(Repeat(Literal('a'), Bounded(3, 2, Greedy)))
  error("a{3,2}", "", DotNet, Java, JavaScript, PCRE, Python, Ruby)("Numbers out of order in {} quantifier at 1")
  check("a{,3}", "", PCRE, Perl, Python, Ruby)(Repeat(Literal('a'), MaxBounded(3, Greedy)))
  check("a{,3}?", "", PCRE, Perl, Python, Ruby)(Repeat(Literal('a'), MaxBounded(3, Lazy)))
  check("a{,3}+", "", PCRE, Perl, Python, Ruby)(Repeat(Literal('a'), MaxBounded(3, Possessive)))
  check("a{", "", DotNet, JavaScript, PCRE, Perl, Python, Ruby)(Sequence(Literal('a'), Literal('{')))
  check("a{,3}", "", DotNet, JavaScript)(Sequence(Literal('a'), Literal('{'), Literal(','), Literal('3'), Literal('}')))
  check("a{,}", "", DotNet, JavaScript, PCRE, Perl, Python, Ruby):
    Sequence(Literal('a'), Literal('{'), Literal(','), Literal('}'))
  check("a{,3x", "", PCRE, Perl, Python, Ruby):
    Sequence(Literal('a'), Literal('{'), Literal(','), Literal('3'), Literal('x'))
  check("a{ 2 }", "", PCRE, Perl)(Repeat(Literal('a'), Exact(2, Greedy)))
  check("a{ 2 , 3 }", "", PCRE, Perl)(Repeat(Literal('a'), Bounded(2, 3, Greedy)))
  check("a{\t2,\t}", "", PCRE, Perl)(Repeat(Literal('a'), Unbounded(2, Greedy)))
  check("a{ ,3}", "", PCRE, Perl)(Repeat(Literal('a'), MaxBounded(3, Greedy)))
  check("a{ 2}", "", DotNet, JavaScript, Python, Ruby):
    Sequence(Literal('a'), Literal('{'), Literal(' '), Literal('2'), Literal('}'))
  error("a{", "", Java)("Incomplete quantifier at 1")
  check("\\d{", "", DotNet, JavaScript, PCRE, Python, Ruby)(Sequence(Backslash(EscapeClass(Digit)), Literal('{')))
  error("\\d{", "", Perl)("Unescaped left brace at 2")
  error("\\E{x", "", Perl)("Unescaped left brace at 2")
  error("\\\\p{x", "", Perl)("Unescaped left brace at 3")
  error("a\\c\\p{x", "", Perl)("Unescaped left brace at 5")
  check("\\\\pa{", "", Perl):
    Sequence(Backslash(Escape(Single('\\'), '\\')), Literal('p'), Literal('a'), Literal('{'))
  check("\\\\p{", "", PCRE)(Sequence(Backslash(Escape(Single('\\'), '\\')), Literal('p'), Literal('{')))
  check("\\x41{", "", Perl)(Sequence(Backslash(Escape(Hex2, 0x41)), Literal('{')))
  check("\\-{", "", Perl)(Sequence(Backslash(Unknown('-')), Literal('{')))
  check("\\d{2}", "", All*)(Repeat(Backslash(EscapeClass(Digit)), Exact(2, Greedy)))
  error("a{", "u", JavaScript)("Incomplete quantifier at 1")
  error("*", "", All*)("Nothing to repeat at 0")
  check("a**", "", Ruby)(Repeat(Repeat(Literal('a'), Star(Greedy)), Star(Greedy)))
  error("a**", "", DotNet, Java, JavaScript, PCRE, Perl, Python)("Nested quantifier at 2")
  error("a*++", "", Java, PCRE, Perl, Python)("Nested quantifier at 3")
  check("a{2}{3}", "", Java)(Repeat(Literal('a'), Exact(2, Greedy)))
  check("a*?{2,}+", "", Java)(Repeat(Literal('a'), Star(Lazy)))
  error("a{2}{3}", "", DotNet, JavaScript, PCRE, Perl, Python)("Nested quantifier at 7")
  error("a{2}*", "", Java)("Nested quantifier at 4")
  check("({1})", "", Java)(Group(IndexedCapture, Sequence()))
  check("({1})", "", Perl)(Group(IndexedCapture, Sequence(Literal('{'), Literal('1'), Literal('}'))))
  error("({1})", "", DotNet, JavaScript, PCRE, Python, Ruby)("Nothing to repeat at 4")
  error("a*+", "", DotNet, JavaScript)("Nested quantifier at 2")
  check("^*", "", DotNet, Java, Perl, Ruby)(Repeat(Caret, Star(Greedy)))
  error("^*", "", JavaScript, PCRE, Python)("Nothing to repeat at 1")
  check("$+", "", DotNet, Java, Perl, Ruby)(Repeat(Dollar, Plus(Greedy)))
  error("$*", "", JavaScript, PCRE, Python)("Nothing to repeat at 1")
  check("\\b?", "", DotNet, Java, Perl, Ruby)(Repeat(Backslash(Assert(Boundary(None))), Question(Greedy)))
  error("\\b?", "", JavaScript, PCRE, Python)("Nothing to repeat at 2")
  check("\\A{2}", "", DotNet, Java, Perl, Ruby)(Repeat(Backslash(Assert(Begin)), Exact(2, Greedy)))
  error("\\A{2}", "", PCRE, Python)("Nothing to repeat at 5")
  error("(?=a)*", "u", JavaScript)("Nothing to repeat at 5")
  error("(?<=a)*", "", JavaScript)("Nothing to repeat at 6")
  check("}", "", All*)(Literal('}'))
  error("}", "u", JavaScript)("Incomplete quantifier at 0")

  // Group
  check("(?:a)", "", All*)(Group(NonCapture, Literal('a')))
  check("(?|a)", "", PCRE, Perl)(Command(BranchReset(Literal('a'))))
  check("(?|a|b)", "", PCRE, Perl)(Command(BranchReset(Literal('a'), Literal('b'))))
  check("(?=a)", "", All*)(Group(PositiveLookAhead(Symbolic), Literal('a')))
  check("(?!a)", "", All*)(Group(NegativeLookAhead(Symbolic), Literal('a')))
  check("(?<=a)", "", All*)(Group(PositiveLookBehind(Symbolic), Literal('a')))
  check("(?<!a)", "", All*)(Group(NegativeLookBehind(Symbolic), Literal('a')))
  check("(?<*a)", "", PCRE)(Group(NonAtomicPositiveLookBehind(Symbolic), Literal('a')))
  check("(?<x>a)", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(Group(NamedCapture(Angle, "x"), Literal('a')))
  check("(?<a_1>a)", "", DotNet, JavaScript, PCRE, Perl, Ruby)(Group(NamedCapture(Angle, "a_1"), Literal('a')))
  error("(?<a_1>a)", "", Java)("Invalid identifier at 4")
  check("(?<_>a)", "", DotNet, JavaScript, PCRE, Perl, Ruby)(Group(NamedCapture(Angle, "_"), Literal('a')))
  error("(?<_>a)", "", Java)("Invalid identifier at 3")
  check("(?<$>a)", "", JavaScript, Ruby)(Group(NamedCapture(Angle, "$"), Literal('a')))
  error("(?<$>a)", "", DotNet, Java, PCRE, Perl)("Invalid identifier at 3")
  error("(?<1a>a)", "", Java, JavaScript, PCRE, Perl, Ruby)("Invalid identifier at 3")
  error("(?<1a>a)", "", DotNet)("Invalid identifier at 4")
  check("(?<1>a)", "", DotNet)(Group(NamedCapture(Angle, "1"), Literal('a')))
  error("(?<0>a)", "", DotNet)("Invalid identifier at 3")
  check("(?<é>a)", "", DotNet, JavaScript, Perl, Ruby)(Group(NamedCapture(Angle, "é"), Literal('a')))
  check("(?<é>a)", "u", PCRE)(Group(NamedCapture(Angle, "é"), Literal('a')))
  error("(?<é>a)", "", Java, PCRE)("Invalid identifier at 3")
  check("(?<\u2170>a)", "", Perl)(Group(NamedCapture(Angle, "\u2170"), Literal('a')))
  error("(?<\u2170>a)", "", DotNet)("Invalid identifier at 3")
  check("(?<\u203f>a)", "", DotNet)(Group(NamedCapture(Angle, "\u203f"), Literal('a')))
  error("(?<\u203f>a)", "", Perl)("Invalid identifier at 3")
  check("(?<\u0300>a)", "", DotNet)(Group(NamedCapture(Angle, "\u0300"), Literal('a')))
  check("(?<\u0661>a)", "", DotNet)(Group(NamedCapture(Angle, "\u0661"), Literal('a')))
  error("(?<\u0661>a)", "", Perl)("Invalid identifier at 3")
  check("(?<a\u0903\u200d>a)", "", Perl)(Group(NamedCapture(Angle, "a\u0903\u200d"), Literal('a')))
  error("(?<a\u0903>a)", "", DotNet)("Invalid identifier at 4")
  error("(?<a\u00b7>a)", "", DotNet, Perl)("Invalid identifier at 4")
  check("(?<\ud835\udc65>a)", "", Perl)(Group(NamedCapture(Angle, "\ud835\udc65"), Literal('a')))
  check("(?<\\u{41}\\u0042>a)", "", JavaScript)(Group(NamedCapture(Angle, "AB"), Literal('a')))
  check("(?<\\ud835\\udc65>a)", "", JavaScript)(Group(NamedCapture(Angle, "\ud835\udc65"), Literal('a')))
  error("(?<\\u0031>a)", "", JavaScript)("Invalid identifier at 3")
  check("(?<a-b>a)", "", Ruby)(Group(NamedCapture(Angle, "a-b"), Literal('a')))
  check("(?<\\w+[>a)", "", Ruby)(Group(NamedCapture(Angle, "\\w+["), Literal('a')))
  error("(?<-a>a)", "", Ruby)("Invalid identifier at 3")
  error("(?<\u0661>a)", "", Ruby)("Invalid identifier at 3")
  check("(?<a\u0661>a)", "", Ruby)(Group(NamedCapture(Angle, "a\u0661"), Literal('a')))
  error("(?<a)>a)", "", Ruby)("Invalid identifier at 4")
  check("(?<)>a)", "", Ruby)(Group(NamedCapture(Angle, ")"), Literal('a')))
  check(s"(?<${"a" * 128}>a)", "", PCRE)(Group(NamedCapture(Angle, "a" * 128), Literal('a')))
  error(s"(?<${"a" * 129}>a)", "", PCRE)("Invalid identifier at 3")
  check("(?P<é>a)", "", Python)(Group(PNamedCapture("é"), Literal('a')))
  error("(?P<$>a)", "", PCRE, Perl, Python)("Invalid identifier at 4")
  check("(?<x>a)(?<x>b)", "", DotNet, Perl, Ruby):
    Sequence(Group(NamedCapture(Angle, "x"), Literal('a')), Group(NamedCapture(Angle, "x"), Literal('b')))
  check("(?<x>a)(?<x>b)", "J", PCRE):
    Sequence(Group(NamedCapture(Angle, "x"), Literal('a')), Group(NamedCapture(Angle, "x"), Literal('b')))
  error("(?<x>a)(?<x>b)", "", Java, JavaScript, PCRE)("Duplicated group name at 7")
  error("(?P<x>a)(?P<x>b)", "", PCRE, Python)("Duplicated group name at 8")
  check("(?<x>a)(?J)(?<x>b)", "", PCRE):
    Sequence(
      Group(NamedCapture(Angle, "x"), Literal('a')),
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(dupNames = true), None))),
      Group(NamedCapture(Angle, "x"), Literal('b'))
    )
  error("(?J:(?<x>a))(?<x>b)", "", PCRE)("Duplicated group name at 12")
  check("(?<x>a)|(?<x>b)", "", JavaScript):
    Disjunction(Group(NamedCapture(Angle, "x"), Literal('a')), Group(NamedCapture(Angle, "x"), Literal('b')))
  error("(?<x>a)|(?<x>b)", "", Java, PCRE)("Duplicated group name at 8")
  error("(?:(?<x>a)|(?<x>b))(?<x>c)", "", JavaScript)("Duplicated group name at 19")
  check("(?|(?<x>a)|(?<x>b))", "", PCRE, Perl):
    Command(BranchReset(Group(NamedCapture(Angle, "x"), Literal('a')), Group(NamedCapture(Angle, "x"), Literal('b'))))
  error("(?|(?<x>a)|(?<x>b))(?<x>c)", "", PCRE)("Duplicated group name at 19")
  check("(?<x-y>a)", "", DotNet)(Group(Balance(Angle, Some("x"), "y"), Literal('a')))
  check("(?<x-1>a)", "", DotNet)(Group(Balance(Angle, Some("x"), "1"), Literal('a')))
  check("(?<-y>a)", "", DotNet)(Group(Balance(Angle, None, "y"), Literal('a')))
  check("(?>a)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(Group(Atomic(Symbolic), Literal('a')))
  check("(?*a)", "", PCRE)(Group(NonAtomicPositiveLookAhead(Symbolic), Literal('a')))
  check("(?~a)", "", Ruby)(Group(Absence, Literal('a')))
  check("(?~|a|b|c)", "", Ruby)(Command(AbsenceExpression(Literal('a'), Disjunction(Literal('b'), Literal('c')))))
  check("(?~|a)", "", Ruby)(Command(AbsenceStopper(Literal('a'))))
  check("(?~|)", "", Ruby)(Command(AbsenceRangeClear))
  error("(?~|a)", "", DotNet, Java, JavaScript, PCRE, Perl, Python)("Invalid group at 2")
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
  check("(?i)", "", DotNet, Java, PCRE, Perl, Python, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None)))
  check("(?m)", "", DotNet, Java, PCRE, Perl, Python, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(multiline = true), None)))
  check("(?x)", "", DotNet, Java, PCRE, Perl, Python, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)))
  check("(?J)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(dupNames = true), None)))
  error("(?J)", "", DotNet, Java, Perl, Python, Ruby)("Invalid flag at 2")
  check("(?L)", "b", Python):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(localeUpper = true), None)))
  error("(?L)", "", DotNet, Java, PCRE, Perl, Python, Ruby)("Invalid flag at 2")
  check("(?U)", "", Java):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ungreedy = true), None)))
  check("(?U)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ungreedy = true), None)))
  error("(?U)", "", DotNet, Perl, Python, Ruby)("Invalid flag at 2")
  check("(?a)", "", PCRE, Perl, Python, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), None)))
  error("(?a)", "", DotNet, Java)("Invalid flag at 2")
  check("(?aD)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiDigit = true), None))))
  check("(?aP)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiPosix = true), None))))
  check("(?aS)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiSpace = true), None))))
  check("(?aT)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiPosixDigit = true), None))))
  check("(?aW)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiWord = true), None))))
  check("(?aDa)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true, asciiDigit = true), None)))
  check("(?aU)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true, ungreedy = true), None)))
  check("(?a-aD)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), Some(FlagSet(asciiDigit = true)))))
  check("(?-aW:a)", "", PCRE):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(asciiWord = true)))), Literal('a'))
  check("(?^aS)", "", PCRE)(Command(CommandKind.ResetFlag(FlagSet(asciiSpace = true))))
  error("(?aDS)", "", PCRE)("Invalid flag at 4")
  error("(?aD)", "", Perl, Python, Ruby)("Invalid flag at 3")
  check("(?c)", "", Java, Perl):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(continue = true), None)))
  error("(?c)", "", DotNet, PCRE, Python, Ruby)("Invalid flag at 2")
  check("(?d)", "", Java, Perl, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(hasIndices = true), None)))
  error("(?d)", "", DotNet, PCRE, Python)("Invalid flag at 2")
  check("(?g)", "", Perl):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(global = true), None)))
  error("(?g)", "", DotNet, Java, PCRE, Python, Ruby)("Invalid flag at 2")
  check("(?l)", "", Perl):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(localeLower = true), None)))
  error("(?l)", "", DotNet, Java, PCRE, Python, Ruby)("Invalid flag at 2")
  check("(?n)", "", DotNet, PCRE, Perl):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(explicitCapture = true), None)))
  error("(?n)", "", Java, Python, Ruby)("Invalid flag at 2")
  check("(?o)", "", Perl):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(once = true), None)))
  error("(?o)", "", DotNet, Java, PCRE, Python, Ruby)("Invalid flag at 2")
  check("(?p)", "", Perl):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(preserve = true), None)))
  error("(?p)", "", DotNet, Java, PCRE, Python, Ruby)("Invalid flag at 2")
  check("(?r)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(nonDestructive = true), None)))
  error("(?r)", "", DotNet, Java, Perl, Python, Ruby)("Invalid flag at 2")
  check("(?s)", "", DotNet, Java, PCRE, Perl, Python):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(dotAll = true), None)))
  error("(?s)", "", Ruby)("Invalid flag at 2")
  check("(?u)", "", Java, Perl, Python, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(unicode = true), None)))
  error("(?u)", "", DotNet, PCRE)("Invalid flag at 2")
  error("(?u)", "b", Python)("Invalid flag at 2")
  check("(?i-m+s-x)", "", DotNet):
    Command(
      CommandKind.InlineFlag(
        FlagSetDiff(FlagSet(ignoreCase = true, dotAll = true), Some(FlagSet(multiline = true, verbose = true)))
      )
    )
  check("(?+)", "", DotNet)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(), None))))
  error("(?i-m-s)", "", Java, PCRE, Perl)("Invalid flag at 5")
  error("(?i-m-s)", "", Ruby)("Invalid flag at 6")
  check("(?i-m-x)", "", Ruby):
    Command(
      CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), Some(FlagSet(multiline = true, verbose = true))))
    )
  check("(?--i:a)", "", Ruby):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(ignoreCase = true)))), Literal('a'))
  error("(?--i)", "", Java, PCRE, Perl)("Invalid group at 3")
  error("(?<\ud835\udc65>a)", "", DotNet)("Invalid identifier at 3")
  check("(?xx)", "", PCRE, Perl):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true, verboseMore = true), None)))
  check("(?xx)", "", DotNet, Java, Python, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)))
  check("(?I)", "", DotNet):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None)))
  error("(?I)", "", Java, PCRE, Perl, Python, Ruby)("Invalid flag at 2")
  check("(?)", "", Java, PCRE, Perl)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(), None))))
  error("(?)", "", DotNet, Python, Ruby)("Invalid flag at 2")
  check("(?-)", "", DotNet, Java, PCRE, Perl, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet()))))
  error("(?-:a)", "", JavaScript, Python)("Invalid flag at 3")
  check("(?i-:a)", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), Some(FlagSet()))), Literal('a'))
  error("(?i-:a)", "", Python)("Invalid flag at 4")
  check("(?ii)", "", DotNet, Java, PCRE, Perl, Python, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None)))
  error("(?ii:a)", "", JavaScript)("Duplicated flag at 3")
  check("(?i-i)", "", DotNet, Java, PCRE, Perl, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), Some(FlagSet(ignoreCase = true)))))
  error("(?i-i:a)", "", JavaScript, Python)("Invalid flag at 4")
  check("(?aa)", "", PCRE, Perl, Python, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), None)))
  error("(?aaa)", "", Perl)("Duplicated flag at 4")
  error("(?uu)", "", Perl)("Duplicated flag at 3")
  check("(?au)", "", Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true, unicode = true), None)))
  error("(?au)", "", Perl, Python)("Invalid flag at 3")
  check("(?-a:a)", "", PCRE):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(ascii = true)))), Literal('a'))
  error("(?-a:a)", "", Perl, Python, Ruby)("Invalid flag at 3")
  check("(?im-s:a)", "", JavaScript):
    Group(
      GroupKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true, multiline = true), Some(FlagSet(dotAll = true)))),
      Literal('a')
    )
  error("(?x:a)", "", JavaScript)("Invalid flag at 2")
  error("(?i)", "", JavaScript)("Invalid group at 0")
  check("(?i)(?#x)(?m)a", "", Python):
    Sequence(
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None))),
      Command(Comment("x")),
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(multiline = true), None))),
      Literal('a')
    )
  error("a(?i)", "", Python)("Inline flag not at the start at 1")
  error("a|(?i)", "", Python)("Inline flag not at the start at 2")
  error("((?i))", "", Python)("Inline flag not at the start at 1")
  check("a(?x)b|c", "", DotNet, Java, PCRE, Perl):
    Disjunction(
      Sequence(Literal('a'), Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None))), Literal('b')),
      Literal('c')
    )
  check("a(?x)b|c", "", Ruby):
    Sequence(
      Literal('a'),
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None))),
      Disjunction(Literal('b'), Literal('c'))
    )
  check("(?x:a)", "", DotNet, Java, PCRE, Perl, Python, Ruby):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)), Literal('a'))
  check(" (?x: a ) ", "", DotNet, Java, PCRE, Perl, Python, Ruby):
    Sequence(
      Literal(' '),
      Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)), Literal('a')),
      Literal(' ')
    )
  check("(?x-i)", "", DotNet, Java, PCRE, Perl, Ruby):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), Some(FlagSet(ignoreCase = true)))))
  error("(?x-i)", "", Python)("Invalid group at 0")
  check("(?x-i:a)", "", DotNet, Java, PCRE, Perl, Python, Ruby):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), Some(FlagSet(ignoreCase = true)))), Literal('a'))
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
  error("a(?i)*", "", DotNet, Java, PCRE, Perl)("Nothing to repeat at 5")
  check("(a)", "", All*)(Group(IndexedCapture, Literal('a')))
  error("(", "", All*)("Unclosed group at 1")

  // Caret
  check("^", "", All*)(Caret)

  // Dollar
  check("$", "", All*)(Dollar)

  // Dot
  check(".", "", All*)(Dot)

  // Backslash
  check("\\a", "", DotNet, Java, PCRE, Perl, Python, Ruby)(Backslash(Escape(Single('a'), 0x07)))
  check("\\b", "", All*)(Backslash(Assert(Boundary(None))))
  check("\\b{g}", "", Java, Perl)(Backslash(Assert(Boundary(Some(GModifier)))))
  check("\\b{gcb}", "", Perl)(Backslash(Assert(Boundary(Some(GcbModifier)))))
  check("\\b{lb}", "", Perl)(Backslash(Assert(Boundary(Some(LbModifier)))))
  check("\\b{sb}", "", Perl)(Backslash(Assert(Boundary(Some(SbModifier)))))
  check("\\b{wb}", "", Perl)(Backslash(Assert(Boundary(Some(WbModifier)))))
  check("\\cA", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(Backslash(Escape(Control('A'), 0x01)))
  check("\\cz", "", DotNet, JavaScript, PCRE, Perl, Ruby)(Backslash(Escape(Control('z'), 0x1a)))
  check("\\cz", "", Java)(Backslash(Escape(Control('z'), 0x3a)))
  check("\\c1", "", Java, PCRE, Perl)(Backslash(Escape(Control('1'), 0x71)))
  check("\\c1", "", Ruby)(Backslash(Escape(Control('1'), 0x11)))
  error("\\c1", "", DotNet)("Invalid backslash escape at 2")
  error("\\c1", "u", JavaScript)("Invalid backslash escape at 2")
  check("\\c1", "", JavaScript)(Sequence(Backslash(Unknown('c')), Literal('1')))
  check("[\\c1]", "", JavaScript)(Class(false, ClassBackslashValue(Escape(Control('1'), 0x11))))
  check("\\c?", "", PCRE, Perl, Ruby)(Backslash(Escape(Control('?'), 0x7f)))
  check("\\c{", "", Java, PCRE)(Backslash(Escape(Control('{'), 0x3b)))
  check("\\c{", "", Ruby)(Backslash(Escape(Control('{'), 0x1b)))
  error("\\c{", "", Perl)("Invalid backslash escape at 2")
  error("\\c", "", DotNet, Java, PCRE, Perl, Ruby)("Invalid backslash escape at 2")
  check("\\c\\\\", "", Ruby)(Backslash(Escape(Control('\\'), 0x1c)))
  check("\\C-a", "", Ruby)(Backslash(Escape(ControlDash, 0x01)))
  check("\\M-a", "", Ruby)(Backslash(Escape(Meta, 0xe1)))
  check("\\M-\\C-a", "", Ruby)(Backslash(Escape(Meta, 0x81)))
  error("\\C-", "", Ruby)("Invalid backslash escape at 3")
  check("\\C", "", PCRE)(Backslash(EscapeClass(CodeUnit)))
  error("\\C", "", Perl)("Invalid backslash escape at 1")
  error("[\\C]", "", PCRE)("Invalid backslash escape at 2")
  check("\\c@", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(Escape(Control('@'), 0x00)))
  check("\\c_", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(Escape(Control('_'), 0x1f)))
  check("\\d", "", All*)(Backslash(EscapeClass(Digit)))
  check("\\e", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(Escape(Single('e'), 0x1b)))
  check("\\f", "", All*)(Backslash(Escape(Single('f'), 0x0c)))
  check("\\g1", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Bare), IndexedReference(1))))
  check("\\g+1", "", PCRE)(Backslash(EscapeBackReference(GBackReference(Bare), RelativeReference(1))))
  error("\\g+1", "", Perl)("Invalid backslash escape at 2")
  error("\\g-0", "", PCRE, Perl)("Invalid backslash escape at 3")
  check("\\g-1", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Bare), RelativeReference(-1))))
  check("\\g{1}", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Curly), IndexedReference(1))))
  check("\\g{+1}", "", PCRE)(Backslash(EscapeBackReference(GBackReference(Curly), RelativeReference(1))))
  error("\\g{+1}", "", Perl)("Invalid identifier at 3")
  error("\\g{-0}", "", PCRE, Perl)("Invalid backslash escape at 4")
  check("\\g{-1}", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Curly), RelativeReference(-1))))
  check("\\g{foo}", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Curly), NamedReference("foo"))))
  check("\\g{é}", "", Perl)(Backslash(EscapeBackReference(GBackReference(Curly), NamedReference("é"))))
  check("\\g<1>", "", PCRE, Ruby)(Backslash(EscapeCall(Angle, IndexedReference(1))))
  check("\\g<+1>", "", PCRE, Ruby)(Backslash(EscapeCall(Angle, RelativeReference(1))))
  check("\\g<-1>", "", PCRE, Ruby)(Backslash(EscapeCall(Angle, RelativeReference(-1))))
  check("\\g<foo>", "", PCRE, Ruby)(Backslash(EscapeCall(Angle, NamedReference("foo"))))
  check("\\g'1'", "", PCRE, Ruby)(Backslash(EscapeCall(Quote, IndexedReference(1))))
  check("\\g'+1'", "", PCRE, Ruby)(Backslash(EscapeCall(Quote, RelativeReference(1))))
  check("\\g'-1'", "", PCRE, Ruby)(Backslash(EscapeCall(Quote, RelativeReference(-1))))
  check("\\g'foo'", "", PCRE, Ruby)(Backslash(EscapeCall(Quote, NamedReference("foo"))))
  check("\\g<a-b+1>", "", Ruby)(Backslash(EscapeCall(Angle, NamedReference("a-b+1"))))
  error("\\g<+0>", "", PCRE, Ruby)("Invalid backslash escape at 4")
  error("\\g<a)b>", "", Ruby)("Invalid identifier at 4")
  check("\\h", "", Java, PCRE, Perl)(Backslash(EscapeClass(Horizontal)))
  check("\\h", "", Ruby)(Backslash(EscapeClass(HexDigit)))
  check("\\k<foo>", "", DotNet, Java, PCRE, Perl, Ruby):
    Backslash(EscapeBackReference(KBackReference(Angle), NamedReference("foo")))
  check("\\k<1>", "", Ruby)(Backslash(EscapeBackReference(KBackReference(Angle), IndexedReference(1))))
  error("(?<n>a)\\1", "", Ruby)("Numbered reference with named capture at 8")
  error("\\1(?'n'a)", "", Ruby)("Numbered reference with named capture at 1")
  error("(?<n>a)\\k<1>", "", Ruby)("Numbered reference with named capture at 10")
  error("(?<n>a)\\k'-1'", "", Ruby)("Numbered reference with named capture at 10")
  error("(?<n>a)\\g<1>", "", Ruby)("Numbered reference with named capture at 10")
  error("(?<n>a)\\g<+1>", "", Ruby)("Numbered reference with named capture at 10")
  error("(?<n>a)(?(1)b)", "", Ruby)("Numbered reference with named capture at 10")
  check("(?<n>a)\\g<0>", "", Ruby):
    Sequence(Group(NamedCapture(Angle, "n"), Literal('a')), Backslash(EscapeCall(Angle, IndexedReference(0))))
  check("(?<n>a)\\k<n+0>", "", Ruby):
    Sequence(
      Group(NamedCapture(Angle, "n"), Literal('a')),
      Backslash(EscapeBackReference(KBackReference(Angle), LeveledReference(NamedReference("n"), 0)))
    )
  check("(?<n>a)(a)\\1", "", PCRE, Perl):
    Sequence(
      Group(NamedCapture(Angle, "n"), Literal('a')),
      Group(IndexedCapture, Literal('a')),
      Backslash(EscapeBackReference(BareBackReference, IndexedReference(1)))
    )
  check("\\k<+1>", "", Ruby)(Backslash(EscapeBackReference(KBackReference(Angle), NamedReference("+1"))))
  check("\\k<-1>", "", Ruby)(Backslash(EscapeBackReference(KBackReference(Angle), RelativeReference(-1))))
  check("\\k<1+0>", "", Ruby):
    Backslash(EscapeBackReference(KBackReference(Angle), LeveledReference(IndexedReference(1), 0)))
  check("\\k<1-1>", "", Ruby):
    Backslash(EscapeBackReference(KBackReference(Angle), LeveledReference(IndexedReference(1), -1)))
  check("\\k<-1+0>", "", Ruby):
    Backslash(EscapeBackReference(KBackReference(Angle), LeveledReference(RelativeReference(-1), 0)))
  check("\\k<a.b+0>", "", Ruby):
    Backslash(EscapeBackReference(KBackReference(Angle), LeveledReference(NamedReference("a.b"), 0)))
  error("\\k<a-b>", "", Ruby)("Invalid backslash escape at 5")
  error("\\k<0>", "", Ruby)("Invalid backslash escape at 3")
  error("\\k<-0>", "", Ruby)("Invalid backslash escape at 4")
  check("\\k<0>", "", DotNet)(Backslash(EscapeBackReference(KBackReference(Angle), NamedReference("0"))))
  error("\\k<1a>", "", DotNet)("Invalid backslash escape at 4")
  error("\\k<a_b>", "", Java)("Invalid backslash escape at 4")
  check("\\k<é>", "", DotNet, Perl, Ruby)(Backslash(EscapeBackReference(KBackReference(Angle), NamedReference("é"))))
  error("\\k<é>", "", Java, PCRE)("Invalid identifier at 3")
  check("\\k<x>", "", JavaScript)(Sequence(Backslash(Unknown('k')), Literal('<'), Literal('x'), Literal('>')))
  check("(?<foo>)\\k<foo>", "", JavaScript):
    Sequence(
      Group(NamedCapture(Angle, "foo"), Sequence()),
      Backslash(EscapeBackReference(KBackReference(Angle), NamedReference("foo")))
    )
  check("(?<AB>)\\k<\\u0041\\u{42}>", "", JavaScript):
    Sequence(
      Group(NamedCapture(Angle, "AB"), Sequence()),
      Backslash(EscapeBackReference(KBackReference(Angle), NamedReference("AB")))
    )
  check("\\k'foo'", "", DotNet, PCRE, Perl, Ruby):
    Backslash(EscapeBackReference(KBackReference(Quote), NamedReference("foo")))
  check("\\k'1'", "", Ruby)(Backslash(EscapeBackReference(KBackReference(Quote), IndexedReference(1))))
  check("\\k'+1'", "", Ruby)(Backslash(EscapeBackReference(KBackReference(Quote), NamedReference("+1"))))
  check("\\k'-1'", "", Ruby)(Backslash(EscapeBackReference(KBackReference(Quote), RelativeReference(-1))))
  check("\\k'1+0'", "", Ruby):
    Backslash(EscapeBackReference(KBackReference(Quote), LeveledReference(IndexedReference(1), 0)))
  check("\\k'1-1'", "", Ruby):
    Backslash(EscapeBackReference(KBackReference(Quote), LeveledReference(IndexedReference(1), -1)))
  error("\\k<1>", "", PCRE, Perl)("Invalid identifier at 3")
  check("\\k{ foo }", "", PCRE, Perl):
    Backslash(EscapeBackReference(KBackReference(Curly), NamedReference("foo")))
  error("\\k{foo}", "", DotNet, Java)("Invalid backslash escape at 2")
  error("\\k'foo'", "", Java)("Invalid backslash escape at 2")
  check("\\kx", "", Ruby)(Sequence(Backslash(Unknown('k')), Literal('x')))
  check("\\g{ 1 }", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Curly), IndexedReference(1))))
  check("\\g1", "", Ruby)(Sequence(Backslash(Unknown('g')), Literal('1')))
  check("\\l", "", Perl)(Backslash(CaseCommand(SingleLowerCaseCommand)))
  check("\\n", "", All*)(Backslash(Escape(Single('n'), 0x0a)))
  check("\\o{010}", "", PCRE, Perl)(Backslash(Escape(Octal, 0x08)))
  check("\\p{Letter}", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(EscapeClass(UnicodeProperty("Letter"))))
  check("\\p{Letter}", "u", JavaScript)(Backslash(EscapeClass(UnicodeProperty("Letter"))))
  check("\\p{Script=Hira}", "", DotNet, Java, PCRE, Perl, Ruby):
    Backslash(EscapeClass(UnicodePropertyValue("Script", "Hira")))
  check("\\p{Script=Hira}", "u", JavaScript)(Backslash(EscapeClass(UnicodePropertyValue("Script", "Hira"))))
  check("\\pL", "", Java, PCRE, Perl)(Backslash(EscapeClass(UnicodeBareProperty("L"))))
  check("\\pL", "", Ruby)(Sequence(Backslash(Unknown('p')), Literal('L')))
  check("[\\p]", "", Ruby)(Class(false, ClassBackslashValue(Unknown('p'))))
  error("\\pL", "", DotNet)("Invalid backslash escape at 2")
  check("\\p{^L}", "", PCRE, Perl, Ruby)(Backslash(EscapeClass(NonUnicodeProperty("L"))))
  check("\\P{^L}", "", PCRE, Perl, Ruby)(Backslash(EscapeClass(UnicodeProperty("L"))))
  error("\\p{^L}", "", DotNet)("Invalid identifier at 3")
  check("\\p{block=Basic Latin}", "", Java)(Backslash(EscapeClass(UnicodePropertyValue("block", "Basic Latin"))))
  check("\\p{ L& }", "", PCRE, Perl, Ruby)(Backslash(EscapeClass(UnicodeProperty("L&"))))
  check("\\p{Bc:EN}", "", PCRE, Perl)(Backslash(EscapeClass(UnicodePropertyValue("Bc", "EN"))))
  error("\\p{}", "", PCRE, Perl, Ruby)("Invalid backslash escape at 3")
  error("\\p{L", "", PCRE, Perl, Ruby)("Invalid backslash escape at 4")
  check("\\r", "", All*)(Backslash(Escape(Single('r'), 0x0d)))
  check("\\s", "", All*)(Backslash(EscapeClass(Space)))
  check("\\t", "", All*)(Backslash(Escape(Single('t'), 0x09)))
  check("\\u", "", Perl)(Backslash(CaseCommand(SingleUpperCaseCommand)))
  check("\\uABCD", "", DotNet, Java, JavaScript, Python, Ruby)(Backslash(Escape(UnicodeHex4, 0xabcd)))
  check("\\u{ABCDE}", "u", JavaScript)(Backslash(Escape(UnicodeBracket, 0xabcde)))
  check("\\uA", "", JavaScript)(Sequence(Backslash(Unknown('u')), Literal('A')))
  error("\\uA", "", DotNet, Java, Python, Ruby)("Invalid backslash escape at 2")
  check("\\u{41}", "", Ruby)(Backslash(Escape(UnicodeBracket, 0x41)))
  check("\\u{ 41  42 }", "", Ruby):
    Sequence(Backslash(Escape(UnicodeBracket, 0x41)), Backslash(Escape(UnicodeBracket, 0x42)))
  check("[\\u{41 42}]", "", Ruby):
    Class(
      false,
      ClassUnion(ClassBackslashValue(Escape(UnicodeBracket, 0x41)), ClassBackslashValue(Escape(UnicodeBracket, 0x42)))
    )
  error("\\u{}", "", Ruby)("Invalid backslash escape at 4")
  error("\\u{110000}", "u", JavaScript)("Code point too large at 3")
  error("\\u{110000}", "", Ruby)("Code point too large at 3")
  error("\\uD800", "", Ruby)("Invalid Unicode escape at 1")
  check("\\uD83D\\uDE00", "", Java)(Backslash(Escape(UnicodeHex4Pair, 0x1f600)))
  check("\\uD83D\\uDE00", "u", JavaScript)(Backslash(Escape(UnicodeHex4Pair, 0x1f600)))
  check("\\uD83D\\uDE00", "", DotNet, JavaScript, Python):
    Sequence(Backslash(Escape(UnicodeHex4, 0xd83d)), Backslash(Escape(UnicodeHex4, 0xde00)))
  error("\\U00110000", "", Python)("Code point too large at 1")
  check("\\v", "", Java, PCRE, Perl)(Backslash(EscapeClass(Vertical)))
  check("\\v", "", DotNet, JavaScript, Python, Ruby)(Backslash(Escape(Single('v'), 0x0b)))
  check("\\w", "", All*)(Backslash(EscapeClass(Word)))
  check("\\xAB", "", All*)(Backslash(Escape(Hex2, 0xab)))
  check("\\xA", "", PCRE, Perl, Ruby)(Backslash(Escape(Hex1, 0x0a)))
  check("\\x", "", Perl)(Backslash(Escape(Hex0, 0x00)))
  check("\\xG", "", JavaScript)(Sequence(Backslash(Unknown('x')), Literal('G')))
  error("\\xG", "", DotNet, Java, PCRE, Python, Ruby)("Invalid backslash escape at 2")
  error("\\xA", "", DotNet, Java, Python)("Invalid backslash escape at 2")
  check("\\x{}", "", Perl)(Backslash(Escape(HexBracket, 0x00)))
  error("\\x{}", "", Java, PCRE)("Invalid backslash escape at 3")
  check("\\x{ 41 }", "", PCRE, Perl)(Backslash(Escape(HexBracket, 0x41)))
  check("\\x{4G}", "", Perl)(Backslash(Escape(HexBracket, 0x04)))
  error("\\x{4G}", "", Java, PCRE)("Invalid backslash escape at 4")
  error("\\x{100}", "", PCRE)("Code point too large at 3")
  check("\\x{10FFFF}", "u", PCRE)(Backslash(Escape(HexBracket, 0x10ffff)))
  error("\\x{110000}", "", Java)("Code point too large at 3")
  check("\\x{110000}", "", Perl)(Backslash(Escape(HexBracket, 0x110000)))
  check("\\o{8}", "", Perl)(Backslash(Escape(Octal, 0x00)))
  error("\\o{8}", "", PCRE)("Invalid backslash escape at 3")
  error("\\o{}", "", PCRE, Perl)("Invalid backslash escape at 3")
  check("\\x{ABCD}", "", Java, Perl)(Backslash(Escape(HexBracket, 0xabcd)))
  check("\\x{ABCD}", "u", PCRE)(Backslash(Escape(HexBracket, 0xabcd)))
  check("\\z", "", DotNet, Java, PCRE, Perl, Python, Ruby)(Backslash(Assert(LowerEnd)))
  check("\\B", "", All*)(Backslash(Assert(NonBoundary(None))))
  check("\\B{g}", "", Perl)(Backslash(Assert(NonBoundary(Some(GModifier)))))
  check("\\B{gcb}", "", Perl)(Backslash(Assert(NonBoundary(Some(GcbModifier)))))
  check("\\B{lb}", "", Perl)(Backslash(Assert(NonBoundary(Some(LbModifier)))))
  check("\\B{wb}", "", Perl)(Backslash(Assert(NonBoundary(Some(WbModifier)))))
  check("\\B{sb}", "", Perl)(Backslash(Assert(NonBoundary(Some(SbModifier)))))
  check("\\D", "", All*)(Backslash(EscapeClass(NonDigit)))
  check("\\E", "", PCRE, Perl)(Backslash(CaseCommand(EndCaseCommand)))
  check("\\F", "", Perl)(Backslash(CaseCommand(FoldCaseCommand)))
  check("\\G", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(Assert(Sticky)))
  check("\\H", "", Java, PCRE, Perl)(Backslash(EscapeClass(NonHorizontal)))
  check("\\H", "", Ruby)(Backslash(EscapeClass(NonHexDigit)))
  check("\\K", "", PCRE, Perl, Ruby)(Backslash(Assert(Cut)))
  check("\\L", "", Perl)(Backslash(CaseCommand(LowerCaseCommand)))
  check("\\P{Letter}", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(EscapeClass(NonUnicodeProperty("Letter"))))
  check("\\P{Letter}", "u", JavaScript)(Backslash(EscapeClass(NonUnicodeProperty("Letter"))))
  check("\\P{Script=Hira}", "", DotNet, Java, PCRE, Perl, Ruby):
    Backslash(EscapeClass(NonUnicodePropertyValue("Script", "Hira")))
  check("\\P{Script=Hira}", "u", JavaScript)(Backslash(EscapeClass(NonUnicodePropertyValue("Script", "Hira"))))
  check("\\PL", "", Java, PCRE, Perl)(Backslash(EscapeClass(NonUnicodeBareProperty("L"))))
  check("\\Qab\\E", "", Java, PCRE, Perl):
    Sequence(
      Backslash(CaseCommand(QuoteCommand)),
      Literal('a'),
      Literal('b'),
      Backslash(CaseCommand(EndCaseCommand))
    )
  check("\\Qab", "", Java, PCRE, Perl):
    Sequence(
      Backslash(CaseCommand(QuoteCommand)),
      Literal('a'),
      Literal('b')
    )
  check("\\Q\\\\\\E", "", Java, PCRE, Perl):
    Sequence(
      Backslash(CaseCommand(QuoteCommand)),
      Literal('\\'),
      Literal('\\'),
      Backslash(CaseCommand(EndCaseCommand))
    )
  check("\\N", "", PCRE, Perl)(Backslash(EscapeClass(NonNewline)))
  error("\\N", "", Java, Python)("Invalid backslash escape at 2")
  check("\\N{2}", "", PCRE, Perl)(Repeat(Backslash(EscapeClass(NonNewline)), Exact(2, Greedy)))
  check("\\N{U+41}", "", Perl)(Backslash(Escape(NamedHex, 0x41)))
  check("\\N{U+41}", "u", PCRE)(Backslash(Escape(NamedHex, 0x41)))
  error("\\N{U+41}", "", PCRE)("Invalid backslash escape at 3")
  error("\\N{U+}", "", Perl)("Invalid backslash escape at 5")
  check("\\N{LATIN SMALL LETTER A}", "", Java, Perl, Python)(Backslash(EscapeClass(CharName("LATIN SMALL LETTER A"))))
  check("[\\N{LATIN SMALL LETTER A}]", "", Java, Perl, Python):
    Class(false, ClassBackslashClass(CharName("LATIN SMALL LETTER A")))
  error("\\N{}", "", Java, Perl, Python)("Invalid backslash escape at 3")
  check("\\R", "", Java, PCRE, Perl, Ruby)(Backslash(EscapeClass(GeneralNewline)))
  check("\\S", "", All*)(Backslash(EscapeClass(NonSpace)))
  check("\\U000ABCDE", "", Python)(Backslash(Escape(UnicodeHex8, 0xabcde)))
  check("\\V", "", Java, PCRE, Perl)(Backslash(EscapeClass(NonVertical)))
  check("\\W", "", All*)(Backslash(EscapeClass(NonWord)))
  check("\\X", "", Java, PCRE, Perl)(Backslash(EscapeClass(GraphemeCluster)))
  check("\\Z", "", DotNet, Java, PCRE, Perl, Python, Ruby)(Backslash(Assert(UpperEnd)))
  check("\\010", "", DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)(Backslash(Escape(BareOctal, 8)))
  check("\\0", "", DotNet, JavaScript, PCRE, Perl, Python, Ruby)(Backslash(Escape(BareOctal, 0)))
  error("\\0", "", Java)("Invalid octal escape at 2")
  check("\\0", "u", JavaScript)(Backslash(Escape(Single('0'), 0)))
  check("[\\0]", "v", JavaScript)(Class(false, ClassBackslashValue(Escape(Single('0'), 0))))
  error("\\00", "u", JavaScript)("Invalid backslash escape at 1")
  check("\\0477", "", Java)(Sequence(Backslash(Escape(BareOctal, 39)), Literal('7')))
  check("\\0377", "", Java)(Backslash(Escape(BareOctal, 0xff)))
  check("[\\8]", "", JavaScript, PCRE, Perl, Ruby)(Class(false, ClassBackslashValue(Unknown('8'))))
  error("[\\8]", "", DotNet, Java, Python)("Invalid backslash escape at 2")
  error("[\\1]", "", Java)("Invalid backslash escape at 2")
  check("[\\1]", "", DotNet, JavaScript, PCRE, Perl, Python, Ruby)(
    Class(false, ClassBackslashValue(Escape(BareOctal, 1)))
  )
  check("\\400", "", DotNet, Perl)(Backslash(Escape(BareOctal, 0x100)))
  check("\\400", "u", PCRE)(Backslash(Escape(BareOctal, 0x100)))
  error("\\400", "", PCRE, Python, Ruby)("Octal escape value too large at 1")
  check("\\10", "", DotNet, PCRE, Perl, Ruby)(Backslash(Escape(BareOctal, 8)))
  check("\\18", "", DotNet, PCRE, Perl, Ruby)(Sequence(Backslash(Escape(BareOctal, 1)), Literal('8')))
  check("\\123", "", Python)(Backslash(Escape(BareOctal, 83)))
  check("\\128", "", Python)(
    Sequence(Backslash(EscapeBackReference(BareBackReference, IndexedReference(12))), Literal('8'))
  )
  check("()\\11", "", Java):
    Sequence(
      Group(IndexedCapture, Sequence()),
      Backslash(EscapeBackReference(BareBackReference, IndexedReference(1))),
      Literal('1')
    )
  error("()\\2", "u", JavaScript)("Invalid back-reference at 3")
  check("\\2147483648", "", JavaScript):
    Sequence(
      Backslash(Escape(BareOctal, 0x8c)),
      Literal('7'),
      Literal('4'),
      Literal('8'),
      Literal('3'),
      Literal('6'),
      Literal('4'),
      Literal('8')
    )
  check("()\\99999999999", "", Ruby):
    Sequence(
      Group(IndexedCapture, Sequence()),
      Backslash(EscapeBackReference(BareBackReference, IndexedReference(Int.MaxValue)))
    )
  check("\\412", "", JavaScript)(Sequence(Backslash(Escape(BareOctal, 33)), Literal('2')))
  check("()\\1", "", All*):
    Sequence(Group(IndexedCapture, Sequence()), Backslash(EscapeBackReference(BareBackReference, IndexedReference(1))))
  check("\\1", "", DotNet, Java, PCRE, Perl, Python, Ruby):
    Backslash(EscapeBackReference(BareBackReference, IndexedReference(1)))
  error("\\1", "u", JavaScript)("Invalid back-reference at 1")
  for c <- "/|+*?{}()^$.\\[]" do check(s"\\$c", "", All*)(Backslash(Escape(Single(c), c.toInt)))
  check("\\y", "", JavaScript, Perl, Ruby)(Backslash(Unknown('y')))
  check("\\:", "", All*)(Backslash(Unknown(':')))
  check("\\_", "", Java, JavaScript, PCRE, Perl, Python, Ruby)(Backslash(Unknown('_')))
  error("\\_", "", DotNet)("Invalid backslash escape at 1")
  error("\\é", "", DotNet)("Invalid backslash escape at 1")
  error("\\:", "u", JavaScript)("Invalid backslash escape at 1")
  error("\\&", "u", JavaScript)("Invalid backslash escape at 1")
  error("\\-", "u", JavaScript)("Invalid backslash escape at 1")
  check("[\\-]", "u", JavaScript)(Class(false, ClassBackslashValue(Unknown('-'))))
  error("[\\:]", "u", JavaScript)("Invalid backslash escape at 2")
  check("[\\:]", "v", JavaScript)(Class(false, ClassBackslashValue(Unknown(':'))))
  check("[\\&]", "v", JavaScript)(Class(false, ClassBackslashValue(Escape(Single('&'), '&'))))
  error("\\E", "", Java)("Invalid backslash escape at 1")

  // Class
  check("[a]", "", All*)(Class(false, ClassLiteral('a')))
  check("[^a]", "", All*)(Class(true, ClassLiteral('a')))
  check("[a-z]", "", All*)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[a-]", "", All*)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'))))
  check("[-z]", "", All*)(Class(false, ClassUnion(ClassLiteral('-'), ClassLiteral('z'))))
  check("[\\n-\\r]", "", All*):
    Class(
      false,
      ClassRange(ClassBackslashValue(Escape(Single('n'), '\n')), ClassBackslashValue(Escape(Single('r'), '\r')))
    )
  check("[a-\\w]", "", JavaScript, Perl):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassBackslashClass(Word)))
  error("[a-\\w]", "", DotNet, Java, PCRE, Python, Ruby)("Invalid class range at 1")
  error("[a-\\w]", "u", JavaScript)("Invalid class range at 1")
  check("[\\w-z]", "", DotNet, Java, JavaScript, Perl):
    Class(false, ClassUnion(ClassBackslashClass(Word), ClassLiteral('-'), ClassLiteral('z')))
  error("[\\w-z]", "", PCRE, Python, Ruby)("Invalid class range at 1")
  error("[\\w-z]", "u", JavaScript)("Invalid class range at 1")
  check("[\\d-a-c]", "", JavaScript):
    Class(
      false,
      ClassUnion(ClassBackslashClass(Digit), ClassLiteral('-'), ClassLiteral('a'), ClassLiteral('-'), ClassLiteral('c'))
    )
  check("[\\d-a-c]", "", DotNet, Java, Perl):
    Class(
      false,
      ClassUnion(ClassBackslashClass(Digit), ClassLiteral('-'), ClassRange(ClassLiteral('a'), ClassLiteral('c')))
    )
  check("[a-[:digit:]]", "", Perl):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassPosix(false, "digit")))
  error("[a-[:digit:]]", "", PCRE, Ruby)("Invalid class range at 1")
  check("[[:digit:]-z]", "", Perl):
    Class(false, ClassUnion(ClassPosix(false, "digit"), ClassLiteral('-'), ClassLiteral('z')))
  error("[[:digit:]-z]", "", PCRE, Ruby)("Invalid class range at 1")
  check("[a-[b]]", "", Java, Ruby):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassNest(false, ClassLiteral('b'))))
  check("[[a]-z]", "", Java, Ruby):
    Class(false, ClassUnion(ClassNest(false, ClassLiteral('a')), ClassLiteral('-'), ClassLiteral('z')))
  error("[z-a]", "", All*)("Out of order class range at 1")
  error("[a-[b]]", "", JavaScript, PCRE, Perl, Python)("Out of order class range at 1")
  check("[\\b]", "", DotNet, JavaScript, PCRE, Perl, Python, Ruby)(
    Class(false, ClassBackslashValue(Escape(Single('b'), 0x08)))
  )
  error("[\\b]", "", Java)("Invalid backslash escape at 2")
  check("[\\A\\z\\Z\\G\\K\\R\\X]", "", Perl, Ruby):
    Class(false, ClassUnion("AzZGKRX".map(c => ClassBackslashValue(Unknown(c)))*))
  error("[\\A]", "", DotNet, Java, PCRE, Python)("Invalid escape sequence at 1")
  error("[\\R]", "", Java, PCRE)("Invalid escape sequence at 1")
  error("[\\N]", "", PCRE, Perl)("Invalid escape sequence at 1")
  check("[\\g\\k]", "", PCRE, Perl, Ruby)(
    Class(false, ClassUnion(ClassBackslashValue(Unknown('g')), ClassBackslashValue(Unknown('k'))))
  )
  check("[\\uA]", "", Perl)(Class(false, ClassUnion(ClassBackslashValue(Unknown('u')), ClassLiteral('A'))))
  check("[\\Qa]\\E]", "", Java, PCRE)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral(']'))))
  check("[a-\\Qz\\E]", "", Java, PCRE)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[\\Qa\\E-z]", "", Java, PCRE)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[\\Qxy\\E-\\Qza\\E]", "", Java, PCRE):
    Class(false, ClassUnion(ClassLiteral('x'), ClassRange(ClassLiteral('y'), ClassLiteral('z')), ClassLiteral('a')))
  error("[\\Qxz\\E-y]", "", Java, PCRE)("Out of order class range at 4")
  check("[a-\\Q\\Ez]", "", Java, PCRE)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[a-\\Q\\E]", "", Java, PCRE)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassUnion())))
  check("[\\Q\\E-z]", "", Java, PCRE)(Class(false, ClassUnion(ClassUnion(), ClassLiteral('-'), ClassLiteral('z'))))
  error("[a-\\Qz\\E]", "", Perl)("Out of order class range at 1")
  check("[\\Q]", "", Perl)(Class(false, ClassBackslashValue(Unknown('Q'))))
  check("[]", "", JavaScript)(Class(false, ClassUnion()))
  check("[[:alnum:]]", "", PCRE, Perl, Ruby)(Class(false, ClassPosix(false, "alnum")))
  check("[[:^alnum:]]", "", PCRE, Perl, Ruby)(Class(false, ClassPosix(true, "alnum")))
  error("[[:foo:]]", "", PCRE, Perl, Ruby)("Unknown POSIX class at 1")
  error("[[:^foo:]]", "", PCRE, Perl, Ruby)("Unknown POSIX class at 1")
  error("[[:FOO:]]", "", PCRE, Ruby)("Unknown POSIX class at 1")
  check("[[:FOO:]]", "", Perl):
    Sequence(
      Class(
        false,
        ClassUnion(
          ClassLiteral('['),
          ClassLiteral(':'),
          ClassLiteral('F'),
          ClassLiteral('O'),
          ClassLiteral('O'),
          ClassLiteral(':')
        )
      ),
      Literal(']')
    )
  error("[[:ab:]]", "", PCRE, Ruby)("Unknown POSIX class at 1")
  check("[[:ab:]]", "", Perl):
    Sequence(
      Class(
        false,
        ClassUnion(ClassLiteral('['), ClassLiteral(':'), ClassLiteral('a'), ClassLiteral('b'), ClassLiteral(':'))
      ),
      Literal(']')
    )
  error("[[:a:b:]]", "", PCRE, Perl)("Unknown POSIX class at 1")
  check("[[:a:b:]]", "", Ruby):
    Sequence(
      Class(
        false,
        ClassUnion(
          ClassLiteral('['),
          ClassLiteral(':'),
          ClassLiteral('a'),
          ClassLiteral(':'),
          ClassLiteral('b'),
          ClassLiteral(':')
        )
      ),
      Literal(']')
    )
  error("[[:alpha:b:]]", "", PCRE, Perl, Ruby)("Unknown POSIX class at 1")
  check("[[:]]", "", Ruby)(Class(false, ClassNest(false, ClassLiteral(':'))))
  check("[[:]]", "", PCRE, Perl):
    Sequence(Class(false, ClassUnion(ClassLiteral('['), ClassLiteral(':'))), Literal(']'))
  error("[[.a.]]", "", PCRE, Perl)("Unsupported POSIX collating element at 1")
  error("[[=a=]]", "", PCRE, Perl)("Unsupported POSIX collating element at 1")
  check("[[.a.]]", "", Ruby):
    Class(false, ClassNest(false, ClassUnion(ClassLiteral('.'), ClassLiteral('a'), ClassLiteral('.'))))
  error("[:alpha:]", "", PCRE)("POSIX class outside class at 0")
  error("[.a.]", "", PCRE)("Unsupported POSIX collating element at 0")
  check("[:a:]", "", DotNet, Java, JavaScript, Perl, Python, Ruby):
    Class(false, ClassUnion(ClassLiteral(':'), ClassLiteral('a'), ClassLiteral(':')))
  check("[[:<:]]", "", PCRE)(Class(false, ClassPosix(false, "<")))
  check("[[:>:]]", "", PCRE)(Class(false, ClassPosix(false, ">")))
  error("[[:<:]a]", "", PCRE)("Unknown POSIX class at 1")
  check("[[a]]", "", Java, Ruby)(Class(false, ClassNest(false, ClassLiteral('a'))))
  check("[[a]]", "v", JavaScript)(Class(false, ClassNest(false, ClassLiteral('a'))))
  check("[[^a]]", "", Java, Ruby)(Class(false, ClassNest(true, ClassLiteral('a'))))
  check("[[^a]]", "v", JavaScript)(Class(false, ClassNest(true, ClassLiteral('a'))))
  check("[[a-z]&&[a-c]]", "", Java, Ruby):
    Class(
      false,
      ClassIntersection(
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))),
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('c')))
      )
    )
  check("[[a-z]&&[a-c]]", "v", JavaScript):
    Class(
      false,
      ClassIntersection(
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))),
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('c')))
      )
    )
  check("[[a-z]--[a-c]]", "v", JavaScript):
    Class(
      false,
      ClassDiff(
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))),
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('c')))
      )
    )
  check("[a b]", "x", Java)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('b'))))
  check("[a b]", "x", DotNet, PCRE, Perl, Python, Ruby):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral(' '), ClassLiteral('b')))
  check("[a - z]", "x", Java)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[a#x\nb]", "x", Java)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('b'))))
  check("[ ]]", "x", Java)(Class(false, ClassLiteral(']')))
  error("[ ]", "x", Java)("Unclosed ']' at 3")
  error("[a- ]", "x", Java)("Out of order class range at 1")
  check("[a&&b]", "", DotNet, JavaScript, PCRE, Perl, Python):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('&'), ClassLiteral('&'), ClassLiteral('b')))
  check("[[a]", "", DotNet, JavaScript, PCRE, Perl, Python)(
    Class(false, ClassUnion(ClassLiteral('['), ClassLiteral('a')))
  )
  check("[!--b]", "", All*)(
    Class(false, ClassUnion(ClassRange(ClassLiteral('!'), ClassLiteral('-')), ClassLiteral('b')))
  )
  error("[a--b]", "", All*)("Out of order class range at 1")
  check("[&&a]", "", Java, Ruby)(Class(false, ClassIntersection(ClassUnion(), ClassLiteral('a'))))
  check("[a&&]", "", Java, Ruby)(Class(false, ClassIntersection(ClassLiteral('a'), ClassUnion())))
  check("[&&]", "", Ruby)(Class(false, ClassIntersection(ClassUnion(), ClassUnion())))
  error("[&&]", "", Java)("Invalid class set operation at 1")
  check("[&&]", "", DotNet, JavaScript, PCRE, Perl, Python):
    Class(false, ClassUnion(ClassLiteral('&'), ClassLiteral('&')))
  check("[a-z-[aeiou]]", "", DotNet):
    Class(
      false,
      ClassDiff(
        ClassRange(ClassLiteral('a'), ClassLiteral('z')),
        ClassNest(
          false,
          ClassUnion(ClassLiteral('a'), ClassLiteral('e'), ClassLiteral('i'), ClassLiteral('o'), ClassLiteral('u'))
        )
      )
    )
  check("[a-[^b]]", "", DotNet)(Class(false, ClassDiff(ClassLiteral('a'), ClassNest(true, ClassLiteral('b')))))
  check("[\\w-[\\d]]", "", DotNet):
    Class(false, ClassDiff(ClassBackslashClass(Word), ClassNest(false, ClassBackslashClass(Digit))))
  check("[]-[a]]", "", DotNet)(Class(false, ClassDiff(ClassLiteral(']'), ClassNest(false, ClassLiteral('a')))))
  check("[a-[b-[c]]]", "", DotNet):
    Class(
      false,
      ClassDiff(ClassLiteral('a'), ClassNest(false, ClassDiff(ClassLiteral('b'), ClassNest(false, ClassLiteral('c')))))
    )
  check("[-[a]]", "", DotNet):
    Sequence(Class(false, ClassUnion(ClassLiteral('-'), ClassLiteral('['), ClassLiteral('a'))), Literal(']'))
  error("[a-z-[aeiou]b]", "", DotNet)("Class subtraction must be last at 12")
  error("[\\w-[\\d]]", "", PCRE, Python, Ruby)("Invalid class range at 1")
  check("[a&&b]", "v", JavaScript)(Class(false, ClassIntersection(ClassLiteral('a'), ClassLiteral('b'))))
  check("[a--b--c]", "v", JavaScript):
    Class(false, ClassDiff(ClassDiff(ClassLiteral('a'), ClassLiteral('b')), ClassLiteral('c')))
  error("[ab&&c]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a&&bc]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a&&b--c]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a-z&&b]", "v", JavaScript)("Invalid class set operation at 1")
  error("[&&a]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a--]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a&&&b]", "v", JavaScript)("Invalid class set character at 4")
  error("[a!!b]", "v", JavaScript)("Invalid class set character at 2")
  check("[a!b]", "v", JavaScript)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('!'), ClassLiteral('b'))))
  error("[(]", "v", JavaScript)("Invalid class set character at 1")
  error("[a-]", "v", JavaScript)("Invalid class set character at 2")
  error("[-a]", "v", JavaScript)("Invalid class set character at 1")
  error("[a-[b]]", "v", JavaScript)("Invalid class set character at 2")
  error("[\\w-a]", "v", JavaScript)("Invalid class range at 1")
  error("[^\\q{ab}]", "v", JavaScript)("Negated class may contain strings at 0")
  error("[^[\\q{ab}]]", "v", JavaScript)("Negated class may contain strings at 0")
  error("[[^\\q{ab}]]", "v", JavaScript)("Negated class may contain strings at 1")
  check("[^\\q{a}]", "v", JavaScript)(Class(true, ClassBackslashClass(QuoteSet(Seq(Seq(QuoteValue('a')))))))
  check("[^\\q{ab}&&a]", "v", JavaScript):
    Class(
      true,
      ClassIntersection(
        ClassBackslashClass(QuoteSet(Seq(Seq(QuoteValue('a'), QuoteValue('b'))))),
        ClassLiteral('a')
      )
    )
  check("[\\q{abc|def}]", "v", JavaScript):
    Class(
      false,
      ClassBackslashClass(
        QuoteSet(
          Seq(
            Seq(QuoteValue('a'), QuoteValue('b'), QuoteValue('c')),
            Seq(QuoteValue('d'), QuoteValue('e'), QuoteValue('f'))
          )
        )
      )
    )
  check("[\\q{\\n}]", "v", JavaScript):
    Class(
      false,
      ClassBackslashClass(
        QuoteSet(
          Seq(
            Seq(QuoteBackslash(Escape(Single('n'), 0x0a)))
          )
        )
      )
    )
  error("[\\q{(}]", "v", JavaScript)("Invalid set operation at 4")
  error("[\\q{a!!}]", "v", JavaScript)("Invalid set operation at 5")
  check("[\\q{\\0\\(}]", "v", JavaScript):
    Class(
      false,
      ClassBackslashClass(
        QuoteSet(Seq(Seq(QuoteBackslash(Escape(Single('0'), 0)), QuoteBackslash(Escape(Single('('), '(')))))
      )
    )
  error("[", "", All*)("Unclosed ']' at 1")
  check("]", "", All*)(Literal(']'))
  error("[[a]", "", Java)("Unclosed ']' at 4")
  error("]", "u", JavaScript)("Unclosed ']' at 0")
  error("[[a]", "", Java, Ruby)("Unclosed ']' at 4")
  error("[[]", "", Java, Ruby)("Unclosed ']' at 3")
  error("[^[]a]", "", Java, Ruby)("Unclosed ']' at 6")
  error("[[]", "v", JavaScript)("Unclosed ']' at 3")
