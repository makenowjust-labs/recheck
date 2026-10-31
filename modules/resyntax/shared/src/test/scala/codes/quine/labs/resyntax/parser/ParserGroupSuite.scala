package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.AssertNameStyle.*
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.CommandKind.*
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind.*
import codes.quine.labs.resyntax.ast.NameStyle.*
import codes.quine.labs.resyntax.ast.NodeData.*

class ParserGroupSuite extends ParserSuiteBase:
  // Group
  check("(?:a)", "", All*)(Group(NonCapture, Literal('a')))
  check("(?|a)", "", PCRE, Perl())(Command(BranchReset(Literal('a'))))
  check("(?|a|b)", "", PCRE, Perl())(Command(BranchReset(Literal('a'), Literal('b'))))
  check("(?=a)", "", All*)(Group(PositiveLookAhead(Symbolic), Literal('a')))
  check("(?!a)", "", All*)(Group(NegativeLookAhead(Symbolic), Literal('a')))
  check("(?<=a)", "", All*)(Group(PositiveLookBehind(Symbolic), Literal('a')))
  check("(?<!a)", "", All*)(Group(NegativeLookBehind(Symbolic), Literal('a')))
  check("(?<*a)", "", PCRE)(Group(NonAtomicPositiveLookBehind(Symbolic), Literal('a')))
  check("(?<x>a)", "", DotNet, Java, JavaScript, PCRE, Perl(), Ruby())(Group(NamedCapture(Angle, "x"), Literal('a')))
  check("(?<a_1>a)", "", DotNet, JavaScript, PCRE, Perl(), Ruby())(Group(NamedCapture(Angle, "a_1"), Literal('a')))
  error("(?<a_1>a)", "", Java)("Invalid identifier at 4")
  check("(?<_>a)", "", DotNet, JavaScript, PCRE, Perl(), Ruby())(Group(NamedCapture(Angle, "_"), Literal('a')))
  error("(?<_>a)", "", Java)("Invalid identifier at 3")
  check("(?<$>a)", "", JavaScript, Ruby())(Group(NamedCapture(Angle, "$"), Literal('a')))
  error("(?<$>a)", "", DotNet, Java, PCRE, Perl())("Invalid identifier at 3")
  error("(?<1a>a)", "", Java, JavaScript, PCRE, Perl(), Ruby())("Invalid identifier at 3")
  error("(?<1a>a)", "", DotNet)("Invalid identifier at 4")
  check("(?<1>a)", "", DotNet)(Group(NamedCapture(Angle, "1"), Literal('a')))
  error("(?<0>a)", "", DotNet)("Invalid identifier at 3")
  check("(?<é>a)", "", DotNet, JavaScript, Perl(), Ruby())(Group(NamedCapture(Angle, "é"), Literal('a')))
  check("(?<é>a)", "u", PCRE)(Group(NamedCapture(Angle, "é"), Literal('a')))
  error("(?<é>a)", "", Java, PCRE)("Invalid identifier at 3")
  check("(?<\u2170>a)", "", Perl())(Group(NamedCapture(Angle, "\u2170"), Literal('a')))
  error("(?<\u2170>a)", "", DotNet)("Invalid identifier at 3")
  check("(?<\u203f>a)", "", DotNet)(Group(NamedCapture(Angle, "\u203f"), Literal('a')))
  error("(?<\u203f>a)", "", Perl())("Invalid identifier at 3")
  check("(?<\u0300>a)", "", DotNet)(Group(NamedCapture(Angle, "\u0300"), Literal('a')))
  check("(?<\u0661>a)", "", DotNet)(Group(NamedCapture(Angle, "\u0661"), Literal('a')))
  error("(?<\u0661>a)", "", Perl())("Invalid identifier at 3")
  check("(?<a\u0903\u200d>a)", "", Perl())(Group(NamedCapture(Angle, "a\u0903\u200d"), Literal('a')))
  error("(?<a\u0903>a)", "", DotNet)("Invalid identifier at 4")
  error("(?<a\u00b7>a)", "", DotNet, Perl())("Invalid identifier at 4")
  check("(?<\ud835\udc65>a)", "", Perl())(Group(NamedCapture(Angle, "\ud835\udc65"), Literal('a')))
  check("(?<\\u{41}\\u0042>a)", "", JavaScript)(Group(NamedCapture(Angle, "AB"), Literal('a')))
  check("(?<\\ud835\\udc65>a)", "", JavaScript)(Group(NamedCapture(Angle, "\ud835\udc65"), Literal('a')))
  error("(?<\\u0031>a)", "", JavaScript)("Invalid identifier at 3")
  check("(?<a-b>a)", "", Ruby())(Group(NamedCapture(Angle, "a-b"), Literal('a')))
  check("(?<\\w+[>a)", "", Ruby())(Group(NamedCapture(Angle, "\\w+["), Literal('a')))
  error("(?<-a>a)", "", Ruby())("Invalid identifier at 3")
  error("(?<\u0661>a)", "", Ruby())("Invalid identifier at 3")
  check("(?<a\u0661>a)", "", Ruby())(Group(NamedCapture(Angle, "a\u0661"), Literal('a')))
  error("(?<a)>a)", "", Ruby())("Invalid identifier at 4")
  check("(?<)>a)", "", Ruby())(Group(NamedCapture(Angle, ")"), Literal('a')))
  check(s"(?<${"a" * 128}>a)", "", PCRE)(Group(NamedCapture(Angle, "a" * 128), Literal('a')))
  error(s"(?<${"a" * 129}>a)", "", PCRE)("Invalid identifier at 3")
  check("(?P<é>a)", "", Python())(Group(PNamedCapture("é"), Literal('a')))
  error("(?P<$>a)", "", PCRE, Perl(), Python())("Invalid identifier at 4")
  check("(?<x>a)(?<x>b)", "", DotNet, Perl(), Ruby()):
    Sequence(Group(NamedCapture(Angle, "x"), Literal('a')), Group(NamedCapture(Angle, "x"), Literal('b')))
  check("(?<x>a)(?<x>b)", "J", PCRE):
    Sequence(Group(NamedCapture(Angle, "x"), Literal('a')), Group(NamedCapture(Angle, "x"), Literal('b')))
  error("(?<x>a)(?<x>b)", "", Java, JavaScript, PCRE)("Duplicated group name at 7")
  error("(?P<x>a)(?P<x>b)", "", PCRE, Python())("Duplicated group name at 8")
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
  check("(?|(?<x>a)|(?<x>b))", "", PCRE, Perl()):
    Command(BranchReset(Group(NamedCapture(Angle, "x"), Literal('a')), Group(NamedCapture(Angle, "x"), Literal('b'))))
  error("(?|(?<x>a)|(?<x>b))(?<x>c)", "", PCRE)("Duplicated group name at 19")
  check("(?<x-y>a)", "", DotNet)(Group(Balance(Angle, Some("x"), "y"), Literal('a')))
  check("(?<x-1>a)", "", DotNet)(Group(Balance(Angle, Some("x"), "1"), Literal('a')))
  check("(?<-y>a)", "", DotNet)(Group(Balance(Angle, None, "y"), Literal('a')))
  check("(?>a)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Group(Atomic(Symbolic), Literal('a')))
  check("(?*a)", "", PCRE)(Group(NonAtomicPositiveLookAhead(Symbolic), Literal('a')))
  check("(?~a)", "", Ruby())(Group(Absence, Literal('a')))
  check("(?~|a|b|c)", "", Ruby())(Command(AbsenceExpression(Literal('a'), Disjunction(Literal('b'), Literal('c')))))
  check("(?~|a)", "", Ruby())(Command(AbsenceStopper(Literal('a'))))
  check("(?~|)", "", Ruby())(Command(AbsenceRangeClear))
  error("(?~|a)", "", DotNet, Java, JavaScript, PCRE, Perl(), Python())("Invalid group at 2")

  check("(a)", "", All*)(Group(IndexedCapture, Literal('a')))
  error("(", "", All*)("Unclosed group at 1")
