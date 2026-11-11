package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.AssertNameStyle.*
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.CommandKind.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.GroupKind.*
import codes.quine.labs.resyntax.ast.NameStyle.*
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.dialect.Dialect.*

class ParserGroupSuite extends munit.FunSuite with ParserHelper:

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
  // The non-ASCII names are checked with .NET 10.0.14 and Perl 5.44.0.
  check("(?<a\u200d>a)", "", DotNet)(Group(NamedCapture(Angle, "a\u200d"), Literal('a')))
  // U+A7CE is a letter of Unicode 17.
  check("(?<\ua7ce>a)", "", Perl())(Group(NamedCapture(Angle, "\ua7ce"), Literal('a')))
  // Perl takes `XID_Start` and `XID_Continue` restricted to `\w`.
  error("(?<\u0e33>a)", "", Perl())("Invalid identifier at 3")
  error("(?<\u2118>a)", "", Perl())("Invalid identifier at 3")
  error("(?<a\u24b6>a)", "", Perl())("Invalid identifier at 4")
  check("(?<a\u0e33>a)", "", Perl())(Group(NamedCapture(Angle, "a\u0e33"), Literal('a')))
  // Perl's byte string takes only ASCII names.
  error("(?<\u00e9>a)", "", Perl(utf8 = false))("Invalid identifier at 3")
  check("(?<\ud835\udc65>a)", "", Perl())(Group(NamedCapture(Angle, "\ud835\udc65"), Literal('a')))
  // The escapes in a group name are not kept.
  checkPrinted("(?<\\u{41}\\u0042>a)", "(?<AB>a)", "", JavaScript)(Group(NamedCapture(Angle, "AB"), Literal('a')))
  checkPrinted("(?<\\ud835\\udc65>a)", "(?<\ud835\udc65>a)", "", JavaScript):
    Group(NamedCapture(Angle, "\ud835\udc65"), Literal('a'))
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

  // Java skips whitespace and comments in a group's opening except right after `(?` (checked with Java 27.0.1).
  // They are kept in the flags and in the body of a capture group, but not in the others.
  check("( a)", "x", Java)(Group(IndexedCapture, Sequence(Blank(" "), Literal('a'))))
  check("( |^)", "x", Java)(Group(IndexedCapture, Disjunction(Blank(" "), Caret)))
  check("(#c\n)", "x", Java)(Group(IndexedCapture, Blank("#c\n")))
  checkPrinted("( ?:a)", "(?:a)", "x", Java)(Group(NonCapture, Literal('a')))
  check("(? :a)", "x", Java)(Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), None)), Literal('a')))
  check("(?#c\n:a)", "x", Java)(Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), None, Some("#c\n"))), Literal('a')))
  error("(? =a)", "x", Java)("Invalid flag at 3")
  error("(?#x)", "", Java)("Invalid flag at 2")
  checkPrinted("(?< =a)", "(?<=a)", "x", Java)(Group(PositiveLookBehind(Symbolic), Literal('a')))
  checkPrinted("(?<n #c\na >a)", "(?<na>a)", "x", Java)(Group(NamedCapture(Angle, "na"), Literal('a')))
  error("(", "", All*)("Unclosed group at 1")

  error("(?Px)", "", PCRE, Python())("Invalid group at 3")
  // PCRE in UTF mode takes letters and digits in a group name, and Java only ASCII ones.
  check("(?<a\u00e9>x)", "u", PCRE)(Group(NamedCapture(Angle, "a\u00e9"), Literal('x')))
  error("(?<a\u00e9>x)", "", Java)("Invalid identifier at 4")
