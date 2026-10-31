package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.AssertKind.*
import codes.quine.labs.resyntax.ast.BackslashKind.*
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.CommandKind.Comment
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.EscapeClassKind.*
import codes.quine.labs.resyntax.ast.EscapeStyle.*
import codes.quine.labs.resyntax.ast.GroupKind.*
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.Quantifier.*

class ParserSuite extends ParserSuiteBase:
  // Top
  error(")", "", All*)("Unmatched ')' at 0")

  // Comment
  check(" ", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Sequence())
  check("# foo", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Sequence())
  check("# \\\nfoo", "x", Python())(Sequence())

  // Disjunction:
  check("a|b", "", All*)(Disjunction(Literal('a'), Literal('b')))

  // Sequence:
  check("ab", "", All*)(Sequence(Literal('a'), Literal('b')))
  check("\uD83D\uDE00", "", Java, Perl(), Python(), Ruby())(Literal(0x1f600))
  check("\uD83D\uDE00", "", DotNet, JavaScript)(Sequence(Literal(0xd83d), Literal(0xde00)))
  check("\uD83D\uDE00", "u", PCRE)(Literal(0x1f600))
  check("\uD83D\uDE00", "", PCRE)(Sequence(Literal(0xf0), Literal(0x9f), Literal(0x98), Literal(0x80)))

  // Repeat:
  check("a*", "", All*)(Repeat(Literal('a'), Star(Greedy)))
  check("a*?", "", All*)(Repeat(Literal('a'), Star(Lazy)))
  check("a*+", "", Java, PCRE, Perl(), Python(), Ruby())(Repeat(Literal('a'), Star(Possessive)))
  check("a+", "", All*)(Repeat(Literal('a'), Plus(Greedy)))
  check("a+?", "", All*)(Repeat(Literal('a'), Plus(Lazy)))
  check("a++", "", Java, PCRE, Perl(), Python(), Ruby())(Repeat(Literal('a'), Plus(Possessive)))
  check("a?", "", All*)(Repeat(Literal('a'), Question(Greedy)))
  check("a??", "", All*)(Repeat(Literal('a'), Question(Lazy)))
  check("a?+", "", Java, PCRE, Perl(), Python(), Ruby())(Repeat(Literal('a'), Question(Possessive)))
  check("a{2}", "", All*)(Repeat(Literal('a'), Exact(2, Greedy)))
  check("a{2}?", "", DotNet, Java, JavaScript, PCRE, Perl(), Python())(Repeat(Literal('a'), Exact(2, Lazy)))
  check("a{2}?", "", Ruby())(Repeat(Repeat(Literal('a'), Exact(2, Greedy)), Question(Greedy)))
  check("a{2}+", "", Java, PCRE, Perl(), Python())(Repeat(Literal('a'), Exact(2, Possessive)))
  check("a{2}+", "", Ruby())(Repeat(Repeat(Literal('a'), Exact(2, Greedy)), Plus(Greedy)))
  check("a{2,}", "", All*)(Repeat(Literal('a'), Unbounded(2, Greedy)))
  check("a{2,}?", "", All*)(Repeat(Literal('a'), Unbounded(2, Lazy)))
  check("a{2,}+", "", Java, PCRE, Perl(), Python())(Repeat(Literal('a'), Unbounded(2, Possessive)))
  check("a{2,}+", "", Ruby())(Repeat(Repeat(Literal('a'), Unbounded(2, Greedy)), Plus(Greedy)))
  check("a{2,3}", "", All*)(Repeat(Literal('a'), Bounded(2, 3, Greedy)))
  check("a{2,3}?", "", All*)(Repeat(Literal('a'), Bounded(2, 3, Lazy)))
  check("a{2,3}+", "", Java, PCRE, Perl(), Python())(Repeat(Literal('a'), Bounded(2, 3, Possessive)))
  check("a{2,3}+", "", Ruby())(Repeat(Repeat(Literal('a'), Bounded(2, 3, Greedy)), Plus(Greedy)))
  check("a{65535}", "", All*)(Repeat(Literal('a'), Exact(65535, Greedy)))
  error("a{65536}", "", PCRE)("Number too big in {} quantifier at 2")
  check("a{,100000}", "", Ruby())(Repeat(Literal('a'), MaxBounded(100000, Greedy)))
  error("a{,100001}", "", Ruby())("Number too big in {} quantifier at 3")
  check("a{2147483646}", "", DotNet, Java, JavaScript, Perl(), Python())(
    Repeat(Literal('a'), Exact(2147483646, Greedy))
  )
  error("a{2147483647}", "", Perl())("Number too big in {} quantifier at 2")
  check("a{1,2147483647}", "", DotNet, Java, JavaScript, Python())(Repeat(Literal('a'), Bounded(1, 2147483647, Greedy)))
  error("a{1,2147483648}", "", DotNet, Java)("Number too big in {} quantifier at 4")
  check("a{4294967294}", "", JavaScript, Python())(Repeat(Literal('a'), Exact(Int.MaxValue, Greedy)))
  error("a{4294967295}", "", Python())("Number too big in {} quantifier at 2")
  check("a{99999999999999999999}", "", JavaScript)(Repeat(Literal('a'), Exact(Int.MaxValue, Greedy)))
  check("a{3,2}", "", Perl())(Repeat(Literal('a'), Bounded(3, 2, Greedy)))
  error("a{3,2}", "", DotNet, Java, JavaScript, PCRE, Python(), Ruby())("Numbers out of order in {} quantifier at 1")
  check("a{,3}", "", PCRE, Perl(), Python(), Ruby())(Repeat(Literal('a'), MaxBounded(3, Greedy)))
  check("a{,3}?", "", PCRE, Perl(), Python(), Ruby())(Repeat(Literal('a'), MaxBounded(3, Lazy)))
  check("a{,3}+", "", PCRE, Perl(), Python())(Repeat(Literal('a'), MaxBounded(3, Possessive)))
  check("a{,3}+", "", Ruby())(Repeat(Repeat(Literal('a'), MaxBounded(3, Greedy)), Plus(Greedy)))
  check("a{", "", DotNet, JavaScript, PCRE, Perl(), Python(), Ruby())(Sequence(Literal('a'), Literal('{')))
  check("a{,3}", "", DotNet, JavaScript)(Sequence(Literal('a'), Literal('{'), Literal(','), Literal('3'), Literal('}')))
  check("a{,}", "", DotNet, JavaScript, PCRE, Perl(), Python(), Ruby()):
    Sequence(Literal('a'), Literal('{'), Literal(','), Literal('}'))
  check("a{,3x", "", PCRE, Perl(), Python(), Ruby()):
    Sequence(Literal('a'), Literal('{'), Literal(','), Literal('3'), Literal('x'))
  check("a{ 2 }", "", PCRE, Perl())(Repeat(Literal('a'), Exact(2, Greedy)))
  check("a{ 2 , 3 }", "", PCRE, Perl())(Repeat(Literal('a'), Bounded(2, 3, Greedy)))
  check("a{\t2,\t}", "", PCRE, Perl())(Repeat(Literal('a'), Unbounded(2, Greedy)))
  check("a{ ,3}", "", PCRE, Perl())(Repeat(Literal('a'), MaxBounded(3, Greedy)))
  check("a{ 2}", "", DotNet, JavaScript, Python(), Ruby()):
    Sequence(Literal('a'), Literal('{'), Literal(' '), Literal('2'), Literal('}'))
  error("a{", "", Java)("Incomplete quantifier at 1")
  check("\\d{", "", DotNet, JavaScript, PCRE, Python(), Ruby())(Sequence(Backslash(EscapeClass(Digit)), Literal('{')))
  error("\\d{", "", Perl())("Unescaped left brace at 2")
  // Perl's lexer removes the commands of a regex literal, so the regex compiler does not see them before `{`.
  check("\\E{x", "", Perl())(Sequence(Literal('{'), Literal('x')))
  check("\\u{", "", Perl())(Literal('{'))
  check("a\\u{3}", "", Perl())(Repeat(Literal('a'), Exact(3, Greedy)))
  errorPerlString("\\E{x", "")("Unescaped left brace at 2")
  errorPerlString("\\u{", "")("Unescaped left brace at 2")
  error("\\\\p{x", "", Perl())("Unescaped left brace at 3")
  error("a\\c\\p{x", "", Perl())("Unescaped left brace at 5")
  check("\\\\pa{", "", Perl()):
    Sequence(Backslash(Escape(Single('\\'), '\\')), Literal('p'), Literal('a'), Literal('{'))
  check("\\\\p{", "", PCRE)(Sequence(Backslash(Escape(Single('\\'), '\\')), Literal('p'), Literal('{')))
  check("\\x41{", "", Perl())(Sequence(Backslash(Escape(Hex2, 0x41)), Literal('{')))
  check("\\-{", "", Perl())(Sequence(Backslash(Unknown('-')), Literal('{')))
  check("\\d{2}", "", All*)(Repeat(Backslash(EscapeClass(Digit)), Exact(2, Greedy)))
  error("a{", "u", JavaScript)("Incomplete quantifier at 1")
  error("*", "", All*)("Nothing to repeat at 0")
  check("a**", "", Ruby())(Repeat(Repeat(Literal('a'), Star(Greedy)), Star(Greedy)))
  error("a**", "", DotNet, Java, JavaScript, PCRE, Perl(), Python())("Nested quantifier at 2")
  error("a*++", "", Java, PCRE, Perl(), Python())("Nested quantifier at 3")
  check("a{2}{3}", "", Java)(Repeat(Literal('a'), Exact(2, Greedy)))
  check("a*?{2,}+", "", Java)(Repeat(Literal('a'), Star(Lazy)))
  error("a{2}{3}", "", DotNet, JavaScript, PCRE, Perl(), Python())("Nested quantifier at 7")
  error("a{2}*", "", Java)("Nested quantifier at 4")
  check("({1})", "", Java)(Group(IndexedCapture, Sequence()))
  check("({1})", "", Perl())(Group(IndexedCapture, Sequence(Literal('{'), Literal('1'), Literal('}'))))
  error("({1})", "", DotNet, JavaScript, PCRE, Python(), Ruby())("Nothing to repeat at 4")
  error("a*+", "", DotNet, JavaScript)("Nested quantifier at 2")
  check("^*", "", DotNet, Java, Perl(), Ruby())(Repeat(Caret, Star(Greedy)))
  error("^*", "", JavaScript, PCRE, Python())("Nothing to repeat at 1")
  check("$+", "", DotNet, Java, Perl(), Ruby())(Repeat(Dollar, Plus(Greedy)))
  error("$*", "", JavaScript, PCRE, Python())("Nothing to repeat at 1")
  check("\\b?", "", DotNet, Java, Perl(), Ruby())(Repeat(Backslash(Assert(Boundary(None))), Question(Greedy)))
  error("\\b?", "", JavaScript, PCRE, Python())("Nothing to repeat at 2")
  check("\\A{2}", "", DotNet, Java, Perl(), Ruby())(Repeat(Backslash(Assert(Begin)), Exact(2, Greedy)))
  error("\\A{2}", "", PCRE, Python())("Nothing to repeat at 5")
  error("(?=a)*", "u", JavaScript)("Nothing to repeat at 5")
  error("(?<=a)*", "", JavaScript)("Nothing to repeat at 6")
  check("}", "", All*)(Literal('}'))
  error("}", "u", JavaScript)("Incomplete quantifier at 0")
  check("a(?#x)*", "", DotNet, PCRE, Perl(), Python(), Ruby())(Repeat(Literal('a'), Star(Greedy)))
  check("a(?#x)(?#y)+", "", DotNet, PCRE, Perl(), Python(), Ruby())(Repeat(Literal('a'), Plus(Greedy)))
  check("a(?#x){2}", "", DotNet, PCRE, Perl(), Python(), Ruby())(Repeat(Literal('a'), Exact(2, Greedy)))
  check("a(?#x)b", "", DotNet, PCRE, Perl(), Python(), Ruby())(
    Sequence(Literal('a'), Command(Comment("x")), Literal('b'))
  )
  error("(?#x)*", "", DotNet, PCRE, Perl(), Python(), Ruby())("Nothing to repeat at 5")
  error("(?#x){2}", "", DotNet, PCRE, Python(), Ruby())("Nothing to repeat at 8")
  check("(?#x){2}", "", Perl())(Sequence(Command(Comment("x")), Literal('{'), Literal('2'), Literal('}')))
  error("a(?i)(?#x)*", "", DotNet, PCRE, Perl(), Ruby())("Nothing to repeat at 10")
  check("a+(?#x)?", "", DotNet, PCRE, Perl(), Ruby())(Repeat(Literal('a'), Plus(Lazy)))
  error("a+(?#x)?", "", Python())("Nested quantifier at 7")
  check("a+(?#x)+", "", PCRE, Perl(), Ruby())(Repeat(Literal('a'), Plus(Possessive)))
  error("a+(?#x)+", "", DotNet)("Nested quantifier at 7")
  check("a{2}(?#x)?", "", DotNet, PCRE, Perl())(Repeat(Literal('a'), Exact(2, Lazy)))
  check("a{2}(?#x)?", "", Ruby())(Repeat(Repeat(Literal('a'), Exact(2, Greedy)), Question(Greedy)))
  error("a{2}(?#x)?", "", Python())("Nested quantifier at 9")
  check("a+ ?", "x", DotNet, Java, PCRE, Perl())(Repeat(Literal('a'), Plus(Lazy)))
  check("a+ #x\n?", "x", DotNet, Java, PCRE, Perl())(Repeat(Literal('a'), Plus(Lazy)))
  check("a{1,2} ?", "x", DotNet, Java, PCRE, Perl())(Repeat(Literal('a'), Bounded(1, 2, Lazy)))
  check("a+ ?", "x", Ruby())(Repeat(Repeat(Literal('a'), Plus(Greedy)), Question(Greedy)))
  error("a+ ?", "x", Python())("Nested quantifier at 3")

  // Caret
  check("^", "", All*)(Caret)

  // Dollar
  check("$", "", All*)(Dollar)

  // Dot
  check(".", "", All*)(Dot)
