package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.AssertKind.*
import codes.quine.labs.resyntax.ast.BackslashKind.*
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.CommandKind.Comment
import codes.quine.labs.resyntax.ast.CommandKind.InlineFlag
import codes.quine.labs.resyntax.ast.CommandKind.StartOption
import codes.quine.labs.resyntax.ast.EscapeClassKind.*
import codes.quine.labs.resyntax.ast.EscapeStyle.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind.*
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.Quantifier.*
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect.*

class ParserSuite extends munit.FunSuite with ParserHelper:

  // Top
  error(")", "", All*)("Unmatched ')' at 0")

  // Comment
  check(" ", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Sequence())
  check("# foo", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Sequence())
  check("# \\\nfoo", "x", Python())(Sequence())
  // Checked with Python 3.15.2.
  error("#\\", "x", Python())("Invalid backslash escape at 1")
  // The whitespace and the comment ends are checked with .NET 10.0.14, Java 27.0.1, PHP 8.5.12 with PCRE2 10.49,
  // Perl 5.44.0, Python 3.15.2 and Ruby 4.0.9.
  check("a\t\n\f\r b", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Sequence(Literal('a'), Literal('b')))
  check("a\u000bb", "x", Java, PCRE, Perl(), Python())(Sequence(Literal('a'), Literal('b')))
  check("a\u000bb", "x", DotNet, Ruby())(Sequence(Literal('a'), Literal(0x0b), Literal('b')))
  check("\u00a0*", "x", DotNet, Java, Perl(), Python(), Ruby())(Repeat(Literal(0xa0), Star(Greedy)))
  check("\u00a0*", "xu", PCRE)(Repeat(Literal(0xa0), Star(Greedy)))
  check("a\u0085\u200e\u200f\u2028\u2029b", "x", Perl())(Sequence(Literal('a'), Literal('b')))
  check("a\u0085\u200e\u200f\u2028\u2029b", "xu", PCRE)(Sequence(Literal('a'), Literal('b')))
  check("a\u2028b", "x", DotNet, Java, Python(), Ruby())(Sequence(Literal('a'), Literal(0x2028), Literal('b')))
  check("\u0085*", "x", DotNet, Java, Python(), Ruby())(Repeat(Literal(0x85), Star(Greedy)))
  error("\u0085*", "x", Perl())("Nothing to repeat at 1")
  error("\u0085*", "xu", PCRE)("Nothing to repeat at 1")
  // In a byte mode, U+0085 is the bytes C2 85, and the byte 85 is whitespace.
  check("\u0085*", "x", PCRE, Perl(utf8 = false))(Repeat(Literal(0xc2), Star(Greedy)))
  check("#c\r)", "x", DotNet, PCRE, Perl(), Python(), Ruby())(Sequence())
  error("#c\r)", "x", Java)("Unmatched ')' at 3")
  check("#c\r)", "xd", Java)(Sequence())
  check("(?d)#c\r)", "x", Java)(Command(InlineFlag(FlagSetDiff(FlagSet(hasIndices = true), None))))
  error("#c\u0085)", "x", Java)("Unmatched ')' at 3")
  check("#c\u2028*", "x", Java)(Repeat(Literal(0x2028), Star(Greedy)))
  check("#c\u0000a", "x", Java)(Sequence(Literal(0x00), Literal('a')))
  check("#c\u0000a", "x", DotNet, PCRE, Perl(), Python(), Ruby())(Sequence())
  check("(*NUL)#c\u0000a", "x", PCRE)(Sequence(Command(StartOption("NUL", None)), Literal('a')))
  check("(*CR)#c\n)", "xu", PCRE)(Command(StartOption("CR", None)))
  error("(*CR)#c\r)", "xu", PCRE)("Unmatched ')' at 8")
  check("(*CRLF)#c\r)", "x", PCRE)(Command(StartOption("CRLF", None)))
  error("(*CRLF)#c\r\n)", "x", PCRE)("Unmatched ')' at 11")
  error("(*ANY)#c\u0085)", "xu", PCRE)("Unmatched ')' at 9")
  check("(*CR)(*LF)#c\r)", "x", PCRE)(Sequence(Command(StartOption("CR", None)), Command(StartOption("LF", None))))
  // Java turns the extended mode on and off in the middle of a pattern.
  check("(?x)a b(?-x) c", "", Java):
    Sequence(
      Command(InlineFlag(FlagSetDiff(FlagSet(verbose = true), None))),
      Literal('a'),
      Literal('b'),
      Command(InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(verbose = true))))),
      Literal(' '),
      Literal('c')
    )

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
  check("a{,}", "", DotNet, JavaScript, PCRE, Perl(), Ruby()):
    Sequence(Literal('a'), Literal('{'), Literal(','), Literal('}'))
  check("a{,}", "", Python())(Repeat(Literal('a'), Unbounded(0, Greedy)))
  check("a{,}?", "", Python())(Repeat(Literal('a'), Unbounded(0, Lazy)))
  check("a{,}+", "", Python())(Repeat(Literal('a'), Unbounded(0, Possessive)))
  check("a{ ,}", "", Python())(Sequence(Literal('a'), Literal('{'), Literal(' '), Literal(','), Literal('}')))
  error("{,}", "", Python())("Nothing to repeat at 3")
  check("a {,}", "x", Python())(Repeat(Literal('a'), Unbounded(0, Greedy)))
  check("a{, }", "x", Python())(Sequence(Literal('a'), Literal('{'), Literal(','), Literal('}')))
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
  // Java skips whitespace and comments in `{n,m}` except before the first digit (checked with Java 27.0.1).
  check("a{1 0}", "x", Java)(Repeat(Literal('a'), Exact(10, Greedy)))
  check("a{1 ,2 }", "x", Java)(Repeat(Literal('a'), Bounded(1, 2, Greedy)))
  check("a{1,#c\n}", "x", Java)(Repeat(Literal('a'), Unbounded(1, Greedy)))
  error("a{ 1}", "x", Java)("Incomplete quantifier at 1")

  // Caret
  check("^", "", All*)(Caret)

  // Dollar
  check("$", "", All*)(Dollar)

  // Dot
  check(".", "", All*)(Dot)

  test("Parser.determineUnitMode: the mode of the IR"):
    val cases = Seq(
      ("a", "", JavaScript, UnitMode.UTF16),
      ("a", "u", JavaScript, UnitMode.UTF16CodePoint),
      ("a", "v", JavaScript, UnitMode.UTF16CodePoint),
      ("a", "", Java, UnitMode.UTF16CodePoint),
      ("a", "", DotNet, UnitMode.UTF16),
      ("a", "", Python(), UnitMode.CodePoint),
      ("a", "", Python(Python.Type.Bytes), UnitMode.Byte),
      ("a", "", Ruby(), UnitMode.ScalarValue),
      ("a", "n", Ruby(), UnitMode.Byte),
      ("a", "", Ruby(Ruby.Encoding.ASCII8Bit), UnitMode.Byte),
      ("a", "", Perl(), UnitMode.CodePoint),
      ("a", "", Perl(utf8 = false), UnitMode.Byte),
      ("a", "", PCRE, UnitMode.Byte),
      ("a", "u", PCRE, UnitMode.ScalarValue),
      ("(*UTF)a", "", PCRE, UnitMode.ScalarValue),
      ("(*NOTEMPTY)(*UTF8)a|b", "", PCRE, UnitMode.ScalarValue)
    )
    for (source, flags, dialect, mode) <- cases do
      val flagSet = FlagSet.parse(flags, dialect)
      assertEquals(Parser.determineUnitMode(source, flagSet, dialect), mode, (source, flags, dialect))

  test("Parser.parse: a high surrogate without a low surrogate after it is a code point by itself"):
    val node = Parser.parse("\uD800a", FlagSet.parse("u", JavaScript), JavaScript, UnitMode.UTF16CodePoint)
    assert(node.equalsWithoutLoc(Node(Sequence(Literal(0xd800), Literal('a')))), node)
