package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.AssertKind.*
import codes.quine.labs.resyntax.ast.BackReferenceStyle.*
import codes.quine.labs.resyntax.ast.BackslashKind.*
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.BoundaryModifier.*
import codes.quine.labs.resyntax.ast.CaseCommandKind.*
import codes.quine.labs.resyntax.ast.ClassItemData.*
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.EscapeClassKind.*
import codes.quine.labs.resyntax.ast.EscapeStyle.*
import codes.quine.labs.resyntax.ast.GroupKind.*
import codes.quine.labs.resyntax.ast.NameStyle.*
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.Quantifier.*
import codes.quine.labs.resyntax.ast.Reference.*

class ParserBackslashSuite extends ParserSuiteBase:
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
