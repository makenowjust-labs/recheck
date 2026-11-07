package codes.quine.labs.resyntax.parser
import codes.quine.labs.resyntax.ast.AssertNameStyle.*
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackControlKind.*
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.ClassItemData.*
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.CommandKind.*
import codes.quine.labs.resyntax.ast.ConditionalTest.*
import codes.quine.labs.resyntax.ast.EscapeStyle
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.GroupKind.*
import codes.quine.labs.resyntax.ast.NameStyle.*
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.Quantifier.*
import codes.quine.labs.resyntax.ast.Reference.*
import codes.quine.labs.resyntax.dialect.Dialect.*

class ParserCommandSuite extends munit.FunSuite with ParserHelper:

  check("(?(1)a)", "", DotNet, PCRE, Perl(), Python(), Ruby())(Command(Conditional(Indexed(1), Literal('a'))))
  check("(?(1)a|b)", "", DotNet, PCRE, Perl(), Python(), Ruby())(
    Command(Conditional(Indexed(1), Literal('a'), Literal('b')))
  )
  check("(?(<x>)a)", "", PCRE, Perl(), Ruby())(Command(Conditional(Named(Angle, "x"), Literal('a'))))
  check("(?('x')a)", "", PCRE, Perl(), Ruby())(Command(Conditional(Named(Quote, "x"), Literal('a'))))
  check("(?(<x>)a)", "", DotNet):
    Command(Conditional(LookAround(Sequence(Literal('<'), Literal('x'), Literal('>'))), Literal('a')))
  check("(?(<x+0>)a)", "", DotNet):
    Command(
      Conditional(
        LookAround(Sequence(Literal('<'), Repeat(Literal('x'), Plus(Greedy)), Literal('0'), Literal('>'))),
        Literal('a')
      )
    )
  check("(?(<1>)a|b)", "", Ruby()):
    Command(Conditional(Referenced(Angle, IndexedReference(1)), Literal('a'), Literal('b')))
  check("(?('-1')a)", "", Ruby())(Command(Conditional(Referenced(Quote, RelativeReference(-1)), Literal('a'))))
  check("(?(<x+0>)a)", "", Ruby()):
    Command(Conditional(Referenced(Angle, LeveledReference(NamedReference("x"), 0)), Literal('a')))
  check("(?(<+1>)a)", "", Ruby())(Command(Conditional(Named(Angle, "+1"), Literal('a'))))
  error("(?(<1>)a)", "", PCRE, Perl())("Invalid identifier at 4")
  check("(?<n>a)(?(<1>)b)", "", Ruby()):
    Sequence(
      Group(NamedCapture(Angle, "n"), Literal('a')),
      Command(Conditional(Referenced(Angle, IndexedReference(1)), Literal('b')))
    )
  check("(?(DEFINE)a)", "", PCRE, Perl())(Command(Conditional(Define, Literal('a'))))
  check("(?(VERSION>=1.2)a)", "", PCRE)(Command(Conditional(Version(lt = true, 1, 2), Literal('a'))))
  check("(?(VERSION=1.2)a)", "", PCRE)(Command(Conditional(Version(lt = false, 1, 2), Literal('a'))))
  check("(?(R)a)", "", PCRE, Perl())(Command(Conditional(RRecursion, Literal('a'))))
  check("(?(R1)a)", "", PCRE, Perl())(Command(Conditional(IndexedRecursion(1), Literal('a'))))
  check("(?(R&x)a)", "", PCRE, Perl())(Command(Conditional(NamedRecursion("x"), Literal('a'))))
  check("(?(x)a)", "", PCRE, Python())(Command(Conditional(Named(Bare, "x"), Literal('a'))))
  check("(?(+1)a)", "", PCRE)(Command(Conditional(Relative(1), Literal('a'))))
  check("(?(-1)a)", "", PCRE)(Command(Conditional(Relative(-1), Literal('a'))))
  check("(?(?=a)b)", "", DotNet, PCRE, Perl()):
    Command(Conditional(LookAround(PositiveLookAhead(Symbolic), Literal('a')), Literal('b')))
  check("(?(*positive_lookahead:a)b)", "", PCRE, Perl()):
    Command(Conditional(LookAround(PositiveLookAhead(Alphabetic), Literal('a')), Literal('b')))
  check("(?(*pla:a)b)", "", PCRE, Perl()):
    Command(Conditional(LookAround(PositiveLookAhead(Abbrev), Literal('a')), Literal('b')))
  check("(?(?!a)b)", "", DotNet, PCRE, Perl()):
    Command(Conditional(LookAround(NegativeLookAhead(Symbolic), Literal('a')), Literal('b')))
  check("(?(*negative_lookahead:a)b)", "", PCRE, Perl()):
    Command(Conditional(LookAround(NegativeLookAhead(Alphabetic), Literal('a')), Literal('b')))
  check("(?(*nla:a)b)", "", PCRE, Perl()):
    Command(Conditional(LookAround(NegativeLookAhead(Abbrev), Literal('a')), Literal('b')))
  check("(?(?<=a)b)", "", DotNet, PCRE, Perl()):
    Command(Conditional(LookAround(PositiveLookBehind(Symbolic), Literal('a')), Literal('b')))
  check("(?(*positive_lookbehind:a)b)", "", PCRE, Perl()):
    Command(Conditional(LookAround(PositiveLookBehind(Alphabetic), Literal('a')), Literal('b')))
  check("(?(*plb:a)b)", "", PCRE, Perl()):
    Command(Conditional(LookAround(PositiveLookBehind(Abbrev), Literal('a')), Literal('b')))
  check("(?(?<!a)b)", "", DotNet, PCRE, Perl()):
    Command(Conditional(LookAround(NegativeLookBehind(Symbolic), Literal('a')), Literal('b')))
  check("(?(*negative_lookbehind:a)b)", "", PCRE, Perl()):
    Command(Conditional(LookAround(NegativeLookBehind(Alphabetic), Literal('a')), Literal('b')))
  check("(?(*nlb:a)b)", "", PCRE, Perl()):
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
  // The blanks in a condition are not kept.
  checkPrinted("(?(?C) (?!a)b)", "(?(?C)(?!a)b)", "x", PCRE):
    Command(
      Conditional(
        CalloutLookAround(CommandKind.Callout, LookAround(NegativeLookAhead(Symbolic), Literal('a'))),
        Literal('b')
      )
    )
  error("(?(?C1) (?=a)b)", "", PCRE)("Invalid condition at 7")
  error("(?(?C1)(?:a)b)", "", PCRE)("Invalid condition at 8")
  error("(?(?C1)(?=a)b)", "", Perl())("Invalid condition at 3")
  // The group of a .NET look-ahead condition, the zeros of an index and a version's zero minor are not kept.
  checkPrinted("(?(?:.)b)", "(?(.)b)", "", DotNet)(Command(Conditional(LookAround(Dot), Literal('b'))))
  error("(?(.)b)", "", PCRE, Perl())("Invalid condition at 3")
  check("(?(0)a)", "", DotNet)(Command(Conditional(Indexed(0), Literal('a'))))
  error("(?(0)a)", "", PCRE, Perl(), Python(), Ruby())("Invalid condition at 3")
  checkPrinted("(?(01)a)", "(?(1)a)", "", DotNet, PCRE, Python(), Ruby()):
    Command(Conditional(Indexed(1), Literal('a')))
  error("(?(01)a)", "", Perl())("Invalid condition at 3")
  error("(?(DEFINE)a|b)", "", PCRE, Perl())("Invalid condition at 11")
  check("(?(DEFINEx)a)", "", PCRE)(Command(Conditional(Named(Bare, "DEFINEx"), Literal('a'))))
  check("(?(Rx)a)", "", PCRE)(Command(Conditional(Named(Bare, "Rx"), Literal('a'))))
  error("(?(Rx)a)", "", Perl())("Invalid condition at 3")
  checkPrinted("(?(VERSION>=10)a)", "(?(VERSION>=10.0)a)", "", PCRE):
    Command(Conditional(Version(lt = true, 10, 0), Literal('a')))
  error("(?(VERSION>=10.)a)", "", PCRE)("Invalid condition at 15")
  check("(?'x'a)", "", DotNet, PCRE, Perl(), Ruby())(Group(NamedCapture(Quote, "x"), Literal('a')))
  check("(?'x-y'a)", "", DotNet)(Group(Balance(Quote, Some("x"), "y"), Literal('a')))
  check("(?'-y'a)", "", DotNet)(Group(Balance(Quote, None, "y"), Literal('a')))
  check("(?P<x>a)", "", PCRE, Perl(), Python())(Group(PNamedCapture("x"), Literal('a')))
  check("(?P=x)", "", PCRE, Perl(), Python())(Command(PBackReference("x")))
  check("(?P>x)", "", PCRE, Perl())(Command(PNamedCall("x")))
  check("(?R)", "", PCRE, Perl())(Command(RCall))
  check("(?&x)", "", PCRE, Perl())(Command(NamedCall("x")))
  check("(?1)", "", PCRE, Perl())(Command(IndexedCall(1)))
  check("(?+1)", "", PCRE, Perl())(Command(RelativeCall(1)))
  check("(?-1)", "", PCRE, Perl())(Command(RelativeCall(-1)))
  check("(?-x)", "", DotNet, Java, PCRE, Perl(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(verbose = true)))))
  error("(?-x)", "", Python())("Invalid group at 0")
  check("(?-x:a)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby()):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(verbose = true)))), Literal('a'))
  check("(?-i:a)", "", JavaScript):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(ignoreCase = true)))), Literal('a'))
  error("(?-i)", "", JavaScript)("Invalid group at 0")
  check("(?^x)", "", PCRE, Perl())(Command(CommandKind.ResetFlag(FlagSet(verbose = true))))
  check("(?^x:a)", "", PCRE, Perl())(Group(GroupKind.ResetFlag(FlagSet(verbose = true)), Literal('a')))
  check("(?^)", "", PCRE, Perl())(Command(CommandKind.ResetFlag(FlagSet())))
  check("(?^aa)", "", PCRE)(Command(CommandKind.ResetFlag(FlagSet(ascii = true), Some("aa"))))
  check("(?^aa)", "", Perl())(Command(CommandKind.ResetFlag(FlagSet(ascii = true, asciiRestrict = true))))
  error("(?^d)", "", Perl())("Invalid flag at 3")
  error("(?^-i)", "", PCRE, Perl())("Invalid group at 3")
  check("(?#x)", "", DotNet, PCRE, Perl(), Python(), Ruby())(Command(Comment("x")))
  check("(?#x\\)y)", "", Python(), Ruby())(Command(Comment("x\\)y")))
  check("(?#x)(?#y)a", "", DotNet, PCRE, Perl(), Python(), Ruby()):
    Sequence(Command(Comment("x")), Command(Comment("y")), Literal('a'))
  check("a(?#x)|(?#y)", "", DotNet, PCRE, Perl(), Python(), Ruby()):
    Disjunction(Sequence(Literal('a'), Command(Comment("x"))), Command(Comment("y")))
  check("((?#x))", "", DotNet, PCRE, Perl(), Python(), Ruby())(Group(IndexedCapture, Command(Comment("x"))))
  error("(?#x", "", DotNet, PCRE, Perl(), Python(), Ruby())("Invalid group at 4")
  // The comments that Ruby removes are kept in the flags and quantifiers, but not right after `(?` or in a name.
  checkPrinted("(?(?#x)a)", "(?a)", "", Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), None)))
  check("(?i(?#x)m)a", "", Ruby()):
    Sequence(
      Command(
        CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true, multiline = true), None, Some("i(?#x)m")))
      ),
      Literal('a')
    )
  check("a{1(?#x),2}", "", Ruby())(Repeat(Node(Literal('a')), Bounded(1, 2, Greedy), Some("{1(?#x),2}")))
  checkPrinted("(?<a(?#x)b>c)", "(?<ab>c)", "", Ruby())(Group(NamedCapture(Angle, "ab"), Literal('c')))
  // Ruby reads `\x` and `\u` before removing comments, so a comment in them is an error.
  error("\\x(?#x)41", "", Ruby())("Invalid backslash escape at 1")
  error("\\u{4(?#x)1}", "", Ruby())("Invalid backslash escape at 1")
  error("\\u{(?#x)41}", "", Ruby())("Invalid backslash escape at 1")
  error("\\u{41(?#x)}", "", Ruby())("Invalid backslash escape at 1")
  check("\\x4(?#x)1", "", Ruby())(
    Sequence(Backslash(BackslashKind.Escape(EscapeStyle.Hex1, 0x04)), Command(Comment("x")), Literal('1'))
  )
  check("\\u0041(?#x)", "", Ruby())(
    Sequence(Backslash(BackslashKind.Escape(EscapeStyle.UnicodeHex4, 0x41)), Command(Comment("x")))
  )
  check("[(?#x)]", "", Ruby()):
    Class(
      false,
      ClassUnion(ClassLiteral('('), ClassLiteral('?'), ClassLiteral('#'), ClassLiteral('x'), ClassLiteral(')'))
    )
  check("[#(?#x)]", "x", Ruby()):
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
  error("\\(?#x)", "", Ruby())("Unmatched ')' at 5")
  error("(?x:#(?#\n))", "", Ruby())("Unmatched ')' at 10")
  check("(?x:#\n)(?#y)", "", Ruby()):
    Sequence(
      Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)), Blank("#\n")),
      Command(Comment("y"))
    )
  check("a (?#x)b", "x", Ruby())(Sequence(Literal('a'), Blank(" "), Command(Comment("x")), Literal('b')))
  check("(?{x})", "", Perl())(Command(InlineCode("x")))
  check("(??{x})", "", Perl())(Command(EmbedCode("x")))
  check("(?C)", "", PCRE)(Command(Callout))
  check("(?C1)", "", PCRE)(Command(CalloutInt(1)))
  error("(?C256)", "", PCRE)("Invalid group at 6")
  error("a(?C1)*", "", PCRE)("Nothing to repeat at 6")
  check("(?C{x})", "", PCRE)(Command(CalloutString('{', '}', "x")))
  check("(?C{x{{y}}z})", "", PCRE)(Command(CalloutString('{', '}', "x{{y}}z")))
  for delim <- Seq('`', '\'', '"', '^', '%', '#', '$') do
    check(s"(?C${delim}x$delim)", "", PCRE)(Command(CalloutString(delim, delim, "x")))

  check("(*ACCEPT)", "", PCRE, Perl())(Command(BacktrackControl(Some(Accept), None)))
  check("(*ACCEPT:x)", "", PCRE, Perl())(Command(BacktrackControl(Some(Accept), Some("x"))))
  check("(*FAIL)", "", PCRE, Perl())(Command(BacktrackControl(Some(Fail), None)))
  check("(*FAIL:x)", "", PCRE, Perl())(Command(BacktrackControl(Some(Fail), Some("x"))))
  check("(*MARK:x)", "", PCRE, Perl())(Command(BacktrackControl(Some(Mark), Some("x"))))
  check("(*COMMIT)", "", PCRE, Perl())(Command(BacktrackControl(Some(Commit), None)))
  check("(*COMMIT:x)", "", PCRE, Perl())(Command(BacktrackControl(Some(Commit), Some("x"))))
  check("(*PRUNE)", "", PCRE, Perl())(Command(BacktrackControl(Some(Prune), None)))
  check("(*PRUNE:x)", "", PCRE, Perl())(Command(BacktrackControl(Some(Prune), Some("x"))))
  check("(*SKIP)", "", PCRE, Perl())(Command(BacktrackControl(Some(Skip), None)))
  check("(*SKIP:x)", "", PCRE, Perl())(Command(BacktrackControl(Some(Skip), Some("x"))))
  check("(*THEN)", "", PCRE, Perl())(Command(BacktrackControl(Some(Then), None)))
  check("(*THEN:x)", "", PCRE, Perl())(Command(BacktrackControl(Some(Then), Some("x"))))
  check("(*positive_lookahead:a)", "", PCRE, Perl())(Group(PositiveLookAhead(Alphabetic), Literal('a')))
  check("(*pla:a)", "", PCRE, Perl())(Group(PositiveLookAhead(Abbrev), Literal('a')))
  check("(*negative_lookahead:a)", "", PCRE, Perl())(Group(NegativeLookAhead(Alphabetic), Literal('a')))
  check("(*nla:a)", "", PCRE, Perl())(Group(NegativeLookAhead(Abbrev), Literal('a')))
  check("(*positive_lookbehind:a)", "", PCRE, Perl())(Group(PositiveLookBehind(Alphabetic), Literal('a')))
  check("(*plb:a)", "", PCRE, Perl())(Group(PositiveLookBehind(Abbrev), Literal('a')))
  check("(*negative_lookbehind:a)", "", PCRE, Perl())(Group(NegativeLookBehind(Alphabetic), Literal('a')))
  check("(*nlb:a)", "", PCRE, Perl())(Group(NegativeLookBehind(Abbrev), Literal('a')))
  check("(*atomic:a)", "", PCRE, Perl())(Group(Atomic(Alphabetic), Literal('a')))
  check("(*script_run:a)", "", PCRE, Perl())(Group(ScriptRun(Alphabetic), Literal('a')))
  check("(*sr:a)", "", PCRE, Perl())(Group(ScriptRun(Abbrev), Literal('a')))
  check("(*atomic_script_run:a)", "", PCRE, Perl())(Group(AtomicScriptRun(Alphabetic), Literal('a')))
  check("(*asr:a)", "", PCRE, Perl())(Group(AtomicScriptRun(Abbrev), Literal('a')))
  check("(*non_atomic_positive_lookahead:a)", "", PCRE)(Group(NonAtomicPositiveLookAhead(Alphabetic), Literal('a')))
  check("(*napla:a)", "", PCRE)(Group(NonAtomicPositiveLookAhead(Abbrev), Literal('a')))
  check("(*non_atomic_positive_lookbehind:a)", "", PCRE)(Group(NonAtomicPositiveLookBehind(Alphabetic), Literal('a')))
  check("(*naplb:a)", "", PCRE)(Group(NonAtomicPositiveLookBehind(Abbrev), Literal('a')))
  check("(*:x)", "", PCRE, Perl())(Command(BacktrackControl(None, Some("x"))))
  check("(*scan_substring:(1)a)", "", PCRE)(Group(ScanSubstring(Alphabetic, Seq(IndexedReference(1))), Literal('a')))
  // The quotes of a name are not kept.
  checkPrinted("(*scs:(-1,<x>,'y')a)", "(*scs:(-1,<x>,<y>)a)", "", PCRE):
    Group(
      ScanSubstring(Abbrev, Seq(RelativeReference(-1), NamedReference("x"), NamedReference("y"))),
      Literal('a')
    )
  error("(*scs:(x)a)", "", PCRE)("Invalid group at 7")
  error("(*scs:(1,)a)", "", PCRE)("Invalid group at 9")
  error("(*scs:(1)a)", "", Perl())("Invalid group at 2")
  check("(*:a b-c)", "", PCRE, Perl())(Command(BacktrackControl(None, Some("a b-c"))))
  error("(*:)", "", PCRE, Perl())("Invalid group at 3")
  error("(*MARK)", "", PCRE, Perl())("Invalid group at 6")
  error("(*MARK:)", "", PCRE, Perl())("Invalid group at 7")
  check("(*F)", "", PCRE, Perl())(Command(BacktrackControl(Some(F), None)))
  check("(*COMMIT:)", "", PCRE, Perl())(Command(BacktrackControl(Some(Commit), Some(""))))
  check("(*ACCEPT)?", "", PCRE, Perl())(Repeat(Command(BacktrackControl(Some(Accept), None)), Question(Greedy)))
  check("(*COMMIT)?", "", Perl())(Repeat(Command(BacktrackControl(Some(Commit), None)), Question(Greedy)))
  error("(*COMMIT)?", "", PCRE)("Nothing to repeat at 9")
  check("(*UTF)(*LIMIT_MATCH=10)a", "", PCRE):
    Sequence(Command(StartOption("UTF", None)), Command(StartOption("LIMIT_MATCH", Some(10))), Literal('a'))
  error("(*UTF)", "", Perl())("Invalid group at 2")
  error("a(*UTF)", "", PCRE)("Invalid group at 3")
  error("(?i)(*UTF)", "", PCRE)("Invalid group at 6")
  error("(*UTF)*", "", PCRE)("Nothing to repeat at 6")
  check("(*TURKISH_CASING)(*UTF)", "", PCRE):
    Sequence(Command(StartOption("TURKISH_CASING", None)), Command(StartOption("UTF", None)))
  error("(*TURKISH_CASING)", "", PCRE)("Invalid group at 2")
  check("(*TURKISH_CASING)", "u", PCRE)(Command(StartOption("TURKISH_CASING", None)))
  error("(*UCP)(*TURKISH_CASING)", "", PCRE)("Invalid group at 8")
  error("(*COMMITx)", "", PCRE)("Invalid group at 8")

  // python-regex's fuzzy constraints, and the literal `{` otherwise (regex 2026.9.29).
  check("a{e<=1}", "", PythonRegex())(Command(Fuzzy(Literal('a'), "e<=1")))
  check("a{e}", "", PythonRegex())(Command(Fuzzy(Literal('a'), "e")))
  check("a{1<=e<2}", "", PythonRegex())(Command(Fuzzy(Literal('a'), "1<=e<2")))
  check("a{1<e<=2}", "", PythonRegex())(Command(Fuzzy(Literal('a'), "1<e<=2")))
  check("a{2i}", "", PythonRegex())(Sequence(Literal('a'), Literal('{'), Literal('2'), Literal('i'), Literal('}')))
  check("a{i<=1,d<=1}", "", PythonRegex())(Command(Fuzzy(Literal('a'), "i<=1,d<=1")))
  check("a{2i+2d<5,e}", "", PythonRegex())(Command(Fuzzy(Literal('a'), "2i+2d<5,e")))
  check("a{e<=1:[a-z]}", "", PythonRegex())(Command(Fuzzy(Literal('a'), "e<=1:[a-z]")))
  check("a{e:\\d}", "", PythonRegex())(Command(Fuzzy(Literal('a'), "e:\\d")))
  check("a{e:x}", "", PythonRegex())(Command(Fuzzy(Literal('a'), "e:x")))
  check("(?x)a{ e }", "", PythonRegex()):
    Sequence(
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None))),
      Command(Fuzzy(Literal('a'), " e "))
    )
  checkPrinted("a(?i){e}", "a{e}(?i)", "", PythonRegex()):
    Sequence(
      Command(Fuzzy(Literal('a'), "e")),
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None)))
    )
  check("a{1<e}", "", PythonRegex()):
    Sequence(Literal('a'), Literal('{'), Literal('1'), Literal('<'), Literal('e'), Literal('}'))
  check("a{1<x<2}", "", PythonRegex()):
    Sequence(
      Literal('a'),
      Literal('{'),
      Literal('1'),
      Literal('<'),
      Literal('x'),
      Literal('<'),
      Literal('2'),
      Literal('}')
    )
  check("a{2<e}", "", PythonRegex()):
    Sequence(Literal('a'), Literal('{'), Literal('2'), Literal('<'), Literal('e'), Literal('}'))
  check("a{e,x}", "", PythonRegex()):
    Sequence(Literal('a'), Literal('{'), Literal('e'), Literal(','), Literal('x'), Literal('}'))
  check("a{2i+x<=1}", "", PythonRegex()):
    Sequence(
      Literal('a'),
      Literal('{'),
      Literal('2'),
      Repeat(Literal('i'), Plus(Greedy)),
      Literal('x'),
      Literal('<'),
      Literal('='),
      Literal('1'),
      Literal('}')
    )
  error("a{e:(}", "", PythonRegex())("Expected character set at 4")
  error("a{e<3", "", PythonRegex())("Expected } at 5")
  error("a{e<=x}", "", PythonRegex())("Bad fuzzy cost limit at 5")
  error("a{e<0}", "", PythonRegex())("Bad fuzzy cost limit at 4")
  error("a{2<=e<=1}", "", PythonRegex())("Bad fuzzy cost limit at 8")
  error("a{2i<=0,2d<1}", "", PythonRegex())("More than one cost equation at 8")
  error("a{2i+i<=1}", "", PythonRegex())("Repeated fuzzy cost at 5")
  error("a{i+d<=1}", "", PythonRegex())("Expected } at 3")
  error("a{2i<x}", "", PythonRegex())("Bad fuzzy cost limit at 5")
  error("a{2i<0}", "", PythonRegex())("Bad fuzzy cost limit at 5")
  error("{e}", "", PythonRegex())("Nothing to repeat at 3")
  error("a*{e}", "", PythonRegex())("Nested quantifier at 5")
  error("a{e}*", "", PythonRegex())("Nothing to repeat at 4")

  // python-regex's verbs.
  check("(*FAIL)", "", PythonRegex())(Command(BacktrackControl(Some(Fail), None)))
  check("(*F)", "", PythonRegex())(Command(BacktrackControl(Some(F), None)))
  check("(*PRUNE)", "", PythonRegex())(Command(BacktrackControl(Some(Prune), None)))
  check("(*SKIP)", "", PythonRegex())(Command(BacktrackControl(Some(Skip), None)))
  checkPrinted("(* FAIL )", "(*FAIL)", "x", PythonRegex())(Command(BacktrackControl(Some(Fail), None)))
  error("(*COMMIT)", "", PythonRegex())("Unknown verb at 2")
  error("(*)", "", PythonRegex())("Nothing to repeat at 1")
  error("(*1)", "", PythonRegex())("Nothing to repeat at 1")
  error("(*FAIL", "", PythonRegex())("Unclosed group at 6")

  // python-regex's conditionals and calls.
  check("(?(DEFINE)a|b)", "", PythonRegex())(Command(Conditional(Define, Literal('a'), Literal('b'))))
  check("(?(?=a)a|b)", "", PythonRegex()):
    Command(Conditional(LookAround(PositiveLookAhead(Symbolic), Literal('a')), Literal('a'), Literal('b')))
  checkPrinted("(?P&n)", "(?P>n)", "", PythonRegex())(Command(PNamedCall("n")))
  error("(?P&n)", "", Python())("Invalid group at 3")
  check("(?R)", "", PythonRegex())(Command(RCall))
  check("(?1)", "", PythonRegex())(Command(IndexedCall(1)))
  check("(?-1)", "", PythonRegex())(Command(RelativeCall(-1)))
  check("(?&n)", "", PythonRegex())(Command(NamedCall("n")))
