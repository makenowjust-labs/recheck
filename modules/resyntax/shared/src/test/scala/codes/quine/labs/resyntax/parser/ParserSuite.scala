package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.AssertKind._
import codes.quine.labs.resyntax.ast.AssertNameStyle._
import codes.quine.labs.resyntax.ast.BackReferenceStyle._
import codes.quine.labs.resyntax.ast.BackslashKind._
import codes.quine.labs.resyntax.ast.BacktrackControlKind._
import codes.quine.labs.resyntax.ast.BacktrackStrategy._
import codes.quine.labs.resyntax.ast.BoundaryModifier._
import codes.quine.labs.resyntax.ast.CaseCommandKind._
import codes.quine.labs.resyntax.ast.ClassItemData._
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.CommandKind._
import codes.quine.labs.resyntax.ast.ConditionalTest._
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect._
import codes.quine.labs.resyntax.ast.EscapeClassKind._
import codes.quine.labs.resyntax.ast.EscapeStyle._
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.GroupKind._
import codes.quine.labs.resyntax.ast.NameStyle._
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.NodeData._
import codes.quine.labs.resyntax.ast.Quantifier._
import codes.quine.labs.resyntax.ast.QuoteLiteral._
import codes.quine.labs.resyntax.ast.Reference._

class ParserSuite extends munit.FunSuite {
  def check(s: String, flags: String, dialects: Dialect*)(expected: NodeData)(implicit loc: munit.Location): Unit = {
    for (dialect <- dialects) {
      test(s"Parser.parse: /$s/$flags in $dialect") {
        val flagSet = FlagSet.parse(flags, dialect)
        val result = Parser.parse(s, flagSet, dialect)
        if (!result.equalsWithoutLoc(Node(expected))) {
          println(result)
          println(Node(expected))
        }
        assert(result.equalsWithoutLoc(Node(expected)))
      }
    }
  }

  def error(s: String, flags: String, dialects: Dialect*)(message: String)(implicit loc: munit.Location): Unit = {
    for (dialect <- dialects) {
      test(s"Parser.parse: /$s/$flags in $dialect with error \"$message\"") {
        val flagSet = FlagSet.parse(flags, dialect)
        interceptMessage[ParsingException](message) {
          Parser.parse(s, flagSet, dialect)
        }
      }
    }
  }

  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)

  // Top
  error(")", "", All: _*)("Unmatched ')' at 0")

  // Comment
  check(" ", "x", DotNet, Java, PCRE, Perl, Python, Ruby)(Sequence())
  check("# foo", "x", DotNet, Java, PCRE, Perl, Python, Ruby)(Sequence())
  check("# \\\nfoo", "x", Python)(Sequence())

  // Disjunction:
  check("a|b", "", All: _*)(Disjunction(Literal('a'), Literal('b')))

  // Sequence:
  check("ab", "", All: _*)(Sequence(Literal('a'), Literal('b')))

  // Repeat:
  check("a*", "", All: _*)(Repeat(Literal('a'), Star(Greedy)))
  check("a*?", "", All: _*)(Repeat(Literal('a'), Star(Lazy)))
  check("a*+", "", Java, PCRE, Perl, Ruby)(Repeat(Literal('a'), Star(Possessive)))
  check("a+", "", All: _*)(Repeat(Literal('a'), Plus(Greedy)))
  check("a+?", "", All: _*)(Repeat(Literal('a'), Plus(Lazy)))
  check("a++", "", Java, PCRE, Perl, Ruby)(Repeat(Literal('a'), Plus(Possessive)))
  check("a?", "", All: _*)(Repeat(Literal('a'), Question(Greedy)))
  check("a??", "", All: _*)(Repeat(Literal('a'), Question(Lazy)))
  check("a?+", "", Java, PCRE, Perl, Ruby)(Repeat(Literal('a'), Question(Possessive)))
  check("a{2}", "", All: _*)(Repeat(Literal('a'), Exact(2, Greedy)))
  check("a{2}?", "", DotNet, Java, JavaScript, PCRE, Perl, Python)(Repeat(Literal('a'), Exact(2, Lazy)))
  check("a{2}?", "", Ruby)(Repeat(Repeat(Literal('a'), Exact(2, Greedy)), Question(Greedy)))
  check("a{2}+", "", Java, PCRE, Perl)(Repeat(Literal('a'), Exact(2, Possessive)))
  check("a{2}+", "", Ruby)(Repeat(Repeat(Literal('a'), Exact(2, Greedy)), Plus(Greedy)))
  check("a{2,}", "", All: _*)(Repeat(Literal('a'), Unbounded(2, Greedy)))
  check("a{2,}?", "", All: _*)(Repeat(Literal('a'), Unbounded(2, Lazy)))
  check("a{2,}+", "", Java, PCRE, Perl, Ruby)(Repeat(Literal('a'), Unbounded(2, Possessive)))
  check("a{2,3}", "", All: _*)(Repeat(Literal('a'), Bounded(2, 3, Greedy)))
  check("a{2,3}?", "", All: _*)(Repeat(Literal('a'), Bounded(2, 3, Lazy)))
  check("a{2,3}+", "", Java, PCRE, Perl, Ruby)(Repeat(Literal('a'), Bounded(2, 3, Possessive)))
  check("a{,3}", "", Perl, Python, Ruby)(Repeat(Literal('a'), MaxBounded(3, Greedy)))
  check("a{,3}?", "", Perl, Python, Ruby)(Repeat(Literal('a'), MaxBounded(3, Lazy)))
  check("a{,3}+", "", Perl, Ruby)(Repeat(Literal('a'), MaxBounded(3, Possessive)))
  check("a{", "", DotNet, JavaScript, PCRE, Perl, Python, Ruby)(Sequence(Literal('a'), Literal('{')))
  error("a{", "", Java)("Incomplete quantifier at 1")
  error("a{", "u", JavaScript)("Incomplete quantifier at 1")
  error("*", "", All: _*)("Nothing to repeat at 0")
  check("a**", "", Ruby)(Repeat(Repeat(Literal('a'), Star(Greedy)), Star(Greedy)))
  error("a**", "", DotNet, Java, JavaScript, PCRE, Perl, Python)("Nested quantifier at 2")
  error("^*", "", DotNet, Java, JavaScript, PCRE, Perl, Python)("Nothing to repeat at 1")
  error("$*", "", DotNet, Java, JavaScript, PCRE, Perl, Python)("Nothing to repeat at 1")
  error("(?=a)*", "u", JavaScript)("Nothing to repeat at 5")
  error("(?<=a)*", "", JavaScript)("Nothing to repeat at 6")
  check("}", "", All: _*)(Literal('}'))
  error("}", "u", JavaScript)("Incomplete quantifier at 0")

  // Group
  check("(?:a)", "", All: _*)(Group(NonCapture, Literal('a')))
  check("(?|a)", "", PCRE, Perl)(Command(BranchReset(Literal('a'))))
  check("(?|a|b)", "", PCRE, Perl)(Command(BranchReset(Literal('a'), Literal('b'))))
  check("(?=a)", "", All: _*)(Group(PositiveLookAhead(Symbolic), Literal('a')))
  check("(?!a)", "", All: _*)(Group(NegativeLookAhead(Symbolic), Literal('a')))
  check("(?<=a)", "", All: _*)(Group(PositiveLookBehind(Symbolic), Literal('a')))
  check("(?<!a)", "", All: _*)(Group(NegativeLookBehind(Symbolic), Literal('a')))
  check("(?<*a)", "", PCRE)(Group(NonAtomicPositiveLookBehind(Symbolic), Literal('a')))
  check("(?<x>a)", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(Group(NamedCapture(Angle, "x"), Literal('a')))
  check("(?<x-y>a)", "", DotNet)(Group(Balance(Angle, Some("x"), "y"), Literal('a')))
  check("(?<-y>a)", "", DotNet)(Group(Balance(Angle, None, "y"), Literal('a')))
  check("(?>a)", "", DotNet, Java, PCRE, Perl, Ruby)(Group(Atomic(Symbolic), Literal('a')))
  check("(?*a)", "", PCRE)(Group(NonAtomicPositiveLookAhead(Symbolic), Literal('a')))
  check("(?~a)", "", Ruby)(Group(Absence, Literal('a')))
  check("(?(1)a)", "", DotNet, PCRE, Perl, Python, Ruby)(Command(Conditional(Indexed(1), Literal('a'))))
  check("(?(1)a|b)", "", DotNet, PCRE, Perl, Python, Ruby)(Command(Conditional(Indexed(1), Literal('a'), Literal('b'))))
  check("(?(<x>)a)", "", DotNet, PCRE, Perl, Ruby)(Command(Conditional(Named(Angle, "x"), Literal('a'))))
  check("(?('x')a)", "", DotNet, PCRE, Perl, Ruby)(Command(Conditional(Named(Quote, "x"), Literal('a'))))
  check("(?(DEFINE)a)", "", PCRE, Perl)(Command(Conditional(Define, Literal('a'))))
  check("(?(VERSION>=1.2)a)", "", PCRE)(Command(Conditional(Version(lt = true, 1, 2), Literal('a'))))
  check("(?(VERSION=1.2)a)", "", PCRE)(Command(Conditional(Version(lt = false, 1, 2), Literal('a'))))
  check("(?(R)a)", "", PCRE, Perl)(Command(Conditional(RRecursion, Literal('a'))))
  check("(?(R1)a)", "", PCRE, Perl)(Command(Conditional(IndexedRecursion(1), Literal('a'))))
  check("(?(R&x)a)", "", PCRE, Perl)(Command(Conditional(NamedRecursion("x"), Literal('a'))))
  check("(?(x)a)", "", PCRE, Python)(Command(Conditional(Named(Bare, "x"), Literal('a'))))
  check("(?(+1)a)", "", PCRE)(Command(Conditional(Relative(1), Literal('a'))))
  check("(?(-1)a)", "", PCRE)(Command(Conditional(Relative(-1), Literal('a'))))
  check("(?(?=a)b)", "", DotNet, PCRE, Perl)(
    Command(Conditional(LookAround(PositiveLookAhead(Symbolic), Literal('a')), Literal('b')))
  )
  check("(?(*positive_lookahead:a)b)", "", PCRE, Perl)(
    Command(Conditional(LookAround(PositiveLookAhead(Alphabetic), Literal('a')), Literal('b')))
  )
  check("(?(*pla:a)b)", "", PCRE, Perl)(
    Command(Conditional(LookAround(PositiveLookAhead(Abbrev), Literal('a')), Literal('b')))
  )
  check("(?(?!a)b)", "", DotNet, PCRE, Perl)(
    Command(Conditional(LookAround(NegativeLookAhead(Symbolic), Literal('a')), Literal('b')))
  )
  check("(?(*negative_lookahead:a)b)", "", PCRE, Perl)(
    Command(Conditional(LookAround(NegativeLookAhead(Alphabetic), Literal('a')), Literal('b')))
  )
  check("(?(*nla:a)b)", "", PCRE, Perl)(
    Command(Conditional(LookAround(NegativeLookAhead(Abbrev), Literal('a')), Literal('b')))
  )
  check("(?(?<=a)b)", "", DotNet, PCRE, Perl)(
    Command(Conditional(LookAround(PositiveLookBehind(Symbolic), Literal('a')), Literal('b')))
  )
  check("(?(*positive_lookbehind:a)b)", "", PCRE, Perl)(
    Command(Conditional(LookAround(PositiveLookBehind(Alphabetic), Literal('a')), Literal('b')))
  )
  check("(?(*plb:a)b)", "", PCRE, Perl)(
    Command(Conditional(LookAround(PositiveLookBehind(Abbrev), Literal('a')), Literal('b')))
  )
  check("(?(?<!a)b)", "", DotNet, PCRE, Perl)(
    Command(Conditional(LookAround(NegativeLookBehind(Symbolic), Literal('a')), Literal('b')))
  )
  check("(?(*negative_lookbehind:a)b)", "", PCRE, Perl)(
    Command(Conditional(LookAround(NegativeLookBehind(Alphabetic), Literal('a')), Literal('b')))
  )
  check("(?(*nlb:a)b)", "", PCRE, Perl)(
    Command(Conditional(LookAround(NegativeLookBehind(Abbrev), Literal('a')), Literal('b')))
  )
  check("(?(.)b)", "", DotNet, PCRE, Perl)(Command(Conditional(LookAround(Dot), Literal('b'))))
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
  check("(?-x)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(verbose = true)))))
  )
  check("(?-x:a)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(verbose = true)))), Literal('a'))
  )
  check("(?^x)", "", PCRE, Perl)(Command(CommandKind.ResetFlag(FlagSet(verbose = true))))
  check("(?^x:a)", "", PCRE, Perl)(Group(GroupKind.ResetFlag(FlagSet(verbose = true)), Literal('a')))
  check("(?#x)", "", DotNet, PCRE, Perl, Python, Ruby)(Command(Comment("x")))
  check("(?#x\\)y)", "", Python, Ruby)(Command(Comment("x\\)y")))
  check("(?{x})", "", Perl)(Command(InlineCode("x")))
  check("(??{x})", "", Perl)(Command(EmbedCode("x")))
  check("(?C)", "", PCRE)(Command(Callout))
  check("(?C1)", "", PCRE)(Command(CalloutInt(1)))
  check("(?C{x})", "", PCRE)(Command(CalloutString('{', '}', "x")))
  check("(?C{x{{y}}z})", "", PCRE)(Command(CalloutString('{', '}', "x{{y}}z")))
  for (delim <- Seq('`', '\'', '"', '^', '%', '#', '$')) {
    check(s"(?C${delim}x$delim)", "", PCRE)(Command(CalloutString(delim, delim, "x")))
  }
  check("(?i)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None)))
  )
  check("(?m)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(multiline = true), None)))
  )
  check("(?x)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)))
  )
  check("(?J)", "", PCRE)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(dupNames = true), None)))
  )
  error("(?J)", "", DotNet, Java, Perl, Python, Ruby)("Invalid flag at 2")
  check("(?L)", "b", Python)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(localeUpper = true), None)))
  )
  error("(?L)", "", DotNet, Java, PCRE, Perl, Python, Ruby)("Invalid flag at 2")
  check("(?U)", "", Java)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ungreedy = true), None)))
  )
  error("(?U)", "", DotNet, PCRE, Perl, Python, Ruby)("Invalid flag at 2")
  check("(?a)", "", Perl, Python, Ruby)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), None)))
  )
  error("(?a)", "", DotNet, Java, PCRE)("Invalid flag at 2")
  check("(?d)", "", Java, Perl, Ruby)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(hasIndices = true), None)))
  )
  error("(?d)", "", DotNet, PCRE, Python)("Invalid flag at 2")
  check("(?l)", "", Perl)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(localeLower = true), None)))
  )
  error("(?l)", "", DotNet, Java, PCRE, Python, Ruby)("Invalid flag at 2")
  check("(?n)", "", DotNet, PCRE, Perl)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(explicitCapture = true), None)))
  )
  error("(?n)", "", Java, Python, Ruby)("Invalid flag at 2")
  check("(?p)", "", Perl)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(preserve = true), None)))
  )
  error("(?p)", "", DotNet, Java, PCRE, Python, Ruby)("Invalid flag at 2")
  check("(?s)", "", DotNet, Java, PCRE, Perl, Python)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(dotAll = true), None)))
  )
  error("(?s)", "", Ruby)("Invalid flag at 2")
  check("(?u)", "", Java, Perl, Python, Ruby)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(unicode = true), None)))
  )
  error("(?u)", "", DotNet, PCRE)("Invalid flag at 2")
  check("a(?x)b|c", "", DotNet, Java, PCRE, Perl, Python)(
    Disjunction(
      Sequence(Literal('a'), Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None))), Literal('b')),
      Literal('c')
    )
  )
  check("a(?x)b|c", "", Ruby)(
    Sequence(
      Literal('a'),
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None))),
      Disjunction(Literal('b'), Literal('c'))
    )
  )
  check("(?x:a)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)), Literal('a'))
  )
  check(" (?x: a ) ", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Sequence(
      Literal(' '),
      Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)), Literal('a')),
      Literal(' ')
    )
  )
  check("(?x-i)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), Some(FlagSet(ignoreCase = true)))))
  )
  check("(?x-i:a)", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), Some(FlagSet(ignoreCase = true)))), Literal('a'))
  )
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
  check("(a)", "", All: _*)(Group(IndexedCapture, Literal('a')))
  error("(", "", All: _*)("Unclosed group at 1")

  // Caret
  check("^", "", All: _*)(Caret)

  // Dollar
  check("$", "", All: _*)(Dollar)

  // Dot
  check(".", "", All: _*)(Dot)

  // Backslash
  check("\\a", "", DotNet, Java, PCRE, Perl, Python, Ruby)(Backslash(Escape(Single('a'), 0x07)))
  check("\\b", "", All: _*)(Backslash(Assert(Boundary(None))))
  check("\\b{g}", "", Java, Perl)(Backslash(Assert(Boundary(Some(GModifier)))))
  check("\\b{gcb}", "", Perl)(Backslash(Assert(Boundary(Some(GcbModifier)))))
  check("\\b{lb}", "", Perl)(Backslash(Assert(Boundary(Some(LbModifier)))))
  check("\\b{sb}", "", Perl)(Backslash(Assert(Boundary(Some(SbModifier)))))
  check("\\b{wb}", "", Perl)(Backslash(Assert(Boundary(Some(WbModifier)))))
  check("\\cA", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(Backslash(Escape(Control('A'), 0x01)))
  check("\\cz", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(Backslash(Escape(Control('z'), 0x1a)))
  check("\\c@", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(Escape(Control('@'), 0x00)))
  check("\\c_", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(Escape(Control('_'), 0x1f)))
  check("\\d", "", All: _*)(Backslash(EscapeClass(Digit)))
  check("\\e", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(Escape(Single('e'), 0x1b)))
  check("\\f", "", All: _*)(Backslash(Escape(Single('f'), 0x0c)))
  check("\\g1", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Bare), IndexedReference(1))))
  check("\\g+1", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Bare), RelativeReference(1))))
  check("\\g-1", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Bare), RelativeReference(-1))))
  check("\\g{1}", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Curly), IndexedReference(1))))
  check("\\g{+1}", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Curly), RelativeReference(1))))
  check("\\g{-1}", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Curly), RelativeReference(-1))))
  check("\\g{foo}", "", PCRE, Perl)(Backslash(EscapeBackReference(GBackReference(Curly), NamedReference("foo"))))
  check("\\g<1>", "", PCRE, Ruby)(Backslash(EscapeCall(Angle, IndexedReference(1))))
  check("\\g<+1>", "", PCRE, Ruby)(Backslash(EscapeCall(Angle, RelativeReference(1))))
  check("\\g<-1>", "", PCRE, Ruby)(Backslash(EscapeCall(Angle, RelativeReference(-1))))
  check("\\g<foo>", "", PCRE, Ruby)(Backslash(EscapeCall(Angle, NamedReference("foo"))))
  check("\\g'1'", "", PCRE, Ruby)(Backslash(EscapeCall(Quote, IndexedReference(1))))
  check("\\g'+1'", "", PCRE, Ruby)(Backslash(EscapeCall(Quote, RelativeReference(1))))
  check("\\g'-1'", "", PCRE, Ruby)(Backslash(EscapeCall(Quote, RelativeReference(-1))))
  check("\\g'foo'", "", PCRE, Ruby)(Backslash(EscapeCall(Quote, NamedReference("foo"))))
  check("\\h", "", Java, PCRE, Perl)(Backslash(EscapeClass(Horizontal)))
  check("\\h", "", Ruby)(Backslash(EscapeClass(HexDigit)))
  check("\\k<foo>", "", DotNet, Java, PCRE, Perl, Ruby)(
    Backslash(EscapeBackReference(KBackReference(Angle), NamedReference("foo")))
  )
  check("\\k<1>", "", PCRE, Perl, Ruby)(Backslash(EscapeBackReference(KBackReference(Angle), IndexedReference(1))))
  check("\\k<+1>", "", PCRE, Perl, Ruby)(Backslash(EscapeBackReference(KBackReference(Angle), RelativeReference(1))))
  check("\\k<-1>", "", PCRE, Perl, Ruby)(Backslash(EscapeBackReference(KBackReference(Angle), RelativeReference(-1))))
  check("\\k<1+0>", "", Ruby)(
    Backslash(EscapeBackReference(KBackReference(Angle), LeveledReference(IndexedReference(1), 0)))
  )
  check("\\k<1-1>", "", Ruby)(
    Backslash(EscapeBackReference(KBackReference(Angle), LeveledReference(IndexedReference(1), -1)))
  )
  check("\\k<x>", "", JavaScript)(Sequence(Backslash(Unknown('k')), Literal('<'), Literal('x'), Literal('>')))
  check("(?<foo>)\\k<foo>", "", JavaScript)(
    Sequence(
      Group(NamedCapture(Angle, "foo"), Sequence()),
      Backslash(EscapeBackReference(KBackReference(Angle), NamedReference("foo")))
    )
  )
  check("\\k'foo'", "", DotNet, PCRE, Perl, Ruby)(
    Backslash(EscapeBackReference(KBackReference(Quote), NamedReference("foo")))
  )
  check("\\k'1'", "", PCRE, Perl, Ruby)(Backslash(EscapeBackReference(KBackReference(Quote), IndexedReference(1))))
  check("\\k'+1'", "", PCRE, Perl, Ruby)(Backslash(EscapeBackReference(KBackReference(Quote), RelativeReference(1))))
  check("\\k'-1'", "", PCRE, Perl, Ruby)(Backslash(EscapeBackReference(KBackReference(Quote), RelativeReference(-1))))
  check("\\k'1+0'", "", Ruby)(
    Backslash(EscapeBackReference(KBackReference(Quote), LeveledReference(IndexedReference(1), 0)))
  )
  check("\\k'1-1'", "", Ruby)(
    Backslash(EscapeBackReference(KBackReference(Quote), LeveledReference(IndexedReference(1), -1)))
  )
  check("\\l", "", Perl)(Backslash(CaseCommand(SingleLowerCaseCommand)))
  check("\\n", "", All: _*)(Backslash(Escape(Single('n'), 0x0a)))
  check("\\o{010}", "", PCRE, Perl)(Backslash(Escape(Octal, 0x08)))
  check("\\p{Letter}", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(EscapeClass(UnicodeProperty("Letter"))))
  check("\\p{Letter}", "u", JavaScript)(Backslash(EscapeClass(UnicodeProperty("Letter"))))
  check("\\p{Script=Hira}", "", DotNet, Java, PCRE, Perl, Ruby)(
    Backslash(EscapeClass(UnicodePropertyValue("Script", "Hira")))
  )
  check("\\p{Script=Hira}", "u", JavaScript)(Backslash(EscapeClass(UnicodePropertyValue("Script", "Hira"))))
  check("\\pL", "", Java, PCRE, Perl)(Backslash(EscapeClass(UnicodeBareProperty("L"))))
  check("\\r", "", All: _*)(Backslash(Escape(Single('r'), 0x0d)))
  check("\\s", "", All: _*)(Backslash(EscapeClass(Space)))
  check("\\t", "", All: _*)(Backslash(Escape(Single('t'), 0x09)))
  check("\\u", "", Perl)(Backslash(CaseCommand(SingleUpperCaseCommand)))
  check("\\uABCD", "", DotNet, Java, JavaScript, Python, Ruby)(Backslash(Escape(UnicodeHex4, 0xabcd)))
  check("\\u{ABCDE}", "u", JavaScript)(Backslash(Escape(UnicodeBracket, 0xabcde)))
  check("\\uA", "", JavaScript, Ruby)(Sequence(Backslash(Unknown('u')), Literal('A')))
  check("\\v", "", Java, PCRE, Perl)(Backslash(EscapeClass(Vertical)))
  check("\\v", "", DotNet, JavaScript, Python, Ruby)(Backslash(Escape(Single('v'), 0x0b)))
  check("\\w", "", All: _*)(Backslash(EscapeClass(Word)))
  check("\\xAB", "", All: _*)(Backslash(Escape(Hex2, 0xab)))
  check("\\xA", "", PCRE, Perl, Ruby)(Backslash(Escape(Hex1, 0x0a)))
  check("\\x{ABCD}", "", Java, PCRE, Perl)(Backslash(Escape(HexBracket, 0xabcd)))
  check("\\z", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(Assert(LowerEnd)))
  check("\\B", "", All: _*)(Backslash(Assert(NonBoundary(None))))
  check("\\B{g}", "", Perl)(Backslash(Assert(NonBoundary(Some(GModifier)))))
  check("\\B{gcb}", "", Perl)(Backslash(Assert(NonBoundary(Some(GcbModifier)))))
  check("\\B{lb}", "", Perl)(Backslash(Assert(NonBoundary(Some(LbModifier)))))
  check("\\B{wb}", "", Perl)(Backslash(Assert(NonBoundary(Some(WbModifier)))))
  check("\\B{sb}", "", Perl)(Backslash(Assert(NonBoundary(Some(SbModifier)))))
  check("\\D", "", All: _*)(Backslash(EscapeClass(NonDigit)))
  check("\\E", "", Java, PCRE, Perl)(Backslash(CaseCommand(EndCaseCommand)))
  check("\\F", "", Perl)(Backslash(CaseCommand(FoldCaseCommand)))
  check("\\G", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(Assert(Sticky)))
  check("\\H", "", Java, PCRE, Perl)(Backslash(EscapeClass(NonHorizontal)))
  check("\\H", "", Ruby)(Backslash(EscapeClass(NonHexDigit)))
  check("\\K", "", PCRE, Perl, Ruby)(Backslash(Assert(Cut)))
  check("\\L", "", Perl)(Backslash(CaseCommand(LowerCaseCommand)))
  check("\\P{Letter}", "", DotNet, Java, PCRE, Perl, Ruby)(Backslash(EscapeClass(NonUnicodeProperty("Letter"))))
  check("\\P{Letter}", "u", JavaScript)(Backslash(EscapeClass(NonUnicodeProperty("Letter"))))
  check("\\P{Script=Hira}", "", DotNet, Java, PCRE, Perl, Ruby)(
    Backslash(EscapeClass(NonUnicodePropertyValue("Script", "Hira")))
  )
  check("\\P{Script=Hira}", "u", JavaScript)(Backslash(EscapeClass(NonUnicodePropertyValue("Script", "Hira"))))
  check("\\PL", "", Java, PCRE, Perl)(Backslash(EscapeClass(NonUnicodeBareProperty("L"))))
  check("\\Qab\\E", "", Java, PCRE, Perl)(
    Sequence(
      Backslash(CaseCommand(QuoteCommand)),
      Literal('a'),
      Literal('b'),
      Backslash(CaseCommand(EndCaseCommand))
    )
  )
  check("\\Qab", "", Java, PCRE, Perl)(
    Sequence(
      Backslash(CaseCommand(QuoteCommand)),
      Literal('a'),
      Literal('b')
    )
  )
  check("\\Q\\\\\\E", "", Java, PCRE, Perl)(
    Sequence(
      Backslash(CaseCommand(QuoteCommand)),
      Literal('\\'),
      Literal('\\'),
      Backslash(CaseCommand(EndCaseCommand))
    )
  )
  check("\\R", "", Java, PCRE, Perl, Ruby)(Backslash(EscapeClass(GeneralNewline)))
  check("\\S", "", All: _*)(Backslash(EscapeClass(NonSpace)))
  check("\\U000ABCDE", "", Python)(Backslash(Escape(UnicodeHex8, 0xabcde)))
  check("\\V", "", Java, PCRE, Perl)(Backslash(EscapeClass(NonVertical)))
  check("\\W", "", All: _*)(Backslash(EscapeClass(NonWord)))
  check("\\X", "", Java, PCRE, Perl)(Backslash(EscapeClass(GraphemeCluster)))
  check("\\Z", "", DotNet, Java, PCRE, Perl, Python, Ruby)(Backslash(Assert(UpperEnd)))
  check("\\010", "", All: _*)(Backslash(Escape(BareOctal, 8)))
  check("\\412", "", JavaScript)(Sequence(Backslash(Escape(BareOctal, 33)), Literal('2')))
  check("()\\1", "", All: _*)(
    Sequence(Group(IndexedCapture, Sequence()), Backslash(EscapeBackReference(BareBackReference, IndexedReference(1))))
  )
  check("\\1", "", DotNet, Java, PCRE, Perl, Python, Ruby)(
    Backslash(EscapeBackReference(BareBackReference, IndexedReference(1)))
  )
  check("\\1", "u", JavaScript)(Backslash(EscapeBackReference(BareBackReference, IndexedReference(1))))
  for (c <- "/|+*?{}()^$.\\[]") {
    check(s"\\$c", "", All: _*)(Backslash(Escape(Single(c), c.toInt)))
  }
  check("\\y", "", JavaScript, Perl, Ruby)(Backslash(Unknown('y')))
  check("\\:", "", All: _*)(Backslash(Unknown(':')))

  // Class
  check("[a]", "", All: _*)(Class(false, ClassLiteral('a')))
  check("[^a]", "", All: _*)(Class(true, ClassLiteral('a')))
  check("[a-z]", "", All: _*)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[a-]", "", All: _*)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'))))
  check("[-z]", "", All: _*)(Class(false, ClassUnion(ClassLiteral('-'), ClassLiteral('z'))))
  check("[\\n-\\r]", "", All: _*)(
    Class(
      false,
      ClassRange(ClassBackslashValue(Escape(Single('n'), '\n')), ClassBackslashValue(Escape(Single('r'), '\r')))
    )
  )
  check("[a-\\w]", "", All: _*)(
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassBackslashClass(Word)))
  )
  check("[\\w-z]", "", All: _*)(
    Class(false, ClassUnion(ClassBackslashClass(Word), ClassLiteral('-'), ClassLiteral('z')))
  )
  check("[\\b]", "", All: _*)(Class(false, ClassBackslashValue(Escape(Single('b'), 0x08))))
  check("[]", "", JavaScript)(Class(false, ClassUnion()))
  check("[[:alnum:]]", "", PCRE, Perl, Ruby)(Class(false, ClassPosix(false, "alnum")))
  check("[[:^alnum:]]", "", PCRE, Perl, Ruby)(Class(false, ClassPosix(true, "alnum")))
  check("[[a]]", "", Java, Ruby)(Class(false, ClassNest(false, ClassLiteral('a'))))
  check("[[a]]", "v", JavaScript)(Class(false, ClassNest(false, ClassLiteral('a'))))
  check("[[^a]]", "", Java, Ruby)(Class(false, ClassNest(true, ClassLiteral('a'))))
  check("[[^a]]", "v", JavaScript)(Class(false, ClassNest(true, ClassLiteral('a'))))
  check("[[a-z]&&[a-c]]", "", Java, Ruby)(
    Class(
      false,
      ClassIntersection(
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))),
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('c')))
      )
    )
  )
  check("[[a-z]&&[a-c]]", "v", JavaScript)(
    Class(
      false,
      ClassIntersection(
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))),
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('c')))
      )
    )
  )
  check("[[a-z]--[a-c]]", "v", JavaScript)(
    Class(
      false,
      ClassDiff(
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))),
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('c')))
      )
    )
  )
  check("[\\q{abc|def}]", "v", JavaScript)(
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
  )
  check("[\\q{\\n}]", "v", JavaScript)(
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
  )
  error("[", "", All: _*)("Unclosed ']' at 1")
  error("]", "u", JavaScript)("Unclosed ']' at 0")
}
