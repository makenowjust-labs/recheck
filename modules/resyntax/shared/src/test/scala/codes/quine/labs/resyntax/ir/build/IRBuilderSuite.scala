package codes.quine.labs.resyntax.ir
package build

import scala.language.implicitConversions

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BackReferenceStyle
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackControlKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.BoundaryModifier
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.NameStyle
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Reference
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.Dialect.*
import codes.quine.labs.resyntax.ir.IRLookBehindMatch.Forward
import codes.quine.labs.resyntax.ir.IRNodeData.*
import codes.quine.labs.resyntax.ir.IRQuantifier.*
import codes.quine.labs.resyntax.ir.build.IRClassLookup.ASCIIWord
import codes.quine.labs.resyntax.ir.build.IRHelper.given
import codes.quine.labs.resyntax.parser.Parser

class IRBuilderSuite extends munit.FunSuite with IRHelper:

  // Disjunction
  check("|", "", All*)(Disjunction(Empty, Empty))

  // Sequence
  check("", "", All*)(Empty)
  check("(?:)(?:)", "", All*)(Sequence(Empty, Empty))
  check("ab", "", All*)(Sequence(char('a'), char('b')))

  // Blank
  check(" ", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Empty)
  check(" a # b", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(char('a'))
  check("a b", "x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Sequence(char('a'), char('b')))
  check("[ a]", "x", Java)(char('a'))
  check("[a b]", "x", Java)(Chars(CharSet.of('a', 'b')))
  check("[a&& ]", "x", Java)(Chars(CharSet.empty))

  // Repeat
  check("(?:)*", "", All*)(Repeat(Empty, Unbounded(0, Greedy)))
  check("(?:)*?", "", All*)(Repeat(Empty, Unbounded(0, Lazy)))
  check("(?:)*+", "", Java, PCRE, Perl(), Python(), Ruby())(Atomic(Repeat(Empty, Unbounded(0, Greedy))))
  check("(?:)+", "", All*)(Repeat(Empty, Unbounded(1, Greedy)))
  check("(?:)+?", "", All*)(Repeat(Empty, Unbounded(1, Lazy)))
  check("(?:)++", "", Java, PCRE, Perl(), Python(), Ruby())(Atomic(Repeat(Empty, Unbounded(1, Greedy))))
  check("(?:)?", "", All*)(Repeat(Empty, Bounded(0, 1, Greedy)))
  check("(?:)??", "", All*)(Repeat(Empty, Bounded(0, 1, Lazy)))
  check("(?:)?+", "", Java, PCRE, Perl(), Python(), Ruby())(Atomic(Repeat(Empty, Bounded(0, 1, Greedy))))
  check("(?:){4}", "", All*)(Repeat(Empty, Exact(4)))
  check("(?:){4}?", "", DotNet, Java, JavaScript, PCRE, Perl(), Python())(Repeat(Empty, Exact(4)))
  check("(?:){4}?", "", Ruby())(Repeat(Repeat(Empty, Exact(4)), Bounded(0, 1, Greedy)))
  // `^(?:a|ab){1}+c` does not match `abc` in Java, PCRE, Perl and Python, so a possessive exact count is atomic.
  check("(?:){4}+", "", Java, PCRE, Perl(), Python())(Atomic(Repeat(Empty, Exact(4))))
  check("(?:){4}+", "", Ruby())(Repeat(Repeat(Empty, Exact(4)), Unbounded(1, Greedy)))
  check("(?:){2,3}", "", All*)(Repeat(Empty, Bounded(2, 3, Greedy)))
  check("(?:){2,2}", "", All*)(Repeat(Empty, Exact(2)))
  check("(?:){2,3}?", "", All*)(Repeat(Empty, Bounded(2, 3, Lazy)))
  check("(?:){2,2}?", "", All*)(Repeat(Empty, Exact(2)))
  check("(?:){2,3}+", "", Java, PCRE, Perl(), Python())(Atomic(Repeat(Empty, Bounded(2, 3, Greedy))))
  checkPattern("(?:(a)b)*\\1", "", All*): pattern =>
    val Sequence(Seq(IRNode(Repeat(body, _), _), _)) = pattern.node.data: @unchecked
    assert(body.data.isInstanceOf[Sequence], body)
  // `^(?:a|ab){0,1}+c` matches `abc` in Ruby, so `+` after an interval is another repeat.
  check("(?:){2,3}+", "", Ruby())(Repeat(Repeat(Empty, Bounded(2, 3, Greedy)), Unbounded(1, Greedy)))
  check("(?:){2,2}+", "", Java, PCRE, Perl(), Python())(Atomic(Repeat(Empty, Exact(2))))
  check("(?:){,1}", "", PCRE, Perl(), Python(), Ruby())(Repeat(Empty, Bounded(0, 1, Greedy)))
  check("(?:){,0}", "", PCRE, Perl(), Python(), Ruby())(Repeat(Empty, Exact(0)))
  check("(?:){,1}?", "", PCRE, Perl(), Python(), Ruby())(Repeat(Empty, Bounded(0, 1, Lazy)))
  check("(?:){,0}?", "", PCRE, Perl(), Python(), Ruby())(Repeat(Empty, Exact(0)))
  check("(?:){,1}+", "", PCRE, Perl(), Python())(Atomic(Repeat(Empty, Bounded(0, 1, Greedy))))
  check("(?:){,1}+", "", Ruby())(Repeat(Repeat(Empty, Bounded(0, 1, Greedy)), Unbounded(1, Greedy)))
  check("(?:){2,}", "", All*)(Repeat(Empty, Unbounded(2, Greedy)))
  check("(?:){2,}?", "", All*)(Repeat(Empty, Unbounded(2, Lazy)))
  check("(?:){2,}+", "", Java, PCRE, Perl(), Python())(Atomic(Repeat(Empty, Unbounded(2, Greedy))))

  // PCRE's `U` swaps greedy and lazy: `^(?U)a+` matches `a` and `^(?U)a+?` matches `aaa` in `aaa`.
  check("(?:)*", "U", PCRE)(Repeat(Empty, Unbounded(0, Lazy)))
  check("(?:)*?", "U", PCRE)(Repeat(Empty, Unbounded(0, Greedy)))
  check("(?:)*+", "U", PCRE)(Atomic(Repeat(Empty, Unbounded(0, Greedy))))
  check("(?U)(?:)*(?-U)(?:)*", "", PCRE):
    Sequence(Empty, Repeat(Empty, Unbounded(0, Lazy)), Empty, Repeat(Empty, Unbounded(0, Greedy)))
  check("(?U:(?:)*)(?:)*", "", PCRE)(Sequence(Repeat(Empty, Unbounded(0, Lazy)), Repeat(Empty, Unbounded(0, Greedy))))
  // `^(?U)(?^)a+` matches `a` in `aaa`, so `(?^)` keeps `U`.
  check("(?U)(?^)(?:)*", "", PCRE)(Sequence(Empty, Empty, Repeat(Empty, Unbounded(0, Lazy))))
  // Java's `U` is UNICODE_CHARACTER_CLASS: `^a+` matches `aaa` with it.
  check("(?:)*", "U", Java)(Repeat(Empty, Unbounded(0, Greedy)))

  // Group
  check("()", "", All*)(Capture(1, None, Empty))
  check("(?:)", "", All*)(Empty)
  check("(?<x>)", "", DotNet, Java, JavaScript, PCRE, Perl(), Ruby())(Capture(1, Some("x"), Empty))
  check("(?'x')", "", DotNet, PCRE, Perl(), Ruby())(Capture(1, Some("x"), Empty))
  check("(?P<x>)", "", PCRE, Perl(), Python())(Capture(1, Some("x"), Empty))
  check("(?=a)", "", All*)(LookAhead(false, char('a')))
  check("(?!a)", "", All*)(LookAhead(true, char('a')))
  check("(?<=a)", "", DotNet, JavaScript, Ruby())(LookBehind(false, char('a')))
  check("(?<=a)", "", PCRE, Perl(), Python())(LookBehind(false, char('a'), Forward(Seq(IRLookBehindStartRange(1, 1)))))
  check("(?<=a)", "", Java)(LookBehind(false, char('a'), Forward(Seq(IRLookBehindStartRange(1, 1)))))
  check("(?<!a)", "", DotNet, JavaScript, Ruby())(LookBehind(true, char('a')))
  check("(?<!a)", "", PCRE, Perl(), Python())(LookBehind(true, char('a'), Forward(Seq(IRLookBehindStartRange(1, 1)))))
  check("(?<!a)", "", Java)(LookBehind(true, char('a'), Forward(Seq(IRLookBehindStartRange(1, 1)))))
  check("(*pla:a)", "", PCRE, Perl())(LookAhead(false, char('a')))
  check("(*negative_lookbehind:a)", "", PCRE, Perl())(
    LookBehind(true, char('a'), Forward(Seq(IRLookBehindStartRange(1, 1))))
  )
  check("(?>a)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Atomic(char('a')))
  check("(*atomic:a)", "", PCRE, Perl())(Atomic(char('a')))
  check("(?<x>)(?<-x>)", "", DotNet)(
    Sequence(
      Capture(1, Some("x"), Empty),
      Unsupported(NodeData.Group(GroupKind.Balance(NameStyle.Angle, None, "x"), NodeData.Sequence()))
    )
  )
  check("(?*a)", "", PCRE)(NonAtomicLookAhead(IRNode(char('a'))))
  check("(*napla:a)", "", PCRE)(NonAtomicLookAhead(IRNode(char('a'))))
  check("(?<*a)", "", PCRE)(NonAtomicLookBehind(IRNode(char('a')), Forward(Seq(IRLookBehindStartRange(1, 1)))))
  check("(*non_atomic_positive_lookbehind:a)", "", PCRE)(
    NonAtomicLookBehind(IRNode(char('a')), Forward(Seq(IRLookBehindStartRange(1, 1))))
  )
  check("(*script_run:)", "", PCRE, Perl())(
    Unsupported(NodeData.Group(GroupKind.ScriptRun(AssertNameStyle.Alphabetic), NodeData.Sequence()))
  )
  check("(*atomic_script_run:)", "", PCRE, Perl())(
    Unsupported(NodeData.Group(GroupKind.AtomicScriptRun(AssertNameStyle.Alphabetic), NodeData.Sequence()))
  )
  check("(?~)", "", Ruby())(Unsupported(NodeData.Group(GroupKind.Absence, NodeData.Sequence())))

  // Command
  check("(?R)", "", PCRE, Perl())(Unsupported(NodeData.Command(CommandKind.RCall)))
  check("()(?1)", "", PCRE, Perl())(
    Sequence(Capture(1, None, Empty), Unsupported(NodeData.Command(CommandKind.IndexedCall(1))))
  )
  check("()(?-1)", "", PCRE, Perl())(
    Sequence(Capture(1, None, Empty), Unsupported(NodeData.Command(CommandKind.RelativeCall(-1))))
  )
  check("(?<x>)(?&x)", "", PCRE, Perl())(
    Sequence(Capture(1, Some("x"), Empty), Unsupported(NodeData.Command(CommandKind.NamedCall("x"))))
  )
  check("(?P<x>)(?P>x)", "", PCRE, Perl())(
    Sequence(Capture(1, Some("x"), Empty), Unsupported(NodeData.Command(CommandKind.PNamedCall("x"))))
  )
  check("()\\g<1>", "", PCRE, Ruby())(
    Sequence(
      Capture(1, None, Empty),
      Unsupported(NodeData.Backslash(BackslashKind.EscapeCall(NameStyle.Angle, Reference.IndexedReference(1))))
    )
  )
  check("(?#)", "", DotNet, PCRE, Perl(), Python(), Ruby())(Empty)
  check("(?{})", "", Perl())(Unsupported(NodeData.Command(CommandKind.InlineCode(""))))
  check("(??{})", "", Perl())(Unsupported(NodeData.Command(CommandKind.EmbedCode(""))))
  check("(?C)", "", PCRE)(Unsupported(NodeData.Command(CommandKind.Callout)))
  check("(?C{x})", "", PCRE)(Unsupported(NodeData.Command(CommandKind.CalloutString('{', '}', "x"))))
  check("(?C1)", "", PCRE)(Unsupported(NodeData.Command(CommandKind.CalloutInt(1))))
  check("(?|)", "", PCRE, Perl())(Empty)
  check("(?|(a)|(b)(c))", "", PCRE, Perl())(
    Disjunction(Capture(1, None, char('a')), Sequence(Capture(1, None, char('b')), Capture(2, None, char('c'))))
  )

  // Conditionals
  check("()(?(1)a|b)", "", DotNet, PCRE, Perl(), Python(), Ruby())(
    Sequence(Capture(1, None, Empty), Conditional(IRCondition.Captured(Seq(1)), char('a'), char('b')))
  )
  check("()(?(1)a)", "", DotNet, PCRE, Perl(), Python(), Ruby())(
    Sequence(Capture(1, None, Empty), Conditional(IRCondition.Captured(Seq(1)), char('a'), Empty))
  )
  check("(?<n>)(?(<n>)a|b)", "", PCRE, Perl(), Ruby())(
    Sequence(Capture(1, Some("n"), Empty), Conditional(IRCondition.Captured(Seq(1)), char('a'), char('b')))
  )
  check("(?<n>)(?('n')a|b)", "", PCRE, Perl(), Ruby())(
    Sequence(Capture(1, Some("n"), Empty), Conditional(IRCondition.Captured(Seq(1)), char('a'), char('b')))
  )
  check("(?P<n>)(?(n)a|b)", "", PCRE, Python())(
    Sequence(Capture(1, Some("n"), Empty), Conditional(IRCondition.Captured(Seq(1)), char('a'), char('b')))
  )
  check("()(?(-1)a|b)()(?(+1)c)()", "", PCRE)(
    Sequence(
      Capture(1, None, Empty),
      Conditional(IRCondition.Captured(Seq(1)), char('a'), char('b')),
      Capture(2, None, Empty),
      Conditional(IRCondition.Captured(Seq(3)), char('c'), Empty),
      Capture(3, None, Empty)
    )
  )
  check("()(?(<1>)a|b)", "", Ruby())(
    Sequence(Capture(1, None, Empty), Conditional(IRCondition.Captured(Seq(1)), char('a'), char('b')))
  )
  // A name shared by groups holds if one of them is set, but Ruby tests only the first one.
  check("(?J)(?<n>)(?<n>)(?(<n>)a)", "", PCRE)(
    Sequence(
      Empty,
      Capture(1, Some("n"), Empty),
      Capture(2, Some("n"), Empty),
      Conditional(IRCondition.Captured(Seq(1, 2)), char('a'), Empty)
    )
  )
  check("(?<n>)(?<n>)(?(<n>)a)", "", Perl())(
    Sequence(
      Capture(1, Some("n"), Empty),
      Capture(2, Some("n"), Empty),
      Conditional(IRCondition.Captured(Seq(1, 2)), char('a'), Empty)
    )
  )
  check("(?<n>)(?<n>)(?(<n>)a)", "", Ruby())(
    Sequence(
      Capture(1, Some("n"), Empty),
      Capture(2, Some("n"), Empty),
      Conditional(IRCondition.Captured(Seq(1)), char('a'), Empty)
    )
  )
  // Perl's test of a missing group never holds, and .NET's group 0 is not set during the match.
  check("(?(2)a|b)", "", Perl())(char('b'))
  check("(?(0)a|b)", "", DotNet)(char('b'))
  // .NET reads a group name as a capture test, and any other text as a look-ahead.
  check("(?<x>)(?(x)a|b)", "", DotNet)(
    Sequence(Capture(1, Some("x"), Empty), Conditional(IRCondition.Captured(Seq(1)), char('a'), char('b')))
  )
  check("(?(x)a|b)", "", DotNet)(Conditional(IRCondition.LookAhead(false, char('x')), char('a'), char('b')))
  check("(?([x])a|b)", "", DotNet)(Conditional(IRCondition.LookAhead(false, char('x')), char('a'), char('b')))
  check("(?(?=x)a|b)", "", DotNet, PCRE, Perl())(
    Conditional(IRCondition.LookAhead(false, char('x')), char('a'), char('b'))
  )
  check("(?(?!x)a|b)", "", DotNet, PCRE, Perl())(
    Conditional(IRCondition.LookAhead(true, char('x')), char('a'), char('b'))
  )
  check("(?(?<=x)a|b)", "", DotNet)(
    Conditional(IRCondition.LookBehind(false, char('x')), char('a'), char('b'))
  )
  // An inline flag in a branch lasts until the end of the conditional, also in the next branch.
  check("(?(?=x)(?i)a|b)c", "", DotNet, PCRE, Perl())(
    Sequence(
      Conditional(
        IRCondition.LookAhead(false, char('x')),
        Sequence(Empty, Chars(CharSet.of('A', 'a'))),
        Chars(CharSet.of('B', 'b'))
      ),
      char('c')
    )
  )
  check("(?(?<=x)a|b)", "", PCRE, Perl())(
    Conditional(
      IRCondition.LookBehind(false, char('x'), Forward(Seq(IRLookBehindStartRange(1, 1)))),
      char('a'),
      char('b')
    )
  )
  check("(?(*nlb:x)a|b)", "", PCRE, Perl())(
    Conditional(
      IRCondition.LookBehind(true, char('x'), Forward(Seq(IRLookBehindStartRange(1, 1)))),
      char('a'),
      char('b')
    )
  )
  check("(?(?C1)(?=x)a|b)", "", PCRE)(
    Unsupported(
      NodeData.Command(
        CommandKind.Conditional(
          ConditionalTest.CalloutLookAround(
            CommandKind.CalloutInt(1),
            ConditionalTest.LookAround(GroupKind.PositiveLookAhead(AssertNameStyle.Symbolic), NodeData.Literal('x'))
          ),
          NodeData.Literal('a'),
          NodeData.Literal('b')
        )
      )
    )
  )
  // A constant condition leaves the chosen branch: PCRE2 10.49 is the version.
  check("(?(VERSION>=10.49)a|b)", "", PCRE)(char('a'))
  check("(?(VERSION>=10.5)a|b)", "", PCRE)(char('a'))
  check("(?(VERSION>=10.50)a|b)", "", PCRE)(char('b'))
  check("(?(VERSION=10.49)a|b)", "", PCRE)(char('a'))
  check("(?(VERSION=10)a|b)", "", PCRE)(char('b'))
  check("(?(DEFINE)(a))b", "", PCRE, Perl())(Sequence(Empty, char('b')))
  check("(?(R)a|b)", "", PCRE, Perl())(char('b'))
  check("()(?(R1)a|b)", "", PCRE, Perl())(Sequence(Capture(1, None, Empty), char('b')))
  check("(?<x>)(?(R&x)a|b)", "", PCRE, Perl())(Sequence(Capture(1, Some("x"), Empty), char('b')))
  // PCRE reads `(R1)` as a capture test when a group has the name.
  check("(?<R1>)(?(R1)a|b)", "", PCRE)(
    Sequence(Capture(1, Some("R1"), Empty), Conditional(IRCondition.Captured(Seq(1)), char('a'), char('b')))
  )
  // Recursion tests and `(DEFINE)` depend on calls, which are unsupported.
  check("(?(R)a|b)(?R)", "", PCRE)(
    Sequence(
      Unsupported(
        NodeData.Command(
          CommandKind.Conditional(ConditionalTest.RRecursion, NodeData.Literal('a'), NodeData.Literal('b'))
        )
      ),
      Unsupported(NodeData.Command(CommandKind.RCall))
    )
  )
  check("(?(DEFINE)(a))(?1)", "", PCRE)(
    Sequence(
      Unsupported(
        NodeData.Command(
          CommandKind
            .Conditional(ConditionalTest.Define, NodeData.Group(GroupKind.IndexedCapture, NodeData.Literal('a')))
        )
      ),
      Unsupported(NodeData.Command(CommandKind.IndexedCall(1)))
    )
  )
  check("(?<x>)(?(<x+0>)a)", "", Ruby())(
    Sequence(
      Capture(1, Some("x"), Empty),
      Unsupported(
        NodeData.Command(
          CommandKind.Conditional(
            ConditionalTest.Referenced(NameStyle.Angle, Reference.LeveledReference(Reference.NamedReference("x"), 0)),
            NodeData.Literal('a')
          )
        )
      )
    )
  )

  // Backtracking control verbs
  check("(*FAIL)", "", PCRE, Perl())(Chars(CharSet.empty))
  check("(*F)", "", PCRE, Perl())(Chars(CharSet.empty))
  check("(*FAIL:x)", "", PCRE, Perl())(Chars(CharSet.empty))
  check("(*ACCEPT)", "", PCRE, Perl())(BacktrackControl(IRBacktrackControl.Accept))
  check("(*ACCEPT:x)", "", PCRE, Perl())(BacktrackControl(IRBacktrackControl.Accept))
  check("(*COMMIT)", "", PCRE, Perl())(BacktrackControl(IRBacktrackControl.Commit))
  check("(*PRUNE:x)", "", PCRE, Perl())(BacktrackControl(IRBacktrackControl.Prune))
  check("(*SKIP)", "", PCRE, Perl())(BacktrackControl(IRBacktrackControl.Skip(None)))
  check("(*SKIP:x)", "", PCRE, Perl())(BacktrackControl(IRBacktrackControl.Skip(Some("x"))))
  check("(*THEN)", "", PCRE, Perl())(BacktrackControl(IRBacktrackControl.Then))
  check("(*MARK:x)", "", PCRE, Perl())(BacktrackControl(IRBacktrackControl.Mark("x")))
  check("(*:x)", "", PCRE, Perl())(BacktrackControl(IRBacktrackControl.Mark("x")))
  check("(?~|a|b)", "", Ruby())(
    Unsupported(NodeData.Command(CommandKind.AbsenceExpression(NodeData.Literal('a'), NodeData.Literal('b'))))
  )
  check("(?~|a)", "", Ruby())(Unsupported(NodeData.Command(CommandKind.AbsenceStopper(NodeData.Literal('a')))))
  check("(?~|)", "", Ruby())(Unsupported(NodeData.Command(CommandKind.AbsenceRangeClear)))
  check("(*CRLF)(*UTF)(*LIMIT_MATCH=1)", "", PCRE)(Sequence(Empty, Empty, Empty))
  check("(*NOTEMPTY)", "", PCRE)(Empty)

  // Caret
  check("^", "", DotNet, Java, JavaScript, PCRE, Perl(), Python())(Assert(IRAssertKind.TextBegin))

  // Backslash assertions
  check("\\A", "", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Assert(IRAssertKind.TextBegin))
  check("\\z", "", DotNet, Java, PCRE, Perl(), Ruby())(Assert(IRAssertKind.TextEnd))
  // `a\Z` does not match `a\n` in Python, where `\Z` is the end of the text.
  check("\\Z", "", Python())(Assert(IRAssertKind.TextEnd))
  check("\\b", "", JavaScript, PCRE)(Assert(IRAssertKind.WordBoundary(ASCIIWord)))
  check("\\B", "", JavaScript, PCRE)(Assert(IRAssertKind.NotWordBoundary(ASCIIWord)))
  // .NET's `e` takes `İ` as a word character too.
  check("\\b", "e", DotNet)(Assert(IRAssertKind.WordBoundary(ASCIIWord.union(CharSet.single(0x130)))))
  // Java's `\b` without `U` takes a non-spacing mark after a letter or a digit as a word character.
  locally:
    val marks = IRClassLookup.javaWordMarks
    check("\\b", "", Java)(Assert(IRAssertKind.MarkWordBoundary(ASCIIWord, marks)))
    check("\\B", "", Java)(Assert(IRAssertKind.NotMarkWordBoundary(ASCIIWord, marks)))
    test("IRBuilder.build: Java's word marks"):
      assert(marks.marks.contains(0x0301) && marks.marks.contains(0x1d167) && !marks.extenders.contains(0x1d167))
      assert(marks.bases.contains('a') && marks.bases.contains(0x0660) && !marks.bases.contains('_'))
      assert(!marks.bases.contains(0x1d400) && !marks.bases.contains(0x2160))
  checkWith("\\b", "U", Java)(_ => Assert(IRAssertKind.WordBoundary(IRClassLookup.Java.word(flagsOf(Java, "U")))))
  // ECMAScript's WordCharacters with `u` and `i` includes U+017F and U+212A, which fold to `s` and `k`.
  check("\\b", "iu", JavaScript)(Assert(IRAssertKind.WordBoundary(ASCIIWord.union(CharSet.of(0x17f, 0x212a)))))
  check("\\b{wb}", "", Perl())(
    Unsupported(NodeData.Backslash(BackslashKind.Assert(AssertKind.Boundary(Some(BoundaryModifier.WbModifier)))))
  )
  check("\\B{gcb}", "", Perl())(
    Unsupported(
      NodeData.Backslash(BackslashKind.Assert(AssertKind.NonBoundary(Some(BoundaryModifier.GcbModifier))))
    )
  )
  check("\\b{g}", "", Java)(
    Unsupported(NodeData.Backslash(BackslashKind.Assert(AssertKind.Boundary(Some(BoundaryModifier.GModifier)))))
  )
  // The checkers search once from the start of the input, where `\G` is.
  check("\\G", "", DotNet, Java, PCRE, Perl(), Ruby())(Assert(IRAssertKind.TextBegin))
  check("\\K", "", PCRE, Perl(), Ruby())(ResetMatchStart)

  // Back-reference
  /** Returns a back-reference to the capture `1` at the flags, where Perl's `/d` is `/u` under Unicode rules. */
  def referenceOf(d: Dialect, flags: String = "", unicode: Boolean = true): IRNodeData =
    val charFlags = flagsOf(d, flags)
    val referenceFlags = if d.isPerl && unicode then charFlags.copy(charset = IRCharset.Unicode) else charFlags
    val flagSet = FlagSet.parse(flags, d)
    val mode = Parser.determineUnitMode("", flagSet, d)
    val featureSet = IRFeatureSet.from(flagSet, d, Seq.empty)
    BackReference(
      Seq(1),
      IRCharResolver.determineBackReferenceCaseEquivalence(featureSet.caseEquivalenceKind, mode, referenceFlags),
      IRCharResolver.determineBackReferenceFolding(featureSet.multiCharFoldingKind, mode, referenceFlags)
    )

  checkWith("(a)\\1", "", All*)(d => Sequence(Capture(1, None, char('a')), referenceOf(d)))
  checkWith("(a)(?i)\\1", "", DotNet, Java, PCRE, Perl(), Ruby()): d =>
    Sequence(Capture(1, None, char('a')), Empty, referenceOf(d, "i"))
  checkWith("(a)\\1", "", Perl(utf8 = false))(d =>
    Sequence(Capture(1, None, char('a')), referenceOf(d, unicode = false))
  )
  // A back-reference just after a look-ahead of its capture reads what it has captured, as an atomic group does.
  check("x(?=(a+))\\1", "", All*)(
    Sequence(char('x'), Atomic(Capture(1, None, Repeat(char('a'), Unbounded(1, Greedy)))))
  )
  checkWith("(?=(a))\\1\\1", "", All*)(d =>
    Sequence(LookAhead(false, Capture(1, None, char('a'))), referenceOf(d), referenceOf(d))
  )
  checkWith("(?=(a))(?=\\1)", "", All*)(d =>
    Sequence(LookAhead(false, Capture(1, None, char('a'))), LookAhead(false, referenceOf(d)))
  )
  checkPattern("(a)(?i)\\1", "", Perl(), Ruby()): pattern =>
    val Sequence(Seq(_, _, IRNode(BackReference(_, _, folding), _))) = pattern.node.data: @unchecked
    assertEquals(folding, IRBackReferenceFolding.Full(0, foldsDottedCapitalI = true))
  check("(?<x>a)\\k<x+0>", "", Ruby())(
    Sequence(
      Capture(1, Some("x"), char('a')),
      Unsupported(
        NodeData.Backslash(
          BackslashKind.EscapeBackReference(
            BackReferenceStyle.KBackReference(NameStyle.Angle),
            Reference.LeveledReference(Reference.NamedReference("x"), 0)
          )
        )
      )
    )
  )

  // Ruby's `\u{...}` of several code points is their escapes, and a quantifier after it repeats the last one.
  check("x\\u{41 42}*", "", Ruby())(Sequence(char('x'), char('A'), Repeat(char('B'), Unbounded(0, Greedy))))
  check("(\\u{41 42})", "", Ruby())(Capture(1, None, Sequence(char('A'), char('B'))))
  check("[x\\u{41 42}]", "", Ruby())(Chars(CharSet.of('A', 'B', 'x')))
  check("[\\u{41 42}-D]", "", Ruby())(Chars(CharSet.range('A', 'D')))
  check("[\\u{30 31}-\\u{41 42}&&\\u{31 32}]", "", Ruby())(Chars(CharSet.of('1', '2')))
  // The escapes of bytes make a character with the escapes of `\u{...}` beside them (e.g. `é-é` and `ê`).
  check("[\\xC3\\xA9-\\u{E9 EA}]", "", Ruby())(Chars(CharSet.range(0xe9, 0xea)))

  // Quoting and case commands are their nodes, and a quantifier after `\E` repeats the last one.
  check("\\Qa\\E", "", Java, PCRE, Perl())(char('a'))
  check("x\\Qab\\E*", "", Java, PCRE, Perl())(Sequence(char('x'), char('a'), Repeat(char('b'), Unbounded(0, Greedy))))
  check("a\\Q\\Eb", "", Java, PCRE)(Sequence(char('a'), char('b')))
  check("\\Ua\\E", "", Perl())(char('A'))
  check("\\Uab\\E", "", Perl())(Sequence(char('A'), char('B')))
  check("[\\Qa\\E]", "", Java, PCRE, Perl())(char('a'))
  check("[\\Qab\\E]", "", Java, PCRE, Perl())(Chars(CharSet.of('a', 'b')))
  check("[x\\Qab\\E]", "", Java, PCRE, Perl())(Chars(CharSet.of('a', 'b', 'x')))
  check("[\\Qa\\E-\\Qc\\E]", "", Java, PCRE, Perl())(Chars(CharSet.range('a', 'c')))
  check("[A-\\LC\\E]", "", Perl())(Chars(CharSet.range('A', 'c')))

  // Java's CANON_EQ makes a character-level node unsupported.
  check("a(?c)b", "", Java)(Sequence(char('a'), Empty, Unsupported(NodeData.Literal('b'))))
  check("a", "c", Java)(Unsupported(NodeData.Literal('a')))

  // Pattern
  checkPattern("(a)", "", All*): pattern =>
    assertEquals(pattern.captureCount, 1)
    assertEquals(pattern.anchored, false)
  checkPattern("", "y", JavaScript)(pattern => assertEquals(pattern.anchored, true))
  checkPattern("", "A", PCRE)(pattern => assertEquals(pattern.anchored, true))
  checkPattern("", "", All*)(pattern => assertEquals(pattern.notEmpty, IRNotEmpty.Never))
  checkPattern("(*NOTEMPTY)", "", PCRE)(pattern => assertEquals(pattern.notEmpty, IRNotEmpty.Anywhere))
  checkPattern("(*NOTEMPTY_ATSTART)", "", PCRE)(pattern => assertEquals(pattern.notEmpty, IRNotEmpty.AtStart))
  checkPattern("(*NOTEMPTY)a|b", "", PCRE)(pattern => assertEquals(pattern.notEmpty, IRNotEmpty.Anywhere))
  checkPattern("(*NOTEMPTY_ATSTART)(*NOTEMPTY)", "", PCRE)(pattern =>
    assertEquals(pattern.notEmpty, IRNotEmpty.Anywhere)
  )
  // `(?:(a)|b)\1c` matches `bc` only in JavaScript.
  checkPattern("", "", JavaScript): pattern =>
    assertEquals(pattern.backReference, IRBackReferenceSemantics(true, IRBackReferenceChoice.FirstSet))
  // .NET's `e` matches `(?:(a)|b)\1c` to `bc` too.
  checkPattern("", "e", DotNet): pattern =>
    assertEquals(pattern.backReference, IRBackReferenceSemantics(true, IRBackReferenceChoice.FirstSet))
  checkPattern("", "", DotNet, Java, PCRE, Perl(), Python()): pattern =>
    assertEquals(pattern.backReference, IRBackReferenceSemantics(false, IRBackReferenceChoice.FirstSet))
  // `^(?<n>a)(?<n>ab)\k<n>b` does not match `aabab` in Ruby, which takes `ab` and does not backtrack to `a`.
  checkPattern("", "", Ruby()): pattern =>
    assertEquals(pattern.backReference, IRBackReferenceSemantics(false, IRBackReferenceChoice.LastMatching))

  test("IRBuilder.build: keeps the source location"):
    val pattern = build("a(?:b)*+", "", Java)
    pattern.node.data match
      case Sequence(Seq(a, atomic)) =>
        assertEquals(a.loc, codes.quine.labs.resyntax.ast.SourceLocation(0, 1))
        assertEquals(atomic.loc, codes.quine.labs.resyntax.ast.SourceLocation(1, 8))
        atomic.data match
          case Atomic(repeat) => assertEquals(repeat.loc, atomic.loc)
          case data           => fail(s"Unexpected: $data")
      case data => fail(s"Unexpected: $data")

  test("IRBuilder.build: resolves character-level nodes into character sets"):
    val pattern = IRBuilder.build(Node(NodeData.Literal('a')), FlagSet(), JavaScript, UnitMode.UTF16)
    assertEquals(pattern.node.data, Chars(CharSet.single('a')))

  test("IRBuilder.build: the semantics of the dialect"):
    def patternOf(dialect: Dialect): IRPattern = build("a", "", dialect)

    assert(patternOf(JavaScript).captures.resetsEachIteration)
    assert(patternOf(Perl()).captures.keptOnBacktracking)
    assert(patternOf(Java).captures.heldAfterAtomicMatch)
    assert(patternOf(DotNet).captures.restoredByNegativeCondition)
    assert(patternOf(Ruby()).captures.unsetWhileOpen)
    assertEquals(
      patternOf(PCRE).emptyIteration,
      IREmptyIterationSemantics(IREmptyIteration.ExitFromMin, IREmptyIteration.Ignore, countsReferencedCaptures = false)
    )
    assertEquals(patternOf(Python()).emptyIteration.bounded, IREmptyIteration.ExitOptional)
    assertEquals(
      patternOf(PythonRegex()).emptyIteration,
      IREmptyIterationSemantics(IREmptyIteration.ExitAll, IREmptyIteration.ExitAll, countsReferencedCaptures = true)
    )
    assertEquals(patternOf(Perl()).backtrackControl, IRBacktrackControlSemantics(true, true))
    assertEquals(patternOf(PCRE).backtrackControl, IRBacktrackControlSemantics(false, false))

  // python-regex validates the versions anywhere, and the encodings at the end of the pattern out of groups.
  error("(?V0)(?V1)a", "", PythonRegex())("VERSION0 and VERSION1 flags are incompatible")
  error("(?V1)a", "V0", PythonRegex())("VERSION0 and VERSION1 flags are incompatible")
  error("(?a)(?u)", "", PythonRegex())("ASCII, LOCALE and UNICODE flags are incompatible")
  error("(?a)|(?L)", "", PythonRegex())("ASCII, LOCALE and UNICODE flags are incompatible")
  error("(?u)", "a", PythonRegex())("ASCII, LOCALE and UNICODE flags are incompatible")
  error("(?u)", "", PythonRegex(Python.Type.Bytes))("Cannot use UNICODE flag with a bytes pattern")
  check("(?a)(?-a)(?u)", "", PythonRegex())(Sequence(Empty, Empty, Empty))
  check("(?a:(?u))", "", PythonRegex())(Empty)
  check("(?u:)", "", PythonRegex(Python.Type.Bytes))(Empty)

  // python-regex's `p` (POSIX) and `r` (REVERSE) change the whole matching, and `w` (WORD) the boundaries and lines.
  check("a(?r)", "", PythonRegex())(
    Unsupported(NodeData.Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(nonDestructive = true), None))))
  )
  check("(?p:a)", "", PythonRegex())(
    Unsupported(
      NodeData.Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(preserve = true), None)), Node(NodeData.Literal('a')))
    )
  )
  check("a", "p", PythonRegex())(
    Unsupported(NodeData.Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(preserve = true), None))))
  )
  check("(?be)a", "", PythonRegex())(Sequence(Empty, char('a')))
  check("(?w)\\b", "", PythonRegex())(
    Sequence(Empty, Unsupported(NodeData.Backslash(BackslashKind.Assert(AssertKind.Boundary(None)))))
  )
  check("(?w:.^$)a", "", PythonRegex())(
    Sequence(Sequence(Unsupported(NodeData.Dot), Unsupported(NodeData.Caret), Unsupported(NodeData.Dollar)), char('a'))
  )

  // python-regex's `\m` and `\M`.
  checkWith("\\m\\M", "", PythonRegex())(_ =>
    val word = build("\\w", "", PythonRegex()).node.data.asInstanceOf[Chars].set
    Sequence(
      Sequence(Assert(IRAssertKind.WordBoundary(word)), LookAhead(false, Chars(word))),
      Sequence(Assert(IRAssertKind.WordBoundary(word)), LookBehind(false, Chars(word)))
    )
  )

  // python-regex's conditionals, `(*PRUNE)`, `(*SKIP)` and fuzzy constraints are unsupported, but `(*FAIL)` is not.
  check("(a)(?(1)b)", "", PythonRegex())(
    Sequence(
      Capture(1, None, char('a')),
      Unsupported(NodeData.Command(CommandKind.Conditional(ConditionalTest.Indexed(1), NodeData.Literal('b'))))
    )
  )
  check("(*PRUNE)", "", PythonRegex())(
    Unsupported(NodeData.Command(CommandKind.BacktrackControl(Some(BacktrackControlKind.Prune), None)))
  )
  check("(*FAIL)", "", PythonRegex())(Chars(CharSet.empty))
  check("a{e}", "", PythonRegex())(Unsupported(NodeData.Command(CommandKind.Fuzzy(NodeData.Literal('a'), "e"))))

  // ICU's `\b` skips the characters of `Grapheme_Extend` and `Cf`, and `(?w)` makes it UAX #29's (ICU 79.1).
  locally:
    val marks = IRClassLookup.icuWordMarks
    check("\\b", "", ICU)(Assert(IRAssertKind.MarkWordBoundary(marks.bases, marks)))
    check("\\B", "", ICU)(Assert(IRAssertKind.NotMarkWordBoundary(marks.bases, marks)))
    test("IRBuilder.build: ICU's word marks"):
      assert(marks.marks.contains(0x00ad) && marks.marks.contains(0x0301) && marks.marks.contains(0x200d))
      assert(marks.bases.contains('a') && marks.bases.contains('_') && !marks.bases.contains(0x200d))
  check("\\b", "w", ICU)(Unsupported(NodeData.Backslash(BackslashKind.Assert(AssertKind.Boundary(None)))))
  check("(?w:\\B)", "", ICU)(Unsupported(NodeData.Backslash(BackslashKind.Assert(AssertKind.NonBoundary(None)))))
  check("^", "w", ICU)(Assert(IRAssertKind.TextBegin))
  // ICU's look-behind tries the starts by the lengths in characters, where a literal under `i` may be its folding.
  check("(?<=ab)", "", ICU)(
    LookBehind(false, Sequence(char('a'), char('b')), Forward(Seq(IRLookBehindStartRange(2, 2))))
  )
  checkPattern("(?<=ß)", "i", ICU): pattern =>
    val LookBehind(_, _, m) = pattern.node.data: @unchecked
    assertEquals(m, Forward(Seq(IRLookBehindStartRange(0, 2))))
  checkPattern("(?<=.\\p{L})", "s", ICU): pattern =>
    val LookBehind(_, _, m) = pattern.node.data: @unchecked
    assertEquals(m, Forward(Seq(IRLookBehindStartRange(2, 3))))
  // ICU repeats a bounded repetition without checking an empty iteration.
  test("IRBuilder.build: ICU's empty iterations"):
    assertEquals(
      build("a", "", ICU).emptyIteration,
      IREmptyIterationSemantics(IREmptyIteration.ExitFromMin, IREmptyIteration.Ignore, false)
    )

  // Boost.Regex's verbs but `(*FAIL)` and its conditionals are unsupported (Boost.Regex 1.92.0).
  check("(*FAIL)(*F)", "", BoostRegex())(Sequence(Chars(CharSet.empty), Chars(CharSet.empty)))
  check("(*ACCEPT)", "", BoostRegex())(
    Unsupported(NodeData.Command(CommandKind.BacktrackControl(Some(BacktrackControlKind.Accept), None)))
  )
  check("(a)(?(1)b)", "", BoostRegex())(
    Sequence(
      Capture(1, None, char('a')),
      Unsupported(NodeData.Command(CommandKind.Conditional(ConditionalTest.Indexed(1), NodeData.Literal('b'))))
    )
  )
  // `\<` and `\>` are the start and the end of a word, `` \` `` and `\'` the begin and the end of the input, and `\C`
  // is `.`.
  check("\\<\\>\\`\\'", "", BoostRegex())(
    Sequence(
      Sequence(Assert(IRAssertKind.WordBoundary(ASCIIWord)), LookAhead(false, Chars(ASCIIWord))),
      Sequence(Assert(IRAssertKind.WordBoundary(ASCIIWord)), LookBehind(false, Chars(ASCIIWord))),
      Assert(IRAssertKind.TextBegin),
      Assert(IRAssertKind.TextEnd)
    )
  )
  check("[[:<:]]", "", BoostRegex())(
    Sequence(Assert(IRAssertKind.WordBoundary(ASCIIWord)), LookAhead(false, Chars(ASCIIWord)))
  )
  // The negations of `[[:<:]]` and `[[:>:]]` are ignored.
  check("[^[:<:]][[:^>:]]", "", BoostRegex())(
    Sequence(
      Sequence(Assert(IRAssertKind.WordBoundary(ASCIIWord)), LookAhead(false, Chars(ASCIIWord))),
      Sequence(Assert(IRAssertKind.WordBoundary(ASCIIWord)), LookBehind(false, Chars(ASCIIWord)))
    )
  )
  // A quantifier repeats `(?i)` as an empty group, whose flag reaches the rest.
  check("a(?i)+b", "", BoostRegex())(
    Sequence(char('a'), Repeat(Empty, Unbounded(1, Greedy)), Chars(CharSet.of('b', 'B')))
  )
  check("\\C", "S", BoostRegex())(Chars(UnitMode.Byte.universe.diff(CharSet.of('\n', '\f', '\r'))))
  // `\X` is a character but a combining one and the combining ones after it, of the old table of Boost.Regex.
  check("\\X", "", BoostRegex())(Chars(UnitMode.Byte.universe))
  locally:
    val combining = IRClassLookup.boostRegexCombining
    check("\\X", "", BoostRegex(BoostRegex.Type.U32Regex))(
      Atomic(
        Sequence(Chars(combining.complement(UnitMode.ScalarValue)), Repeat(Chars(combining), Unbounded(0, Greedy)))
      )
    )
    test("IRBuilder.build: Boost.Regex's combining characters"):
      assert(combining.contains(0x0300) && combining.contains(0xfe23) && !combining.contains(0x0362))
  // Boost.Regex ends a repetition after an empty iteration, as `check_null_repeat` sets its count to the maximum.
  test("IRBuilder.build: Boost.Regex's empty iterations"):
    assertEquals(
      build("a", "", BoostRegex()).emptyIteration,
      IREmptyIterationSemantics(IREmptyIteration.ExitAll, IREmptyIteration.ExitAll, false)
    )
  check("(?<=ab)", "", BoostRegex())(
    LookBehind(false, Sequence(char('a'), char('b')), Forward(Seq(IRLookBehindStartRange(2, 2))))
  )
