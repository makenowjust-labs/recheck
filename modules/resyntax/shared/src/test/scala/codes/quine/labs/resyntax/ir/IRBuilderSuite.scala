package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BackReferenceStyle
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy.*
import codes.quine.labs.resyntax.ast.BoundaryModifier
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.NameStyle
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Reference
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRCharClasses.AsciiWord
import codes.quine.labs.resyntax.ir.IRNodeData.*
import codes.quine.labs.resyntax.ir.IRQuantifier.*

class IRBuilderSuite extends IRSuiteBase:
  // Disjunction
  check("|", "", All*)(Disjunction(Empty, Empty))

  // Sequence
  check("", "", All*)(Empty)
  check("(?:)(?:)", "", All*)(Sequence(Empty, Empty))
  check("ab", "", All*)(Sequence(char('a'), char('b')))

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
  check("(?<=a)", "", All*)(LookBehind(false, char('a')))
  check("(?<!a)", "", All*)(LookBehind(true, char('a')))
  check("(*pla:a)", "", PCRE, Perl())(LookAhead(false, char('a')))
  check("(*negative_lookbehind:a)", "", PCRE, Perl())(LookBehind(true, char('a')))
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
  check("(?<*a)", "", PCRE)(NonAtomicLookBehind(IRNode(char('a'))))
  check("(*non_atomic_positive_lookbehind:a)", "", PCRE)(NonAtomicLookBehind(IRNode(char('a'))))
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
  check("(?(?=x)a|b)", "", DotNet, PCRE, Perl())(
    Conditional(IRCondition.LookAhead(false, char('x')), char('a'), char('b'))
  )
  check("(?(?!x)a|b)", "", DotNet, PCRE, Perl())(
    Conditional(IRCondition.LookAhead(true, char('x')), char('a'), char('b'))
  )
  check("(?(?<=x)a|b)", "", DotNet, PCRE, Perl())(
    Conditional(IRCondition.LookBehind(false, char('x')), char('a'), char('b'))
  )
  check("(?(*nlb:x)a|b)", "", PCRE, Perl())(
    Conditional(IRCondition.LookBehind(true, char('x')), char('a'), char('b'))
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
  check("(*CRLF)(*UTF)(*LIMIT_MATCH=1)", "", PCRE)(Sequence(Empty, Empty, Empty))
  check("(*NOTEMPTY)", "", PCRE)(Empty)

  // Caret
  check("^", "", DotNet, Java, JavaScript, PCRE, Perl(), Python())(Assert(IRAssertKind.TextBegin))

  // Backslash assertions
  check("\\A", "", DotNet, Java, PCRE, Perl(), Python(), Ruby())(Assert(IRAssertKind.TextBegin))
  check("\\z", "", DotNet, Java, PCRE, Perl(), Ruby())(Assert(IRAssertKind.TextEnd))
  // `a\Z` does not match `a\n` in Python, where `\Z` is the end of the text.
  check("\\Z", "", Python())(Assert(IRAssertKind.TextEnd))
  check("\\b", "", JavaScript, Java, PCRE)(Assert(IRAssertKind.WordBoundary(AsciiWord)))
  check("\\B", "", JavaScript, Java, PCRE)(Assert(IRAssertKind.NotWordBoundary(AsciiWord)))
  // ECMAScript's WordCharacters with `u` and `i` includes U+017F and U+212A, which fold to `s` and `k`.
  check("\\b", "iu", JavaScript)(Assert(IRAssertKind.WordBoundary(AsciiWord.union(CharSet.of(0x17f, 0x212a)))))
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
  checkWith("(a)\\1", "", All*)(d => Sequence(Capture(1, None, char('a')), BackReference(Seq(1), flagsOf(d))))
  checkWith("(a)(?i)\\1", "", DotNet, Java, PCRE, Perl(), Ruby()): d =>
    Sequence(Capture(1, None, char('a')), Empty, BackReference(Seq(1), flagsOf(d, "i")))
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

  // Quoting command
  check("\\Qa\\E", "", Java, PCRE)(Sequence(Empty, char('a'), Empty))
  // Perl's lexer rewrites a regex literal, so its commands leave no nodes.
  check("\\Ua\\E", "", Perl())(char('A'))
  check("\\Qa\\E", "", Perl())(char('a'))

  // Java's CANON_EQ makes a character-level node unsupported.
  check("a(?c)b", "", Java)(Sequence(char('a'), Empty, Unsupported(NodeData.Literal('b'))))

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
  checkPattern("", "", DotNet, Java, PCRE, Perl(), Python()): pattern =>
    assertEquals(pattern.backReference, IRBackReferenceSemantics(false, IRBackReferenceChoice.FirstSet))
  // `^(?<n>a)(?<n>ab)\k<n>b` does not match `aabab` in Ruby, which takes `ab` and does not backtrack to `a`.
  checkPattern("", "", Ruby()): pattern =>
    assertEquals(pattern.backReference, IRBackReferenceSemantics(false, IRBackReferenceChoice.LastMatching))

  test("IRBuilder.build: keeps the source location"):
    val (pattern, _) = build("a(?:b)*+", "", Java)
    pattern.node.data match
      case Sequence(Seq(a, atomic)) =>
        assertEquals(a.loc, codes.quine.labs.resyntax.ast.SourceLocation(0, 1))
        assertEquals(atomic.loc, codes.quine.labs.resyntax.ast.SourceLocation(1, 8))
        atomic.data match
          case Atomic(repeat) => assertEquals(repeat.loc, atomic.loc)
          case data           => fail(s"Unexpected: $data")
      case data => fail(s"Unexpected: $data")

  test("IRBuilder.build: resolves character-level nodes into character sets"):
    val pattern = IRBuilder.build(Node(NodeData.Literal('a')), FlagSet(), JavaScript)
    assertEquals(pattern.node.data, Chars(CharSet.single('a')))
