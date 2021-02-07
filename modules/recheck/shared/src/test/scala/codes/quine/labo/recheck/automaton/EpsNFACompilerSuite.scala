package codes.quine.labo.recheck
package automaton

import scala.util.Success

import codes.quine.labo.recheck.automaton.EpsNFA._
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.common.UnsupportedException
import codes.quine.labo.recheck.data.IChar
import codes.quine.labo.recheck.data.ICharSet
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.Pattern._

class EpsNFACompilerSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  locally {
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, true, false, false, false, false)
    val flagSet2 = FlagSet(false, false, false, true, false, false)
    val flagSet3 = FlagSet(false, false, false, true, true, false)

    test("EpsNFACompiler.compile: submaatch") {
      assertEquals(
        EpsNFACompiler.compile(Pattern(LineEnd(), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5),
            2,
            1,
            Map(
              0 -> Assert(AssertKind.LineEnd, 1),
              2 -> Eps(Seq(5, 3)),
              3 -> LoopEnter(0, 4),
              4 -> Consume(Set(IChar.Any16), 2),
              5 -> LoopExit(0, 0)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(LineBegin(), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(4, 2)),
              2 -> LoopEnter(0, 3),
              3 -> Consume(Set(IChar.Any16), 1),
              4 -> LoopExit(0, 5)
            )
          )
        )
      )
    }

    test("EpsNFACompiler.compiler: LineBegin, LineEnd, Sequence") {
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Assert(AssertKind.LineEnd, 3)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Sequence(Seq.empty), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4),
            0,
            4,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Eps(Seq(3)),
              3 -> Assert(AssertKind.LineEnd, 4)
            )
          )
        )
      )
    }

    test("EpsNFACompiler.compiler: WordBoundary") {
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), WordBoundary(false), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar.Word.withWord),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Assert(AssertKind.WordBoundary, 3),
              3 -> Eps(Seq(4)),
              4 -> Assert(AssertKind.LineEnd, 5)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), WordBoundary(true), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar.Word.withWord),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Assert(AssertKind.NotWordBoundary, 3),
              3 -> Eps(Seq(4)),
              4 -> Assert(AssertKind.LineEnd, 5)
            )
          )
        )
      )
    }

    test("EpsNFACompiler.compile: Character, CharacterClass") {
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Character('a'), LineEnd())), flagSet1)),
        Success(
          EpsNFA(
            ICharSet.any(true, false).add(IChar.canonicalize(IChar('a'), false)),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Consume(Set(IChar.canonicalize(IChar('a'), false)), 3),
              3 -> Eps(Seq(4)),
              4 -> Assert(AssertKind.LineEnd, 5)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), CharacterClass(false, Seq(Character('a'))), LineEnd())), flagSet0)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar('a')),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Consume(Set(IChar('a')), 3),
              3 -> Eps(Seq(4)),
              4 -> Assert(AssertKind.LineEnd, 5)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), CharacterClass(true, Seq(Character('a'))), LineEnd())), flagSet0)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar('a')),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Consume(Set(IChar.Any16.diff(IChar('a'))), 3),
              3 -> Eps(Seq(4)),
              4 -> Assert(AssertKind.LineEnd, 5)
            )
          )
        )
      )
    }

    test("EpsNFACompiler.compile: Dot") {
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar.Any16.diff(IChar.LineTerminator)),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Consume(Set(IChar.Any16.diff(IChar.LineTerminator)), 3),
              3 -> Eps(Seq(4)),
              4 -> Assert(AssertKind.LineEnd, 5)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet1)),
        Success(
          EpsNFA(
            ICharSet.any(true, false).add(IChar.canonicalize(IChar.Any16.diff(IChar.LineTerminator), false)),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Consume(Set(IChar.canonicalize(IChar.Any16.diff(IChar.LineTerminator), false)), 3),
              3 -> Eps(Seq(4)),
              4 -> Assert(AssertKind.LineEnd, 5)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet2)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Consume(Set(IChar.Any16), 3),
              3 -> Eps(Seq(4)),
              4 -> Assert(AssertKind.LineEnd, 5)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet3)),
        Success(
          EpsNFA(
            ICharSet.any(false, true),
            Set(0, 1, 2, 3, 4, 5),
            0,
            5,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Consume(Set(IChar.Any), 3),
              3 -> Eps(Seq(4)),
              4 -> Assert(AssertKind.LineEnd, 5)
            )
          )
        )
      )
    }

    test("EpsNFACompiler.compile: Disjunction") {
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), Disjunction(Seq(Dot(), Dot())), LineEnd())), flagSet2)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
            0,
            9,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(6)),
              6 -> Eps(Seq(2, 4)),
              2 -> Consume(Set(IChar.Any16), 3),
              3 -> Eps(Seq(7)),
              4 -> Consume(Set(IChar.Any16), 5),
              5 -> Eps(Seq(7)),
              7 -> Eps(Seq(8)),
              8 -> Assert(AssertKind.LineEnd, 9)
            )
          )
        )
      )
    }

    test("EpsNFACompiler.compile: Star") {
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Star(false, Dot()), LineEnd())), flagSet2)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
            0,
            9,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(4)),
              4 -> Eps(Seq(5, 6)),
              5 -> LoopEnter(0, 2),
              2 -> Consume(Set(IChar.Any16), 3),
              3 -> Eps(Seq(4)),
              6 -> LoopExit(0, 7),
              7 -> Eps(Seq(8)),
              8 -> Assert(AssertKind.LineEnd, 9)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Star(true, Dot()), LineEnd())), flagSet2)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
            0,
            9,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(4)),
              4 -> Eps(Seq(6, 5)),
              5 -> LoopEnter(0, 2),
              2 -> Consume(Set(IChar.Any16), 3),
              3 -> Eps(Seq(4)),
              6 -> LoopExit(0, 7),
              7 -> Eps(Seq(8)),
              8 -> Assert(AssertKind.LineEnd, 9)
            )
          )
        )
      )
    }

    test("EpsNFACompiler.compile: Plus") {
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Plus(false, Dot()), LineEnd())), flagSet2)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6, 7, 8),
            0,
            8,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Consume(Set(IChar.Any16), 3),
              3 -> Eps(Seq(4, 5)),
              4 -> LoopEnter(0, 2),
              5 -> LoopExit(0, 6),
              6 -> Eps(Seq(7)),
              7 -> Assert(AssertKind.LineEnd, 8)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Plus(true, Dot()), LineEnd())), flagSet2)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6, 7, 8),
            0,
            8,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(2)),
              2 -> Consume(Set(IChar.Any16), 3),
              3 -> Eps(Seq(5, 4)),
              4 -> LoopEnter(0, 2),
              5 -> LoopExit(0, 6),
              6 -> Eps(Seq(7)),
              7 -> Assert(AssertKind.LineEnd, 8)
            )
          )
        )
      )
    }

    test("EpsNFACompiler.compile: Question") {
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Question(false, Dot()), LineEnd())), flagSet2)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6),
            0,
            6,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(4)),
              4 -> Eps(Seq(2, 3)),
              2 -> Consume(Set(IChar.Any16), 3),
              3 -> Eps(Seq(5)),
              5 -> Assert(AssertKind.LineEnd, 6)
            )
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Question(true, Dot()), LineEnd())), flagSet2)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6),
            0,
            6,
            Map(
              0 -> Assert(AssertKind.LineBegin, 1),
              1 -> Eps(Seq(4)),
              4 -> Eps(Seq(3, 2)),
              2 -> Consume(Set(IChar.Any16), 3),
              3 -> Eps(Seq(5)),
              5 -> Assert(AssertKind.LineEnd, 6)
            )
          )
        )
      )
    }

    test("EpsNFACompiler.commpile: Repeat") {
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, None, Dot()), LineEnd())), flagSet2)),
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Repeat(false, 2, None, Dot()), LineEnd())), flagSet2)),
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), Dot(), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, Some(None), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), Star(false, Dot()), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), Repeat(true, 1, Some(None), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), Star(true, Dot()), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, Some(Some(1)), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), Repeat(true, 1, Some(Some(1)), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet2))
      )
      interceptMessage[InvalidRegExpException]("out of order repetition quantifier") {
        EpsNFACompiler
          .compile(Pattern(Sequence(Seq(LineBegin(), Repeat(false, 2, Some(Some(1)), Dot()), LineEnd())), flagSet2))
          .get
      }
      interceptMessage[InvalidRegExpException]("out of order repetition quantifier") {
        EpsNFACompiler
          .compile(Pattern(Sequence(Seq(LineBegin(), Repeat(true, 2, Some(Some(1)), Dot()), LineEnd())), flagSet2))
          .get
      }
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, Some(Some(2)), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), Question(false, Dot()), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), Repeat(true, 1, Some(Some(2)), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Dot(), Question(true, Dot()), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, Some(Some(3)), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFACompiler.compile(
          Pattern(
            Sequence(Seq(LineBegin(), Dot(), Question(false, Sequence(Seq(Dot(), Question(false, Dot())))), LineEnd())),
            flagSet2
          )
        )
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), Repeat(true, 1, Some(Some(3)), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFACompiler.compile(
          Pattern(
            Sequence(Seq(LineBegin(), Dot(), Question(true, Sequence(Seq(Dot(), Question(true, Dot())))), LineEnd())),
            flagSet2
          )
        )
      )
    }

    test("EpsNFACompiler.compile: Capture, NamedCapture, Group") {
      val nfaA = EpsNFA(
        ICharSet.any(false, false).add(IChar('a')),
        Set(0, 1, 2, 3, 4, 5),
        0,
        5,
        Map(
          0 -> Assert(AssertKind.LineBegin, 1),
          1 -> Eps(Seq(2)),
          2 -> Consume(Set(IChar('a')), 3),
          3 -> Eps(Seq(4)),
          4 -> Assert(AssertKind.LineEnd, 5)
        )
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Character('a'), LineEnd())), flagSet0)),
        Success(nfaA)
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Capture(1, Character('a')), LineEnd())), flagSet0)),
        Success(nfaA)
      )
      assertEquals(
        EpsNFACompiler.compile(
          Pattern(Sequence(Seq(LineBegin(), NamedCapture(1, "foo", Character('a')), LineEnd())), flagSet0)
        ),
        Success(nfaA)
      )
      assertEquals(
        EpsNFACompiler.compile(Pattern(Sequence(Seq(LineBegin(), Group(Character('a')), LineEnd())), flagSet0)),
        Success(nfaA)
      )
    }

    test("EpsNFACompiler.compile: LookAhead, LookBehind, BackReference, NamedBackreference") {
      interceptMessage[UnsupportedException]("look-ahead assertion") {
        EpsNFACompiler.compile(Pattern(LookAhead(false, Dot()), flagSet0)).get
      }
      interceptMessage[UnsupportedException]("look-behind assertion") {
        EpsNFACompiler.compile(Pattern(LookBehind(false, Dot()), flagSet0)).get
      }
      interceptMessage[UnsupportedException]("back-reference") {
        EpsNFACompiler.compile(Pattern(BackReference(1), flagSet0)).get
      }
      interceptMessage[UnsupportedException]("named back-reference") {
        EpsNFACompiler.compile(Pattern(NamedBackReference("foo"), flagSet0)).get
      }
    }
  }
}
