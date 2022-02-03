package codes.quine.labs.recheck
package automaton

import scala.util.Success

import codes.quine.labs.recheck.automaton.EpsNFA._
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.regexp.Pattern
import codes.quine.labs.recheck.regexp.Pattern._
import codes.quine.labs.recheck.unicode.IChar
import codes.quine.labs.recheck.unicode.ICharSet
import codes.quine.labs.recheck.unicode.ICharSet.CharKind

class EpsNFABuilderSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  locally {
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, true, false, false, false, false)
    val flagSet2 = FlagSet(false, false, false, true, false, false)
    val flagSet3 = FlagSet(false, false, false, true, true, false)

    test("EpsNFABuilder.build: submatch") {
      assertEquals(
        EpsNFABuilder.build(Pattern(LineEnd(), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4),
            1,
            0,
            Map(
              1 -> Eps(Seq(4, 2)),
              2 -> LoopEnter(0, 3),
              3 -> Consume(Set((IChar.Any16, CharKind.Normal)), 1),
              4 -> LoopExit(0, 0)
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(Pattern(LineBegin(), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4),
            0,
            4,
            Map(
              0 -> Eps(Seq(3, 1)),
              1 -> LoopEnter(0, 2),
              2 -> Consume(Set((IChar.Any16, CharKind.Normal)), 0),
              3 -> LoopExit(0, 4)
            )
          )
        )
      )
    }

    test("EpsNFABuilder.build: LineBegin, LineEnd, Sequence") {
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1),
            0,
            1,
            Map(
              0 -> Eps(Seq(1))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Sequence(Seq.empty), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2),
            0,
            2,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Eps(Seq(2))
            )
          )
        )
      )
    }

    test("EpsNFABuilder.build: WordBoundary") {
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), WordBoundary(false), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar.Word, CharKind.Word),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Assert(AssertKind.WordBoundary, 2),
              2 -> Eps(Seq(3))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), WordBoundary(true), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar.Word, CharKind.Word),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Assert(AssertKind.WordBoundaryNot, 2),
              2 -> Eps(Seq(3))
            )
          )
        )
      )
    }

    test("EpsNFABuilder.build: Character, CharacterClass") {
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Character('a'), LineEnd())), flagSet1)),
        Success(
          EpsNFA(
            ICharSet.any(true, false).add(IChar.canonicalize(IChar('a'), false)),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Consume(Set((IChar.canonicalize(IChar('a'), false), CharKind.Normal)), 2),
              2 -> Eps(Seq(3))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), CharacterClass(false, Seq(Character('a'))), LineEnd())), flagSet0)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar('a')),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Consume(Set((IChar('a'), CharKind.Normal)), 2),
              2 -> Eps(Seq(3))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), CharacterClass(true, Seq(Character('a'))), LineEnd())), flagSet0)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar('a')),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Consume(Set((IChar.Any16.diff(IChar('a')), CharKind.Normal)), 2),
              2 -> Eps(Seq(3))
            )
          )
        )
      )
    }

    test("EpsNFABuilder.build: Dot") {
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet0)),
        Success(
          EpsNFA(
            ICharSet.any(false, false).add(IChar.Any16.diff(IChar.LineTerminator)),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Consume(Set((IChar.Any16.diff(IChar.LineTerminator), CharKind.Normal)), 2),
              2 -> Eps(Seq(3))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet1)),
        Success(
          EpsNFA(
            ICharSet.any(true, false).add(IChar.canonicalize(IChar.Any16.diff(IChar.LineTerminator), false)),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Consume(
                Set((IChar.canonicalize(IChar.Any16.diff(IChar.LineTerminator), false), CharKind.Normal)),
                2
              ),
              2 -> Eps(Seq(3))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet2)),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Consume(Set((IChar.Any16, CharKind.Normal)), 2),
              2 -> Eps(Seq(3))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet3)),
        Success(
          EpsNFA(
            ICharSet.any(false, true),
            Set(0, 1, 2, 3),
            0,
            3,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Consume(Set((IChar.Any, CharKind.Normal)), 2),
              2 -> Eps(Seq(3))
            )
          )
        )
      )
    }

    test("EpsNFABuilder.build: Disjunction") {
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Disjunction(Seq(Dot(), Dot())), LineEnd())), flagSet2)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6, 7),
            0,
            7,
            Map(
              0 -> Eps(Seq(5)),
              5 -> Eps(Seq(1, 3)),
              1 -> Consume(Set((IChar.Any16, CharKind.Normal)), 2),
              2 -> Eps(Seq(6)),
              3 -> Consume(Set((IChar.Any16, CharKind.Normal)), 4),
              4 -> Eps(Seq(6)),
              6 -> Eps(Seq(7))
            )
          )
        )
      )
    }

    test("EpsNFABuilder.build: Star") {
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Star(false), Dot()), LineEnd())), flagSet2)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6, 7),
            0,
            7,
            Map(
              0 -> Eps(Seq(3)),
              3 -> Eps(Seq(4, 5)),
              4 -> LoopEnter(0, 1),
              1 -> Consume(Set((IChar.Any16, CharKind.Normal)), 2),
              2 -> Eps(Seq(3)),
              5 -> LoopExit(0, 6),
              6 -> Eps(Seq(7))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Star(true), Dot()), LineEnd())), flagSet2)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6, 7),
            0,
            7,
            Map(
              0 -> Eps(Seq(3)),
              3 -> Eps(Seq(5, 4)),
              4 -> LoopEnter(0, 1),
              1 -> Consume(Set((IChar.Any16, CharKind.Normal)), 2),
              2 -> Eps(Seq(3)),
              5 -> LoopExit(0, 6),
              6 -> Eps(Seq(7))
            )
          )
        )
      )
    }

    test("EpsNFABuilder.build: Plus") {
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Plus(false), Dot()), LineEnd())), flagSet2)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6),
            0,
            6,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Consume(Set((IChar.Any16, CharKind.Normal)), 2),
              2 -> Eps(Seq(3, 4)),
              3 -> LoopEnter(0, 1),
              4 -> LoopExit(0, 5),
              5 -> Eps(Seq(6))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Plus(true), Dot()), LineEnd())), flagSet2)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4, 5, 6),
            0,
            6,
            Map(
              0 -> Eps(Seq(1)),
              1 -> Consume(Set((IChar.Any16, CharKind.Normal)), 2),
              2 -> Eps(Seq(4, 3)),
              3 -> LoopEnter(0, 1),
              4 -> LoopExit(0, 5),
              5 -> Eps(Seq(6))
            )
          )
        )
      )
    }

    test("EpsNFABuilder.build: Question") {
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Question(false), Dot()), LineEnd())), flagSet2)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4),
            0,
            4,
            Map(
              0 -> Eps(Seq(3)),
              3 -> Eps(Seq(1, 2)),
              1 -> Consume(Set((IChar.Any16, CharKind.Normal)), 2),
              2 -> Eps(Seq(4))
            )
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Question(true), Dot()), LineEnd())), flagSet2)
        ),
        Success(
          EpsNFA(
            ICharSet.any(false, false),
            Set(0, 1, 2, 3, 4),
            0,
            4,
            Map(
              0 -> Eps(Seq(3)),
              3 -> Eps(Seq(2, 1)),
              1 -> Consume(Set((IChar.Any16, CharKind.Normal)), 2),
              2 -> Eps(Seq(4))
            )
          )
        )
      )
    }

    test("EpsNFABuilder.build: Repeat") {
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Exact(1, false), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Exact(2, false), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Dot(), Dot(), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Unbounded(1, false), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Dot(), Repeat(Quantifier.Star(false), Dot()), LineEnd())), flagSet2)
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Unbounded(1, true), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Dot(), Repeat(Quantifier.Star(true), Dot()), LineEnd())), flagSet2)
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Bounded(1, 1, false), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Bounded(1, 1, true), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet2))
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Bounded(1, 2, false), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Dot(), Repeat(Quantifier.Question(false), Dot()), LineEnd())), flagSet2)
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Bounded(1, 2, true), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Dot(), Repeat(Quantifier.Question(true), Dot()), LineEnd())), flagSet2)
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Bounded(1, 3, false), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(
          Pattern(
            Sequence(
              Seq(
                LineBegin(),
                Dot(),
                Repeat(Quantifier.Question(false), Sequence(Seq(Dot(), Repeat(Quantifier.Question(false), Dot())))),
                LineEnd()
              )
            ),
            flagSet2
          )
        )
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), Repeat(Quantifier.Bounded(1, 3, true), Dot()), LineEnd())), flagSet2)
        ),
        EpsNFABuilder.build(
          Pattern(
            Sequence(
              Seq(
                LineBegin(),
                Dot(),
                Repeat(Quantifier.Question(true), Sequence(Seq(Dot(), Repeat(Quantifier.Question(true), Dot())))),
                LineEnd()
              )
            ),
            flagSet2
          )
        )
      )
    }

    test("EpsNFABuilder.build: Capture, NamedCapture, Group") {
      val nfaA = EpsNFA(
        ICharSet.any(false, false).add(IChar('a')),
        Set(0, 1, 2, 3),
        0,
        3,
        Map(
          0 -> Eps(Seq(1)),
          1 -> Consume(Set((IChar('a'), CharKind.Normal)), 2),
          2 -> Eps(Seq(3))
        )
      )
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Character('a'), LineEnd())), flagSet0)),
        Success(nfaA)
      )
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Capture(1, Character('a')), LineEnd())), flagSet0)),
        Success(nfaA)
      )
      assertEquals(
        EpsNFABuilder.build(
          Pattern(Sequence(Seq(LineBegin(), NamedCapture(1, "foo", Character('a')), LineEnd())), flagSet0)
        ),
        Success(nfaA)
      )
      assertEquals(
        EpsNFABuilder.build(Pattern(Sequence(Seq(LineBegin(), Group(Character('a')), LineEnd())), flagSet0)),
        Success(nfaA)
      )
    }

    test("EpsNFABuilder.build: LookAhead, LookBehind, BackReference, NamedBackreference") {
      interceptMessage[UnsupportedException]("look-ahead assertion") {
        EpsNFABuilder.build(Pattern(LookAhead(false, Dot()), flagSet0)).get
      }
      interceptMessage[UnsupportedException]("look-behind assertion") {
        EpsNFABuilder.build(Pattern(LookBehind(false, Dot()), flagSet0)).get
      }
      interceptMessage[UnsupportedException]("back-reference") {
        EpsNFABuilder.build(Pattern(BackReference(1), flagSet0)).get
      }
      interceptMessage[UnsupportedException]("named back-reference") {
        EpsNFABuilder.build(Pattern(NamedBackReference(1, "foo"), flagSet0)).get
      }
    }
  }
}
