package codes.quine.labo.redos
package automaton

import scala.util.Success

import EpsNFA._
import regexp.Pattern
import regexp.Pattern._
import data.IChar
import data.ICharSet
import util.Timeout

class CompilerSuite extends munit.FunSuite {

  /** Timeout checking is disabled in testing. */
  implicit val timeout: Timeout.NoTimeout.type = Timeout.NoTimeout

  test("Compiler.compile") {
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, true, false, false, false, false)
    val flagSet2 = FlagSet(false, false, false, true, false, false)
    val flagSet3 = FlagSet(false, false, false, true, true, false)

    // submatch
    assertEquals(
      Compiler.compile(Pattern(LineEnd, flagSet0)),
      Success(
        EpsNFA(
          ICharSet.any(false, false),
          Set(0, 1, 2, 3),
          2,
          1,
          Map(
            0 -> Assert(AssertKind.LineEnd, 1),
            2 -> Eps(Seq(0, 3)),
            3 -> Consume(Set(IChar.Any16), 2)
          )
        )
      )
    )
    assertEquals(
      Compiler.compile(Pattern(LineBegin, flagSet0)),
      Success(
        EpsNFA(
          ICharSet.any(false, false),
          Set(0, 1, 2, 3),
          0,
          2,
          Map(
            0 -> Assert(AssertKind.LineBegin, 1),
            1 -> Eps(Seq(2, 3)),
            3 -> Consume(Set(IChar.Any16), 1)
          )
        )
      )
    )

    // LineBegin, LineEnd, Sequence
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, LineEnd)), flagSet0)),
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Sequence(Seq.empty), LineEnd)), flagSet0)),
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

    // WordBoundary
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, WordBoundary(false), LineEnd)), flagSet0)),
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, WordBoundary(true), LineEnd)), flagSet0)),
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

    // Character, CharacterClass
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Character('a'), LineEnd)), flagSet1)),
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
      Compiler.compile(
        Pattern(Sequence(Seq(LineBegin, CharacterClass(false, Seq(Character('a'))), LineEnd)), flagSet0)
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
      Compiler.compile(
        Pattern(Sequence(Seq(LineBegin, CharacterClass(true, Seq(Character('a'))), LineEnd)), flagSet0)
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

    // Dot
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, LineEnd)), flagSet0)),
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, LineEnd)), flagSet1)),
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, LineEnd)), flagSet2)),
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, LineEnd)), flagSet3)),
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

    // Disjunction
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Disjunction(Seq(Dot, Dot)), LineEnd)), flagSet2)),
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

    // Star
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Star(false, Dot), LineEnd)), flagSet2)),
      Success(
        EpsNFA(
          ICharSet.any(false, false),
          Set(0, 1, 2, 3, 4, 5, 6, 7),
          0,
          7,
          Map(
            0 -> Assert(AssertKind.LineBegin, 1),
            1 -> Eps(Seq(4)),
            4 -> Eps(Seq(2, 5)),
            2 -> Consume(Set(IChar.Any16), 3),
            3 -> Eps(Seq(4)),
            5 -> Eps(Seq(6)),
            6 -> Assert(AssertKind.LineEnd, 7)
          )
        )
      )
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Star(true, Dot), LineEnd)), flagSet2)),
      Success(
        EpsNFA(
          ICharSet.any(false, false),
          Set(0, 1, 2, 3, 4, 5, 6, 7),
          0,
          7,
          Map(
            0 -> Assert(AssertKind.LineBegin, 1),
            1 -> Eps(Seq(4)),
            4 -> Eps(Seq(5, 2)),
            2 -> Consume(Set(IChar.Any16), 3),
            3 -> Eps(Seq(4)),
            5 -> Eps(Seq(6)),
            6 -> Assert(AssertKind.LineEnd, 7)
          )
        )
      )
    )

    // Plus
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Plus(false, Dot), LineEnd)), flagSet2)),
      Success(
        EpsNFA(
          ICharSet.any(false, false),
          Set(0, 1, 2, 3, 4, 5, 6),
          0,
          6,
          Map(
            0 -> Assert(AssertKind.LineBegin, 1),
            1 -> Eps(Seq(2)),
            2 -> Consume(Set(IChar.Any16), 3),
            3 -> Eps(Seq(2, 4)),
            4 -> Eps(Seq(5)),
            5 -> Assert(AssertKind.LineEnd, 6)
          )
        )
      )
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Plus(true, Dot), LineEnd)), flagSet2)),
      Success(
        EpsNFA(
          ICharSet.any(false, false),
          Set(0, 1, 2, 3, 4, 5, 6),
          0,
          6,
          Map(
            0 -> Assert(AssertKind.LineBegin, 1),
            1 -> Eps(Seq(2)),
            2 -> Consume(Set(IChar.Any16), 3),
            3 -> Eps(Seq(4, 2)),
            4 -> Eps(Seq(5)),
            5 -> Assert(AssertKind.LineEnd, 6)
          )
        )
      )
    )

    // Question
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Question(false, Dot), LineEnd)), flagSet2)),
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Question(true, Dot), LineEnd)), flagSet2)),
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

    // Repeat
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(false, 1, None, Dot), LineEnd)), flagSet2)),
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, LineEnd)), flagSet2))
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(false, 2, None, Dot), LineEnd)), flagSet2)),
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, Dot, LineEnd)), flagSet2))
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(false, 1, Some(None), Dot), LineEnd)), flagSet2)),
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, Star(false, Dot), LineEnd)), flagSet2))
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(true, 1, Some(None), Dot), LineEnd)), flagSet2)),
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, Star(true, Dot), LineEnd)), flagSet2))
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(false, 1, Some(Some(1)), Dot), LineEnd)), flagSet2)),
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, LineEnd)), flagSet2))
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(true, 1, Some(Some(1)), Dot), LineEnd)), flagSet2)),
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, LineEnd)), flagSet2))
    )
    interceptMessage[InvalidRegExpException]("out of order repetition quantifier") {
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(false, 2, Some(Some(1)), Dot), LineEnd)), flagSet2)).get
    }
    interceptMessage[InvalidRegExpException]("out of order repetition quantifier") {
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(true, 2, Some(Some(1)), Dot), LineEnd)), flagSet2)).get
    }
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(false, 1, Some(Some(2)), Dot), LineEnd)), flagSet2)),
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, Question(false, Dot), LineEnd)), flagSet2))
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(true, 1, Some(Some(2)), Dot), LineEnd)), flagSet2)),
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, Question(true, Dot), LineEnd)), flagSet2))
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(false, 1, Some(Some(3)), Dot), LineEnd)), flagSet2)),
      Compiler.compile(
        Pattern(
          Sequence(Seq(LineBegin, Dot, Question(false, Sequence(Seq(Dot, Question(false, Dot)))), LineEnd)),
          flagSet2
        )
      )
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(true, 1, Some(Some(3)), Dot), LineEnd)), flagSet2)),
      Compiler.compile(
        Pattern(
          Sequence(Seq(LineBegin, Dot, Question(true, Sequence(Seq(Dot, Question(true, Dot)))), LineEnd)),
          flagSet2
        )
      )
    )

    // Capture, NamedCapture, Group (and Character also)
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Character('a'), LineEnd)), flagSet0)),
      Success(nfaA)
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Capture(1, Character('a')), LineEnd)), flagSet0)),
      Success(nfaA)
    )
    assertEquals(
      Compiler.compile(
        Pattern(Sequence(Seq(LineBegin, NamedCapture(1, "foo", Character('a')), LineEnd)), flagSet0)
      ),
      Success(nfaA)
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Group(Character('a')), LineEnd)), flagSet0)),
      Success(nfaA)
    )

    // LookAhead, LookBehind, BackReference, NamedBackreference
    interceptMessage[UnsupportedException]("look-ahead assertion") {
      Compiler.compile(Pattern(LookAhead(false, Dot), flagSet0)).get
    }
    interceptMessage[UnsupportedException]("look-behind assertion") {
      Compiler.compile(Pattern(LookBehind(false, Dot), flagSet0)).get
    }
    interceptMessage[UnsupportedException]("back-reference") {
      Compiler.compile(Pattern(BackReference(1), flagSet0)).get
    }
    interceptMessage[UnsupportedException]("named back-reference") {
      Compiler.compile(Pattern(NamedBackReference("foo"), flagSet0)).get
    }
  }
}
