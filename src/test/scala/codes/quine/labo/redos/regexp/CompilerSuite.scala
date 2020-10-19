package codes.quine.labo.redos
package regexp

import scala.util.Success

import Pattern._
import automaton.EpsNFA
import automaton.EpsNFA._
import data.IChar
import data.ICharSet
import data.UChar

class CompilerSuite extends munit.FunSuite {
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Character(UChar('a')), LineEnd)), flagSet1)),
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
        Pattern(Sequence(Seq(LineBegin, CharacterClass(false, Seq(Character(UChar('a')))), LineEnd)), flagSet0)
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
        Pattern(Sequence(Seq(LineBegin, CharacterClass(true, Seq(Character(UChar('a')))), LineEnd)), flagSet0)
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, Question(false, Dot), LineEnd)), flagSet2))
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Repeat(true, 1, Some(Some(1)), Dot), LineEnd)), flagSet2)),
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Dot, Question(true, Dot), LineEnd)), flagSet2))
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
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Character(UChar('a')), LineEnd)), flagSet0)),
      Success(nfaA)
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Capture(Character(UChar('a'))), LineEnd)), flagSet0)),
      Success(nfaA)
    )
    assertEquals(
      Compiler.compile(
        Pattern(Sequence(Seq(LineBegin, NamedCapture("foo", Character(UChar('a'))), LineEnd)), flagSet0)
      ),
      Success(nfaA)
    )
    assertEquals(
      Compiler.compile(Pattern(Sequence(Seq(LineBegin, Group(Character(UChar('a'))), LineEnd)), flagSet0)),
      Success(nfaA)
    )

    // LookAhead, LookBehind, BackReference, NamedBackreference
    assertEquals(
      intercept[UnsupportedException](Compiler.compile(Pattern(LookAhead(false, Dot), flagSet0)).get).getMessage,
      "look-ahead assertion"
    )
    assertEquals(
      intercept[UnsupportedException](Compiler.compile(Pattern(LookBehind(false, Dot), flagSet0)).get).getMessage,
      "look-behind assertion"
    )
    assertEquals(
      intercept[UnsupportedException](Compiler.compile(Pattern(BackReference(1), flagSet0)).get).getMessage,
      "back-reference"
    )
    assertEquals(
      intercept[UnsupportedException](Compiler.compile(Pattern(NamedBackReference("foo"), flagSet0)).get).getMessage,
      "named back-reference"
    )
  }

  test("Compiler.hasLineBeginAtBegin") {
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, false, true, false, false, false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Disjunction(Seq(LineBegin, LineBegin)), flagSet0)), true)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Disjunction(Seq(Dot, LineBegin)), flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Disjunction(Seq(LineBegin, Dot)), flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Disjunction(Seq(Dot, Dot)), flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Sequence(Seq(Dot, LineBegin)), flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Sequence(Seq(LineBegin, Dot)), flagSet0)), true)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Sequence(Seq(Dot, Dot)), flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Capture(LineBegin), flagSet0)), true)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Capture(Dot), flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(NamedCapture("foo", LineBegin), flagSet0)), true)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(NamedCapture("foo", Dot), flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Group(LineBegin), flagSet0)), true)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Group(Dot), flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(LineBegin, flagSet0)), true)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(LineEnd, flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(Dot, flagSet0)), false)
    assertEquals(Compiler.hasLineBeginAtBegin(Pattern(LineBegin, flagSet1)), false)
  }

  test("Compiler.hasLineEndAtEnd") {
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, false, true, false, false, false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Disjunction(Seq(LineEnd, LineEnd)), flagSet0)), true)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Disjunction(Seq(Dot, LineEnd)), flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Disjunction(Seq(LineEnd, Dot)), flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Disjunction(Seq(Dot, Dot)), flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Sequence(Seq(Dot, LineEnd)), flagSet0)), true)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Sequence(Seq(LineEnd, Dot)), flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Sequence(Seq(Dot, Dot)), flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Capture(LineEnd), flagSet0)), true)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Capture(Dot), flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(NamedCapture("foo", LineEnd), flagSet0)), true)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(NamedCapture("foo", Dot), flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Group(LineEnd), flagSet0)), true)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Group(Dot), flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(LineBegin, flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(LineEnd, flagSet0)), true)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(Dot, flagSet0)), false)
    assertEquals(Compiler.hasLineEndAtEnd(Pattern(LineEnd, flagSet1)), false)
  }

  test("Compiler.alphabet") {
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, false, true, false, false, false)
    val flagSet2 = FlagSet(false, false, false, true, false, false)
    val flagSet3 = FlagSet(false, true, false, false, false, false)
    val flagSet4 = FlagSet(false, true, false, false, true, false)
    val word = IChar.Word.withWord
    val lineTerminator = IChar.LineTerminator.withLineTerminator
    val dot16 = IChar.Any16.diff(IChar.LineTerminator)
    val dot = IChar.Any.diff(IChar.LineTerminator)
    assertEquals(Compiler.alphabet(Pattern(Sequence(Seq.empty), flagSet0)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(Dot, flagSet0)), Success(ICharSet.any(false, false).add(dot16)))
    assertEquals(
      Compiler.alphabet(Pattern(Disjunction(Seq(Character(UChar('A')), Character(UChar('Z')))), flagSet0)),
      Success(ICharSet.any(false, false).add(IChar('A')).add(IChar('Z')))
    )
    assertEquals(
      Compiler.alphabet(Pattern(WordBoundary(false), flagSet0)),
      Success(ICharSet.any(false, false).add(word))
    )
    assertEquals(
      Compiler.alphabet(Pattern(LineBegin, flagSet1)),
      Success(ICharSet.any(false, false).add(lineTerminator))
    )
    assertEquals(
      Compiler.alphabet(Pattern(Sequence(Seq(LineBegin, WordBoundary(false))), flagSet1)),
      Success(ICharSet.any(false, false).add(lineTerminator).add(word))
    )
    assertEquals(Compiler.alphabet(Pattern(Capture(Dot), flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(NamedCapture("foo", Dot), flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(Group(Dot), flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(Star(false, Dot), flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(Plus(false, Dot), flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(Question(false, Dot), flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(Repeat(false, 2, None, Dot), flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(LookAhead(false, Dot), flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(LookBehind(false, Dot), flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(Dot, flagSet2)), Success(ICharSet.any(false, false)))
    assertEquals(Compiler.alphabet(Pattern(Sequence(Seq.empty), flagSet3)), Success(ICharSet.any(true, false)))
    assertEquals(
      Compiler.alphabet(Pattern(Dot, flagSet3)),
      Success(ICharSet.any(true, false).add(IChar.canonicalize(dot16, false)))
    )
    assertEquals(
      Compiler.alphabet(Pattern(Disjunction(Seq(Character(UChar('A')), Character(UChar('Z')))), flagSet3)),
      Success(ICharSet.any(true, false).add(IChar('A')).add(IChar('Z')))
    )
    assertEquals(Compiler.alphabet(Pattern(Sequence(Seq.empty), flagSet4)), Success(ICharSet.any(true, true)))
    assertEquals(
      Compiler.alphabet(Pattern(Dot, flagSet4)),
      Success(ICharSet.any(true, true).add(IChar.canonicalize(dot, true)))
    )
  }

  test("Compiler.needsLineTerminatorDistinction") {
    val flagSet0 = FlagSet(false, false, true, false, false, false)
    val flagSet1 = FlagSet(false, false, false, false, false, false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Disjunction(Seq(Dot, LineBegin)), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Disjunction(Seq(LineBegin, Dot)), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Disjunction(Seq(Dot, Dot)), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Sequence(Seq(Dot, LineBegin)), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Sequence(Seq(LineBegin, Dot)), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Sequence(Seq(Dot, Dot)), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Capture(LineBegin), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Capture(Dot), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(NamedCapture("foo", LineBegin), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(NamedCapture("foo", Dot), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Group(LineBegin), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Group(Dot), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Star(false, LineBegin), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Star(false, Dot), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Plus(false, LineBegin), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Plus(false, Dot), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Question(false, LineBegin), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Question(false, Dot), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Repeat(false, 2, None, LineBegin), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Repeat(false, 2, None, Dot), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(LookAhead(false, LineBegin), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(LookAhead(false, Dot), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(LookBehind(false, LineBegin), flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(LookBehind(false, Dot), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(LineBegin, flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(LineEnd, flagSet0)), true)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(WordBoundary(true), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(WordBoundary(false), flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(Dot, flagSet0)), false)
    assertEquals(Compiler.needsLineTerminatorDistinction(Pattern(LineBegin, flagSet1)), false)
  }

  test("Compiler.needsWordDistinction") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Compiler.needsWordDistinction(Pattern(Disjunction(Seq(Dot, WordBoundary(false))), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Disjunction(Seq(WordBoundary(false), Dot)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Disjunction(Seq(Dot, Dot)), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(Sequence(Seq(Dot, WordBoundary(false))), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Sequence(Seq(WordBoundary(false), Dot)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Sequence(Seq(Dot, Dot)), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(Capture(WordBoundary(false)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Capture(Dot), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(NamedCapture("foo", WordBoundary(false)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(NamedCapture("foo", Dot), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(Group(WordBoundary(false)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Group(Dot), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(Star(false, WordBoundary(false)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Star(false, Dot), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(Plus(false, WordBoundary(false)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Plus(false, Dot), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(Question(false, WordBoundary(false)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Question(false, Dot), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(Repeat(false, 2, None, WordBoundary(false)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(Repeat(false, 2, None, Dot), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(LookAhead(false, WordBoundary(false)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(LookAhead(false, Dot), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(LookBehind(false, WordBoundary(false)), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(LookBehind(false, Dot), flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(WordBoundary(true), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(WordBoundary(false), flagSet)), true)
    assertEquals(Compiler.needsWordDistinction(Pattern(LineBegin, flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(LineEnd, flagSet)), false)
    assertEquals(Compiler.needsWordDistinction(Pattern(Dot, flagSet)), false)
  }
}
