package codes.quine.labo.redos
package regexp

import scala.util.Success

import Pattern._
import data.IChar
import data.ICharSet
import data.UChar

class CompilerSuite extends munit.FunSuite {
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
