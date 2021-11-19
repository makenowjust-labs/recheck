package codes.quine.labo.recheck.regexp

import scala.util.Success

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.regexp.Pattern._
import codes.quine.labo.recheck.regexp.PatternExtensions._
import codes.quine.labo.recheck.unicode.IChar
import codes.quine.labo.recheck.unicode.ICharSet
import codes.quine.labo.recheck.unicode.ICharSet.CharKind
import codes.quine.labo.recheck.unicode.UString

class PatternExtensionsSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("PatternExtensions.AtomNodeOps#toIChar") {
    assertEquals(Character('x').toIChar(false), Success(IChar('x')))
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Word).toIChar(false), Success(IChar.Word))
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Word).toIChar(false),
      Success(IChar.Word.complement(false))
    )
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Word).toIChar(true),
      Success(IChar.Word.complement(true))
    )
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Digit).toIChar(false), Success(IChar.Digit))
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Digit).toIChar(false),
      Success(IChar.Digit.complement(false))
    )
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Space).toIChar(false), Success(IChar.Space))
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Space).toIChar(false),
      Success(IChar.Space.complement(false))
    )
    assertEquals(UnicodeProperty(false, "ASCII").toIChar(false), Success(IChar.UnicodeProperty("ASCII").get))
    assertEquals(
      UnicodeProperty(true, "ASCII").toIChar(true),
      Success(IChar.UnicodeProperty("ASCII").get.complement(true))
    )
    assertEquals(UnicodeProperty(false, "L").toIChar(false), Success(IChar.UnicodeProperty("L").get))
    assertEquals(
      UnicodeProperty(true, "L").toIChar(true),
      Success(IChar.UnicodeProperty("L").get.complement(true))
    )
    interceptMessage[InvalidRegExpException]("unknown Unicode property: invalid") {
      UnicodeProperty(false, "invalid").toIChar(false).get
    }
    val Hira = IChar.UnicodePropertyValue("sc", "Hira").get
    assertEquals(UnicodePropertyValue(false, "sc", "Hira").toIChar(false), Success(Hira))
    assertEquals(UnicodePropertyValue(true, "sc", "Hira").toIChar(true), Success(Hira.complement(true)))
    interceptMessage[InvalidRegExpException]("unknown Unicode property-value: sc=invalid") {
      UnicodePropertyValue(false, "sc", "invalid").toIChar(false).get
    }
    assertEquals(
      CharacterClass(false, Seq(Character('a'), Character('A'))).toIChar(false),
      Success(IChar('a').union(IChar('A')))
    )
    assertEquals(
      CharacterClass(true, Seq(Character('a'), Character('A'))).toIChar(false),
      Success(IChar('a').union(IChar('A'))) // Not complemented is intentionally.
    )
    interceptMessage[InvalidRegExpException]("an empty range") {
      CharacterClass(true, Seq(ClassRange('z', 'a'))).toIChar(false).get
    }
    assertEquals(ClassRange('a', 'a').toIChar(false), Success(IChar('a')))
    assertEquals(ClassRange('a', 'z').toIChar(false), Success(IChar.range('a', 'z')))
    interceptMessage[InvalidRegExpException]("an empty range") {
      ClassRange('z', 'a').toIChar(false).get
    }
  }

  test("PatternExtensions.NodeOps#captureRange") {
    assertEquals(Sequence(Seq(Capture(1, Dot()), Capture(2, Dot()))).captureRange, CaptureRange(Some((1, 2))))
    assertEquals(Disjunction(Seq(Capture(1, Dot()), Capture(2, Dot()))).captureRange, CaptureRange(Some((1, 2))))
    assertEquals(Capture(1, Dot()).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(Capture(1, Capture(2, Dot())).captureRange, CaptureRange(Some((1, 2))))
    assertEquals(NamedCapture(1, "x", Dot()).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(NamedCapture(1, "x", Capture(2, Dot())).captureRange, CaptureRange(Some((1, 2))))
    assertEquals(Group(Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(Star(false, Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(Plus(false, Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(Question(false, Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(Repeat(false, 1, None, Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(WordBoundary(false).captureRange, CaptureRange(None))
    assertEquals(LineBegin().captureRange, CaptureRange(None))
    assertEquals(LineEnd().captureRange, CaptureRange(None))
    assertEquals(LookAhead(false, Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(LookBehind(false, Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(Character('a').captureRange, CaptureRange(None))
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Digit).captureRange, CaptureRange(None))
    assertEquals(UnicodeProperty(false, "L").captureRange, CaptureRange(None))
    assertEquals(UnicodePropertyValue(false, "sc", "Hira").captureRange, CaptureRange(None))
    assertEquals(CharacterClass(false, Seq(Character('a'))).captureRange, CaptureRange(None))
    assertEquals(Dot().captureRange, CaptureRange(None))
    assertEquals(BackReference(1).captureRange, CaptureRange(None))
    assertEquals(NamedBackReference(1, "foo").captureRange, CaptureRange(None))
  }

  test("PatternExtensions.NodeOps#isEmpty") {
    assertEquals(Sequence(Seq(Dot(), Dot())).isEmpty, false)
    assertEquals(Sequence(Seq.empty).isEmpty, true)
    assertEquals(Disjunction(Seq(Dot(), Dot())).isEmpty, false)
    assertEquals(Disjunction(Seq(Sequence(Seq.empty), Dot())).isEmpty, true)
    assertEquals(Disjunction(Seq(Dot(), Sequence(Seq.empty))).isEmpty, true)
    assertEquals(Capture(1, Dot()).isEmpty, false)
    assertEquals(Capture(1, Sequence(Seq.empty)).isEmpty, true)
    assertEquals(NamedCapture(1, "x", Dot()).isEmpty, false)
    assertEquals(NamedCapture(1, "x", Sequence(Seq.empty)).isEmpty, true)
    assertEquals(Group(Dot()).isEmpty, false)
    assertEquals(Group(Sequence(Seq.empty)).isEmpty, true)
    assertEquals(Star(false, Dot()).isEmpty, true)
    assertEquals(Star(false, Sequence(Seq.empty)).isEmpty, true)
    assertEquals(Plus(false, Dot()).isEmpty, false)
    assertEquals(Plus(false, Sequence(Seq.empty)).isEmpty, true)
    assertEquals(Question(false, Dot()).isEmpty, true)
    assertEquals(Question(false, Sequence(Seq.empty)).isEmpty, true)
    assertEquals(Repeat(false, 0, Some(Some(1)), Dot()).isEmpty, true)
    assertEquals(Repeat(false, 0, Some(Some(1)), Sequence(Seq.empty)).isEmpty, true)
    assertEquals(Repeat(false, 1, Some(Some(2)), Dot()).isEmpty, false)
    assertEquals(Repeat(false, 1, Some(Some(2)), Sequence(Seq.empty)).isEmpty, true)
    assertEquals(WordBoundary(false).isEmpty, true)
    assertEquals(LineBegin().isEmpty, true)
    assertEquals(LineEnd().isEmpty, true)
    assertEquals(LookAhead(false, Dot()).isEmpty, true)
    assertEquals(LookBehind(false, Dot()).isEmpty, true)
    assertEquals(Character('a').isEmpty, false)
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Digit).isEmpty, false)
    assertEquals(UnicodeProperty(false, "L").isEmpty, false)
    assertEquals(UnicodePropertyValue(false, "sc", "Hira").isEmpty, false)
    assertEquals(CharacterClass(false, Seq(Character('a'))).isEmpty, false)
    assertEquals(Dot().isEmpty, false)
    assertEquals(BackReference(1).isEmpty, true)
    assertEquals(NamedBackReference(1, "foo").isEmpty, true)
  }

  test("PatternExtensions.CaptureRange#merge") {
    assertEquals(CaptureRange(None).merge(CaptureRange(None)), CaptureRange(None))
    assertEquals(CaptureRange(Some((1, 1))).merge(CaptureRange(None)), CaptureRange(Some((1, 1))))
    assertEquals(CaptureRange(None).merge(CaptureRange(Some((2, 2)))), CaptureRange(Some((2, 2))))
    assertEquals(CaptureRange(Some((1, 1))).merge(CaptureRange(Some((2, 2)))), CaptureRange(Some((1, 2))))
  }

  test("PatternExtensions.PatternOps#hasLineBeginAtBegin") {
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, false, true, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), LineBegin())), flagSet0).hasLineBeginAtBegin, true)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineBegin())), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), Dot())), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineBegin())), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet0).hasLineBeginAtBegin, true)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(Capture(1, LineBegin()), flagSet0).hasLineBeginAtBegin, true)
    assertEquals(Pattern(Capture(1, Dot()), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(NamedCapture(1, "foo", LineBegin()), flagSet0).hasLineBeginAtBegin, true)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(Group(LineBegin()), flagSet0).hasLineBeginAtBegin, true)
    assertEquals(Pattern(Group(Dot()), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(LineBegin(), flagSet0).hasLineBeginAtBegin, true)
    assertEquals(Pattern(LineEnd(), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(Dot(), flagSet0).hasLineBeginAtBegin, false)
    assertEquals(Pattern(LineBegin(), flagSet1).hasLineBeginAtBegin, false)
  }

  test("PatternExtensions.PatternOps#hasLineEndAtEnd") {
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, false, true, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(LineEnd(), LineEnd())), flagSet0).hasLineEndAtEnd, true)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineEnd())), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(Disjunction(Seq(LineEnd(), Dot())), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineEnd())), flagSet0).hasLineEndAtEnd, true)
    assertEquals(Pattern(Sequence(Seq(LineEnd(), Dot())), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(Capture(1, LineEnd()), flagSet0).hasLineEndAtEnd, true)
    assertEquals(Pattern(Capture(1, Dot()), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(NamedCapture(1, "foo", LineEnd()), flagSet0).hasLineEndAtEnd, true)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(Group(LineEnd()), flagSet0).hasLineEndAtEnd, true)
    assertEquals(Pattern(Group(Dot()), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(LineBegin(), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(LineEnd(), flagSet0).hasLineEndAtEnd, true)
    assertEquals(Pattern(Dot(), flagSet0).hasLineEndAtEnd, false)
    assertEquals(Pattern(LineEnd(), flagSet1).hasLineEndAtEnd, false)
  }

  test("PatternExtensions.PatternOps#isConstant") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).isConstant, true)
    assertEquals(Pattern(Disjunction(Seq(Star(false, Dot()), Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Star(false, Dot()))), flagSet).isConstant, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).isConstant, true)
    assertEquals(Pattern(Sequence(Seq(Star(false, Dot()), Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), Star(false, Dot()))), flagSet).isConstant, false)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(Capture(1, Star(false, Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(NamedCapture(1, "x", Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(NamedCapture(1, "x", Star(false, Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(Group(Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(Group(Star(false, Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(Star(false, Dot()), flagSet).isConstant, false)
    assertEquals(Pattern(Plus(false, Dot()), flagSet).isConstant, false)
    assertEquals(Pattern(Question(false, Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(Repeat(false, 1, None, Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(Repeat(false, 1, Some(None), Dot()), flagSet).isConstant, false)
    assertEquals(Pattern(Repeat(false, 1, Some(Some(2)), Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(LookAhead(false, Star(false, Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(LookBehind(false, Star(false, Dot())), flagSet).isConstant, false)
  }

  test("PatternExtensions.PatternOps#size") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).size, 3)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).size, 2)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).size, 1)
    assertEquals(Pattern(NamedCapture(1, "x", Dot()), flagSet).size, 1)
    assertEquals(Pattern(Group(Dot()), flagSet).size, 1)
    assertEquals(Pattern(Star(false, Dot()), flagSet).size, 2)
    assertEquals(Pattern(Plus(false, Dot()), flagSet).size, 2)
    assertEquals(Pattern(Question(false, Dot()), flagSet).size, 2)
    assertEquals(Pattern(Repeat(false, 2, None, Dot()), flagSet).size, 2)
    assertEquals(Pattern(Repeat(false, 2, Some(None), Dot()), flagSet).size, 4)
    assertEquals(Pattern(Repeat(false, 2, Some(Some(3)), Dot()), flagSet).size, 4)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).size, 2)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).size, 2)
    assertEquals(Pattern(Dot(), flagSet).size, 1)
  }

  test("PatternExtensions.PatternOps#alphabet") {
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, false, true, false, false, false)
    val flagSet2 = FlagSet(false, false, false, true, false, false)
    val flagSet3 = FlagSet(false, true, false, false, false, false)
    val flagSet4 = FlagSet(false, true, false, false, true, false)
    val word = IChar.Word
    val lineTerminator = IChar.LineTerminator
    val dot16 = IChar.Any16.diff(IChar.LineTerminator)
    val dot = IChar.Any.diff(IChar.LineTerminator)
    assertEquals(Pattern(Sequence(Seq.empty), flagSet0).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(Dot(), flagSet0).alphabet, Success(ICharSet.any(false, false).add(dot16)))
    assertEquals(
      Pattern(Disjunction(Seq(Character('A'), Character('Z'))), flagSet0).alphabet,
      Success(ICharSet.any(false, false).add(IChar('A')).add(IChar('Z')))
    )
    assertEquals(
      Pattern(WordBoundary(false), flagSet0).alphabet,
      Success(ICharSet.any(false, false).add(word, CharKind.Word))
    )
    assertEquals(
      Pattern(LineBegin(), flagSet1).alphabet,
      Success(ICharSet.any(false, false).add(lineTerminator, CharKind.LineTerminator))
    )
    assertEquals(
      Pattern(Sequence(Seq(LineBegin(), WordBoundary(false))), flagSet1).alphabet,
      Success(ICharSet.any(false, false).add(lineTerminator, CharKind.LineTerminator).add(word, CharKind.Word))
    )
    assertEquals(Pattern(Capture(1, Dot()), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(Group(Dot()), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(Star(false, Dot()), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(Plus(false, Dot()), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(Question(false, Dot()), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(Repeat(false, 2, None, Dot()), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(Dot(), flagSet2).alphabet, Success(ICharSet.any(false, false)))
    assertEquals(Pattern(Sequence(Seq.empty), flagSet3).alphabet, Success(ICharSet.any(true, false)))
    assertEquals(
      Pattern(Dot(), flagSet3).alphabet,
      Success(ICharSet.any(true, false).add(IChar.canonicalize(dot16, false)))
    )
    assertEquals(
      Pattern(Disjunction(Seq(Character('A'), Character('Z'))), flagSet3).alphabet,
      Success(ICharSet.any(true, false).add(IChar('A')).add(IChar('Z')))
    )
    assertEquals(Pattern(Sequence(Seq.empty), flagSet4).alphabet, Success(ICharSet.any(true, true)))
    assertEquals(
      Pattern(Dot(), flagSet4).alphabet,
      Success(ICharSet.any(true, true).add(IChar.canonicalize(dot, true)))
    )
  }

  test("PatternExtensions.PatternOps#needsLineTerminatorDistinction") {
    val flagSet0 = FlagSet(false, false, true, false, false, false)
    val flagSet1 = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineBegin())), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), Dot())), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineBegin())), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(Capture(1, LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Capture(1, Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(NamedCapture(1, "foo", LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(Group(LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Group(Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(Star(false, LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Star(false, Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(Plus(false, LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Plus(false, Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(Question(false, LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Question(false, Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(Repeat(false, 2, None, LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Repeat(false, 2, None, Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(LookAhead(false, LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(LookBehind(false, LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(LineBegin(), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(LineEnd(), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(WordBoundary(true), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(WordBoundary(false), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(Dot(), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(LineBegin(), flagSet1).needsLineTerminatorDistinction, false)
  }

  test("PatternExtensions.PatternOps#needsWordDistinction") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), WordBoundary(false))), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Disjunction(Seq(WordBoundary(false), Dot())), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), WordBoundary(false))), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Sequence(Seq(WordBoundary(false), Dot())), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Capture(1, WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(NamedCapture(1, "foo", WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Group(WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Group(Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Star(false, WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Star(false, Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Plus(false, WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Plus(false, Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Question(false, WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Question(false, Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Repeat(false, 2, None, WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Repeat(false, 2, None, Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(LookAhead(false, WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(LookBehind(false, WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(WordBoundary(true), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(WordBoundary(false), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(LineBegin(), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(LineEnd(), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Dot(), flagSet).needsWordDistinction, false)
  }

  test("PatternExtensions.PatternOps#parts") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    val seq = Sequence(Seq(Character('x'), Character('y'), Character('z')))
    assertEquals(
      Pattern(Sequence(Seq(Character('x'), Character('y'), Character('z'), Dot(), Character('0'))), flagSet).parts,
      Set(UString("xyz"))
    )
    assertEquals(
      Pattern(
        Sequence(Seq(Character('x'), Character('y'), Character('z'), Dot(), Character('0'))),
        flagSet.copy(ignoreCase = true)
      ).parts,
      Set(UString("XYZ"))
    )
    assertEquals(Pattern(Sequence(Seq(seq)), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(Disjunction(Seq(seq, Dot())), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(Capture(1, seq), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(NamedCapture(1, "x", seq), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(Group(seq), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(Star(false, seq), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(Plus(false, seq), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(Question(false, seq), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(Repeat(false, 2, None, seq), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(LookAhead(false, seq), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(LookBehind(false, seq), flagSet).parts, Set(UString("xyz")))
    assertEquals(Pattern(Dot(), flagSet).parts, Set.empty[UString])
    assertEquals(Pattern(Character('x'), flagSet).parts, Set.empty[UString])
  }

  test("PatternExtensions.PatternOps#capturesSize") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Capture(1, Dot()), Capture(2, Dot()))), flagSet).capturesSize, 2)
    assertEquals(Pattern(Sequence(Seq(Capture(1, Dot()), Capture(2, Dot()))), flagSet).capturesSize, 2)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).capturesSize, 1)
    assertEquals(Pattern(Capture(1, Capture(2, Dot())), flagSet).capturesSize, 2)
    assertEquals(Pattern(NamedCapture(1, "x", Dot()), flagSet).capturesSize, 1)
    assertEquals(Pattern(NamedCapture(1, "x", Capture(2, Dot())), flagSet).capturesSize, 2)
    assertEquals(Pattern(Group(Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Star(false, Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Plus(false, Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Question(false, Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Repeat(false, 2, None, Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Dot(), flagSet).capturesSize, 0)
  }
}
