package codes.quine.labs.recheck.regexp

import scala.language.implicitConversions

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.regexp.Pattern.*
import codes.quine.labs.recheck.regexp.PatternExtensions.*
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class PatternExtensionsSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  test("PatternExtensions.AtomNodeOps#toCharSet"):
    assertEquals(Character('x').toCharSet(false), CharSet.single('x'))
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Word).toCharSet(false), EcmaScriptChars.Word)
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Word).toCharSet(false),
      EcmaScriptChars.Word.complement(UnitMode.Utf16)
    )
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Word).toCharSet(true),
      EcmaScriptChars.Word.complement(UnitMode.CodePoint)
    )
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Digit).toCharSet(false), EcmaScriptChars.Digit)
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Digit).toCharSet(false),
      EcmaScriptChars.Digit.complement(UnitMode.Utf16)
    )
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Space).toCharSet(false), EcmaScriptChars.Space)
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Space).toCharSet(false),
      EcmaScriptChars.Space.complement(UnitMode.Utf16)
    )
    assertEquals(
      UnicodeProperty(false, "ASCII", EcmaScriptChars.unicodeProperty("ASCII").get).toCharSet(false),
      EcmaScriptChars.unicodeProperty("ASCII").get
    )
    assertEquals(
      UnicodeProperty(true, "ASCII", EcmaScriptChars.unicodeProperty("ASCII").get.complement(UnitMode.CodePoint))
        .toCharSet(true),
      EcmaScriptChars.unicodeProperty("ASCII").get.complement(UnitMode.CodePoint)
    )
    assertEquals(
      UnicodeProperty(false, "L", EcmaScriptChars.unicodeProperty("L").get).toCharSet(false),
      EcmaScriptChars.unicodeProperty("L").get
    )
    assertEquals(
      UnicodeProperty(true, "L", EcmaScriptChars.unicodeProperty("L").get.complement(UnitMode.CodePoint))
        .toCharSet(true),
      EcmaScriptChars.unicodeProperty("L").get.complement(UnitMode.CodePoint)
    )
    val Hira = EcmaScriptChars.unicodePropertyValue("sc", "Hira").get
    assertEquals(UnicodePropertyValue(false, "sc", "Hira", Hira).toCharSet(false), Hira)
    assertEquals(
      UnicodePropertyValue(true, "sc", "Hira", Hira.complement(UnitMode.CodePoint)).toCharSet(true),
      Hira.complement(UnitMode.CodePoint)
    )
    assertEquals(
      CharacterClass(false, Seq(Character('a'), Character('A'))).toCharSet(false),
      CharSet.single('a').union(CharSet.single('A'))
    )
    assertEquals(
      CharacterClass(true, Seq(Character('a'), Character('A'))).toCharSet(false),
      CharSet.single('a').union(CharSet.single('A')) // Not complemented is intentionally.
    )
    assertEquals(ClassRange('a', 'a').toCharSet(false), CharSet.single('a'))
    assertEquals(ClassRange('a', 'z').toCharSet(false), CharSet.range('a', 'z'))

  test("PatternExtensions.NodeOps#captureRange"):
    assertEquals(Sequence(Seq(Capture(1, Dot()), Capture(2, Dot()))).captureRange, CaptureRange(Some((1, 2))))
    assertEquals(Disjunction(Seq(Capture(1, Dot()), Capture(2, Dot()))).captureRange, CaptureRange(Some((1, 2))))
    assertEquals(Capture(1, Dot()).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(Capture(1, Capture(2, Dot())).captureRange, CaptureRange(Some((1, 2))))
    assertEquals(NamedCapture(1, "x", Dot()).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(NamedCapture(1, "x", Capture(2, Dot())).captureRange, CaptureRange(Some((1, 2))))
    assertEquals(Group(Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(Repeat(Quantifier.Exact(1, false), Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(WordBoundary(false).captureRange, CaptureRange(None))
    assertEquals(LineBegin().captureRange, CaptureRange(None))
    assertEquals(LineEnd().captureRange, CaptureRange(None))
    assertEquals(LookAhead(false, Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(LookBehind(false, Capture(1, Dot())).captureRange, CaptureRange(Some((1, 1))))
    assertEquals(Character('a').captureRange, CaptureRange(None))
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Digit).captureRange, CaptureRange(None))
    assertEquals(UnicodeProperty(false, "L", null).captureRange, CaptureRange(None))
    assertEquals(UnicodePropertyValue(false, "sc", "Hira", null).captureRange, CaptureRange(None))
    assertEquals(CharacterClass(false, Seq(Character('a'))).captureRange, CaptureRange(None))
    assertEquals(Dot().captureRange, CaptureRange(None))
    assertEquals(BackReference(1).captureRange, CaptureRange(None))
    assertEquals(NamedBackReference(1, "foo").captureRange, CaptureRange(None))

  test("PatternExtensions.NodeOps#isEmpty"):
    assertEquals(Sequence(Seq(Dot(), Dot())).canMatchEmpty, false)
    assertEquals(Sequence(Seq.empty).canMatchEmpty, true)
    assertEquals(Disjunction(Seq(Dot(), Dot())).canMatchEmpty, false)
    assertEquals(Disjunction(Seq(Sequence(Seq.empty), Dot())).canMatchEmpty, true)
    assertEquals(Disjunction(Seq(Dot(), Sequence(Seq.empty))).canMatchEmpty, true)
    assertEquals(Capture(1, Dot()).canMatchEmpty, false)
    assertEquals(Capture(1, Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(NamedCapture(1, "x", Dot()).canMatchEmpty, false)
    assertEquals(NamedCapture(1, "x", Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Group(Dot()).canMatchEmpty, false)
    assertEquals(Group(Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Star(false), Dot()).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Star(false), Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Plus(false), Dot()).canMatchEmpty, false)
    assertEquals(Repeat(Quantifier.Plus(false), Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Question(false), Dot()).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Question(false), Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Exact(0, false), Dot()).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Exact(0, false), Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Exact(1, false), Dot()).canMatchEmpty, false)
    assertEquals(Repeat(Quantifier.Exact(1, false), Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Unbounded(0, false), Dot()).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Unbounded(0, false), Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Unbounded(1, false), Dot()).canMatchEmpty, false)
    assertEquals(Repeat(Quantifier.Unbounded(1, false), Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Bounded(0, 1, false), Dot()).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Bounded(0, 1, false), Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(Repeat(Quantifier.Bounded(1, 2, false), Dot()).canMatchEmpty, false)
    assertEquals(Repeat(Quantifier.Bounded(1, 2, false), Sequence(Seq.empty)).canMatchEmpty, true)
    assertEquals(WordBoundary(false).canMatchEmpty, true)
    assertEquals(LineBegin().canMatchEmpty, true)
    assertEquals(LineEnd().canMatchEmpty, true)
    assertEquals(LookAhead(false, Dot()).canMatchEmpty, true)
    assertEquals(LookBehind(false, Dot()).canMatchEmpty, true)
    assertEquals(Character('a').canMatchEmpty, false)
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Digit).canMatchEmpty, false)
    assertEquals(UnicodeProperty(false, "L", null).canMatchEmpty, false)
    assertEquals(UnicodePropertyValue(false, "sc", "Hira", null).canMatchEmpty, false)
    assertEquals(CharacterClass(false, Seq(Character('a'))).canMatchEmpty, false)
    assertEquals(Dot().canMatchEmpty, false)
    assertEquals(BackReference(1).canMatchEmpty, true)
    assertEquals(NamedBackReference(1, "foo").canMatchEmpty, true)

  test("PatternExtensions.CaptureRange#merge"):
    assertEquals(CaptureRange(None).merge(CaptureRange(None)), CaptureRange(None))
    assertEquals(CaptureRange(Some((1, 1))).merge(CaptureRange(None)), CaptureRange(Some((1, 1))))
    assertEquals(CaptureRange(None).merge(CaptureRange(Some((2, 2)))), CaptureRange(Some((2, 2))))
    assertEquals(CaptureRange(Some((1, 1))).merge(CaptureRange(Some((2, 2)))), CaptureRange(Some((1, 2))))

  test("PatternExtensions.PatternOps#existsLineAssertion"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineBegin())), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), Dot())), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineBegin())), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(Capture(1, LineBegin()), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(NamedCapture(1, "foo", LineBegin()), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(Group(LineBegin()), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(Group(Dot()), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), LineBegin()), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), Dot()), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), LineBegin()), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), Dot()), flagSet).existsLineAssertion, false)
    assertEquals(
      Pattern(Repeat(Quantifier.Question(false), LineBegin()), flagSet).existsLineAssertion,
      true
    )
    assertEquals(Pattern(Repeat(Quantifier.Question(false), Dot()), flagSet).existsLineAssertion, false)
    assertEquals(
      Pattern(Repeat(Quantifier.Exact(2, false), LineBegin()), flagSet).existsLineAssertion,
      true
    )
    assertEquals(Pattern(Repeat(Quantifier.Exact(2, false), Dot()), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(LookAhead(false, LineBegin()), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(LookBehind(false, LineBegin()), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(LineBegin(), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(LineEnd(), flagSet).existsLineAssertion, true)
    assertEquals(Pattern(WordBoundary(true), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(WordBoundary(false), flagSet).existsLineAssertion, false)
    assertEquals(Pattern(Dot(), flagSet).existsLineAssertion, false)

  test("PatternExtensions.PatternOps#existsLineAssertionInMiddle"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineBegin())), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), Dot())), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineEnd())), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Disjunction(Seq(LineEnd(), Dot())), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineBegin())), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineEnd())), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Sequence(Seq(LineEnd(), Dot())), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Capture(1, LineBegin()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Capture(1, LineEnd()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(NamedCapture(1, "foo", LineBegin()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(NamedCapture(1, "foo", LineEnd()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Group(LineBegin()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Group(LineEnd()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Group(Dot()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), LineBegin()), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), LineEnd()), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), Dot()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), LineBegin()), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), LineEnd()), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), Dot()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(LookAhead(false, LineBegin()), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(LookAhead(false, LineEnd()), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(LookBehind(false, LineBegin()), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(LookBehind(false, LineEnd()), flagSet).existsLineAssertionInMiddle, true)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(LineBegin(), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(LineEnd(), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(WordBoundary(true), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(WordBoundary(false), flagSet).existsLineAssertionInMiddle, false)
    assertEquals(Pattern(Dot(), flagSet).existsLineAssertionInMiddle, false)

  test("PatternExtensions.PatternOps#everyBeginPointIsLineBegin"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), LineBegin())), flagSet).everyBeginPointIsLineBegin, true)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineBegin())), flagSet).everyBeginPointIsLineBegin, false)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), Dot())), flagSet).everyBeginPointIsLineBegin, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).everyBeginPointIsLineBegin, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineBegin())), flagSet).everyBeginPointIsLineBegin, false)
    assertEquals(Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet).everyBeginPointIsLineBegin, true)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).everyBeginPointIsLineBegin, false)
    assertEquals(Pattern(Capture(1, LineBegin()), flagSet).everyBeginPointIsLineBegin, true)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).everyBeginPointIsLineBegin, false)
    assertEquals(Pattern(NamedCapture(1, "foo", LineBegin()), flagSet).everyBeginPointIsLineBegin, true)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet).everyBeginPointIsLineBegin, false)
    assertEquals(Pattern(Group(LineBegin()), flagSet).everyBeginPointIsLineBegin, true)
    assertEquals(Pattern(Group(Dot()), flagSet).everyBeginPointIsLineBegin, false)
    assertEquals(Pattern(LineBegin(), flagSet).everyBeginPointIsLineBegin, true)
    assertEquals(Pattern(LineEnd(), flagSet).everyBeginPointIsLineBegin, false)
    assertEquals(Pattern(Dot(), flagSet).everyBeginPointIsLineBegin, false)

  test("PatternExtensions.PatternOps#everyBeginPointIsNotLineBegin"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), LineBegin())), flagSet).everyBeginPointIsNotLineBegin, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineBegin())), flagSet).everyBeginPointIsNotLineBegin, false)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), Dot())), flagSet).everyBeginPointIsNotLineBegin, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).everyBeginPointIsNotLineBegin, true)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineBegin())), flagSet).everyBeginPointIsNotLineBegin, true)
    assertEquals(Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet).everyBeginPointIsNotLineBegin, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).everyBeginPointIsNotLineBegin, true)
    assertEquals(Pattern(Capture(1, LineBegin()), flagSet).everyBeginPointIsNotLineBegin, false)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).everyBeginPointIsNotLineBegin, true)
    assertEquals(Pattern(NamedCapture(1, "foo", LineBegin()), flagSet).everyBeginPointIsNotLineBegin, false)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet).everyBeginPointIsNotLineBegin, true)
    assertEquals(Pattern(Group(LineBegin()), flagSet).everyBeginPointIsNotLineBegin, false)
    assertEquals(Pattern(Group(Dot()), flagSet).everyBeginPointIsNotLineBegin, true)
    assertEquals(Pattern(LineBegin(), flagSet).everyBeginPointIsNotLineBegin, false)
    assertEquals(Pattern(LineEnd(), flagSet).everyBeginPointIsNotLineBegin, true)
    assertEquals(Pattern(Dot(), flagSet).everyBeginPointIsNotLineBegin, true)

  test("PatternExtensions.PatternOps#everyEndPointIsLineEnd"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(LineEnd(), LineEnd())), flagSet).everyEndPointIsLineEnd, true)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineEnd())), flagSet).everyEndPointIsLineEnd, false)
    assertEquals(Pattern(Disjunction(Seq(LineEnd(), Dot())), flagSet).everyEndPointIsLineEnd, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).everyEndPointIsLineEnd, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineEnd())), flagSet).everyEndPointIsLineEnd, true)
    assertEquals(Pattern(Sequence(Seq(LineEnd(), Dot())), flagSet).everyEndPointIsLineEnd, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).everyEndPointIsLineEnd, false)
    assertEquals(Pattern(Capture(1, LineEnd()), flagSet).everyEndPointIsLineEnd, true)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).everyEndPointIsLineEnd, false)
    assertEquals(Pattern(NamedCapture(1, "foo", LineEnd()), flagSet).everyEndPointIsLineEnd, true)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet).everyEndPointIsLineEnd, false)
    assertEquals(Pattern(Group(LineEnd()), flagSet).everyEndPointIsLineEnd, true)
    assertEquals(Pattern(Group(Dot()), flagSet).everyEndPointIsLineEnd, false)
    assertEquals(Pattern(LineBegin(), flagSet).everyEndPointIsLineEnd, false)
    assertEquals(Pattern(LineEnd(), flagSet).everyEndPointIsLineEnd, true)
    assertEquals(Pattern(Dot(), flagSet).everyEndPointIsLineEnd, false)

  test("PatternExtensions.PatternOps#everyEndPointIsNotLineEnd"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(LineEnd(), LineEnd())), flagSet).everyEndPointIsNotLineEnd, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineEnd())), flagSet).everyEndPointIsNotLineEnd, false)
    assertEquals(Pattern(Disjunction(Seq(LineEnd(), Dot())), flagSet).everyEndPointIsNotLineEnd, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).everyEndPointIsNotLineEnd, true)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineEnd())), flagSet).everyEndPointIsNotLineEnd, false)
    assertEquals(Pattern(Sequence(Seq(LineEnd(), Dot())), flagSet).everyEndPointIsNotLineEnd, true)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).everyEndPointIsNotLineEnd, true)
    assertEquals(Pattern(Capture(1, LineEnd()), flagSet).everyEndPointIsNotLineEnd, false)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).everyEndPointIsNotLineEnd, true)
    assertEquals(Pattern(NamedCapture(1, "foo", LineEnd()), flagSet).everyEndPointIsNotLineEnd, false)
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet).everyEndPointIsNotLineEnd, true)
    assertEquals(Pattern(Group(LineEnd()), flagSet).everyEndPointIsNotLineEnd, false)
    assertEquals(Pattern(Group(Dot()), flagSet).everyEndPointIsNotLineEnd, true)
    assertEquals(Pattern(LineBegin(), flagSet).everyEndPointIsNotLineEnd, true)
    assertEquals(Pattern(LineEnd(), flagSet).everyEndPointIsNotLineEnd, false)
    assertEquals(Pattern(Dot(), flagSet).everyEndPointIsNotLineEnd, true)

  test("PatternExtensions.PatternOps#isConstant"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).isConstant, true)
    assertEquals(Pattern(Disjunction(Seq(Repeat(Quantifier.Star(false), Dot()), Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Repeat(Quantifier.Star(false), Dot()))), flagSet).isConstant, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).isConstant, true)
    assertEquals(Pattern(Sequence(Seq(Repeat(Quantifier.Star(false), Dot()), Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), Repeat(Quantifier.Star(false), Dot()))), flagSet).isConstant, false)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(Capture(1, Repeat(Quantifier.Star(false), Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(NamedCapture(1, "x", Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(NamedCapture(1, "x", Repeat(Quantifier.Star(false), Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(Group(Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(Group(Repeat(Quantifier.Star(false), Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), Dot()), flagSet).isConstant, false)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), Dot()), flagSet).isConstant, false)
    assertEquals(Pattern(Repeat(Quantifier.Question(false), Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(Repeat(Quantifier.Exact(1, false), Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(Repeat(Quantifier.Unbounded(1, false), Dot()), flagSet).isConstant, false)
    assertEquals(Pattern(Repeat(Quantifier.Bounded(1, 2, false), Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(LookAhead(false, Repeat(Quantifier.Star(false), Dot())), flagSet).isConstant, false)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).isConstant, true)
    assertEquals(Pattern(LookBehind(false, Repeat(Quantifier.Star(false), Dot())), flagSet).isConstant, false)

  test("PatternExtensions.PatternOps#size"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Dot(), Dot())), flagSet).size, 3)
    assertEquals(Pattern(Sequence(Seq(Dot(), Dot())), flagSet).size, 2)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).size, 1)
    assertEquals(Pattern(NamedCapture(1, "x", Dot()), flagSet).size, 1)
    assertEquals(Pattern(Group(Dot()), flagSet).size, 1)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), Dot()), flagSet).size, 2)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), Dot()), flagSet).size, 2)
    assertEquals(Pattern(Repeat(Quantifier.Question(false), Dot()), flagSet).size, 2)
    assertEquals(Pattern(Repeat(Quantifier.Exact(2, false), Dot()), flagSet).size, 2)
    assertEquals(Pattern(Repeat(Quantifier.Unbounded(2, false), Dot()), flagSet).size, 4)
    assertEquals(Pattern(Repeat(Quantifier.Bounded(2, 3, false), Dot()), flagSet).size, 4)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).size, 2)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).size, 2)
    assertEquals(Pattern(Dot(), flagSet).size, 1)

  test("PatternExtensions.PatternOps#alphabet"):
    val flagSet0 = FlagSet(false, false, false, false, false, false)
    val flagSet1 = FlagSet(false, false, true, false, false, false)
    val flagSet2 = FlagSet(false, false, false, true, false, false)
    val flagSet3 = FlagSet(false, true, false, false, false, false)
    val flagSet4 = FlagSet(false, true, false, false, true, false)
    val word = EcmaScriptChars.Word
    val lineTerminator = EcmaScriptChars.LineTerminator
    val dot16 = EcmaScriptChars.any(false).diff(EcmaScriptChars.LineTerminator)
    val dot = EcmaScriptChars.any(true).diff(EcmaScriptChars.LineTerminator)
    assertEquals(Pattern(Sequence(Seq.empty), flagSet0).alphabet, PatternAlphabet.build(false, false))
    assertEquals(Pattern(Dot(), flagSet0).alphabet, PatternAlphabet.build(false, false, Seq(dot16)))
    assertEquals(
      Pattern(Disjunction(Seq(Character('A'), Character('Z'))), flagSet0).alphabet,
      PatternAlphabet.build(false, false, Seq(CharSet.single('A'), CharSet.single('Z')))
    )
    assertEquals(
      Pattern(WordBoundary(false), flagSet0).alphabet,
      PatternAlphabet.build(false, false, word = Some(word))
    )
    assertEquals(
      Pattern(LineBegin(), flagSet1).alphabet,
      PatternAlphabet.build(false, false, lineTerminator = Some(lineTerminator))
    )
    assertEquals(
      Pattern(Sequence(Seq(LineBegin(), WordBoundary(false))), flagSet1).alphabet,
      PatternAlphabet.build(false, false, lineTerminator = Some(lineTerminator), word = Some(word))
    )
    assertEquals(Pattern(Capture(1, Dot()), flagSet2).alphabet, PatternAlphabet.build(false, false))
    assertEquals(Pattern(NamedCapture(1, "foo", Dot()), flagSet2).alphabet, PatternAlphabet.build(false, false))
    assertEquals(Pattern(Group(Dot()), flagSet2).alphabet, PatternAlphabet.build(false, false))
    assertEquals(Pattern(Repeat(Quantifier.Star(false), Dot()), flagSet2).alphabet, PatternAlphabet.build(false, false))
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), Dot()), flagSet2).alphabet, PatternAlphabet.build(false, false))
    assertEquals(
      Pattern(Repeat(Quantifier.Question(false), Dot()), flagSet2).alphabet,
      PatternAlphabet.build(false, false)
    )
    assertEquals(
      Pattern(Repeat(Quantifier.Exact(2, false), Dot()), flagSet2).alphabet,
      PatternAlphabet.build(false, false)
    )
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet2).alphabet, PatternAlphabet.build(false, false))
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet2).alphabet, PatternAlphabet.build(false, false))
    assertEquals(Pattern(Dot(), flagSet2).alphabet, PatternAlphabet.build(false, false))
    assertEquals(Pattern(Sequence(Seq.empty), flagSet3).alphabet, PatternAlphabet.build(true, false))
    assertEquals(
      Pattern(Dot(), flagSet3).alphabet,
      PatternAlphabet.build(true, false, Seq(EcmaScriptChars.canonicalize(dot16, false)))
    )
    assertEquals(
      Pattern(Disjunction(Seq(Character('A'), Character('Z'))), flagSet3).alphabet,
      PatternAlphabet.build(true, false, Seq(CharSet.single('A'), CharSet.single('Z')))
    )
    assertEquals(Pattern(Sequence(Seq.empty), flagSet4).alphabet, PatternAlphabet.build(true, true))
    assertEquals(
      Pattern(Dot(), flagSet4).alphabet,
      PatternAlphabet.build(true, true, Seq(EcmaScriptChars.canonicalize(dot, true)))
    )

  test("PatternExtensions.PatternOps#needsLineTerminatorDistinction"):
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
    assertEquals(Pattern(Repeat(Quantifier.Star(false), LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), LineBegin()), flagSet0).needsLineTerminatorDistinction, true)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(
      Pattern(Repeat(Quantifier.Question(false), LineBegin()), flagSet0).needsLineTerminatorDistinction,
      true
    )
    assertEquals(Pattern(Repeat(Quantifier.Question(false), Dot()), flagSet0).needsLineTerminatorDistinction, false)
    assertEquals(
      Pattern(Repeat(Quantifier.Exact(2, false), LineBegin()), flagSet0).needsLineTerminatorDistinction,
      true
    )
    assertEquals(Pattern(Repeat(Quantifier.Exact(2, false), Dot()), flagSet0).needsLineTerminatorDistinction, false)
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

  test("PatternExtension.PatternOps#needsInputTerminatorDistinction"):
    val flagSet0 = FlagSet(false, false, true, false, false, false)
    val flagSet1 = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(LineBegin(), flagSet0).needsInputTerminatorDistinction, true)
    assertEquals(Pattern(LineEnd(), flagSet0).needsInputTerminatorDistinction, true)
    assertEquals(Pattern(Dot(), flagSet0).needsInputTerminatorDistinction, false)
    assertEquals(Pattern(Dot(), flagSet1).needsInputTerminatorDistinction, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineBegin())), flagSet1).needsInputTerminatorDistinction, true)
    assertEquals(Pattern(Sequence(Seq(LineBegin(), Dot(), LineEnd())), flagSet1).needsInputTerminatorDistinction, false)
    assertEquals(Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet1).needsInputTerminatorDistinction, false)
    assertEquals(Pattern(Sequence(Seq(Dot(), LineEnd())), flagSet1).needsInputTerminatorDistinction, false)
    assertEquals(Pattern(Disjunction(Seq(LineBegin(), Dot())), flagSet1).needsInputTerminatorDistinction, true)
    assertEquals(Pattern(Disjunction(Seq(Dot(), LineEnd())), flagSet1).needsInputTerminatorDistinction, true)

  test("PatternExtensions.PatternOps#needsWordDistinction"):
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
    assertEquals(Pattern(Repeat(Quantifier.Star(false), WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Repeat(Quantifier.Question(false), WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Repeat(Quantifier.Question(false), Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Repeat(Quantifier.Exact(2, false), WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(Repeat(Quantifier.Exact(2, false), Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(LookAhead(false, WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(LookBehind(false, WordBoundary(false)), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(WordBoundary(true), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(WordBoundary(false), flagSet).needsWordDistinction, true)
    assertEquals(Pattern(LineBegin(), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(LineEnd(), flagSet).needsWordDistinction, false)
    assertEquals(Pattern(Dot(), flagSet).needsWordDistinction, false)

  test("PatternExtension.PatternOps#needsSigmaStarAtBegin"):
    val flagSet0 = FlagSet(false, false, true, false, false, false)
    val flagSet1 = FlagSet(false, false, false, false, false, false)
    val flagSet2 = FlagSet(false, false, false, false, false, true)
    assertEquals(Pattern(Dot(), flagSet0).needsSigmaStarAtBegin, true)
    assertEquals(Pattern(LineBegin(), flagSet0).needsSigmaStarAtBegin, true)
    assertEquals(Pattern(Dot(), flagSet1).needsSigmaStarAtBegin, true)
    assertEquals(Pattern(LineBegin(), flagSet1).needsSigmaStarAtBegin, false)
    assertEquals(Pattern(Dot(), flagSet2).needsSigmaStarAtBegin, false)

  test("PatternExtension.PatternOps#needsSigmaStarAtEnd"):
    val flagSet0 = FlagSet(false, false, true, false, false, false)
    val flagSet1 = FlagSet(false, false, false, false, false, false)
    val flagSet2 = FlagSet(false, false, false, false, false, true)
    assertEquals(Pattern(Dot(), flagSet0).needsSigmaStarAtEnd, true)
    assertEquals(Pattern(LineEnd(), flagSet0).needsSigmaStarAtEnd, true)
    assertEquals(Pattern(Dot(), flagSet1).needsSigmaStarAtEnd, true)
    assertEquals(Pattern(LineEnd(), flagSet1).needsSigmaStarAtEnd, false)
    assertEquals(Pattern(Dot(), flagSet2).needsSigmaStarAtEnd, true)
    assertEquals(Pattern(LineEnd(), flagSet2).needsSigmaStarAtEnd, false)

  test("PatternExtensions.PatternOps#parts"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    val seq = Sequence(Seq(Character('x'), Character('y'), Character('z')))
    assertEquals(
      Pattern(Sequence(Seq(Character('x'), Character('y'), Character('z'), Dot(), Character('0'))), flagSet).parts,
      Set(UStringUtil.fromString("xyz"))
    )
    assertEquals(
      Pattern(
        Sequence(Seq(Character('x'), Character('y'), Character('z'), Dot(), Character('0'))),
        flagSet.copy(ignoreCase = true)
      ).parts,
      Set(UStringUtil.fromString("XYZ"))
    )
    assertEquals(Pattern(Sequence(Seq(seq)), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(Disjunction(Seq(seq, Dot())), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(Capture(1, seq), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(NamedCapture(1, "x", seq), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(Group(seq), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(Repeat(Quantifier.Star(false), seq), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), seq), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(Repeat(Quantifier.Question(false), seq), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(Repeat(Quantifier.Exact(2, false), seq), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(LookAhead(false, seq), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(LookBehind(false, seq), flagSet).parts, Set(UStringUtil.fromString("xyz")))
    assertEquals(Pattern(Dot(), flagSet).parts, Set.empty[UString])
    assertEquals(Pattern(Character('x'), flagSet).parts, Set.empty[UString])

    val ignoreCase = FlagSet(false, true, false, false, false, false)
    assertEquals(
      Pattern(Sequence(Seq(Character('a'), Character('\n'), Character('\\'), Character('b'))), ignoreCase).parts,
      Set(UStringUtil.fromString("A\n\\B"))
    )
    assertEquals(
      Pattern(Sequence(Seq(Character('a'), Character(0x1f600))), ignoreCase).parts,
      Set(UStringUtil.fromString("A😀"))
    )

  test("PatternExtensions.PatternOps#capturesSize"):
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(Pattern(Disjunction(Seq(Capture(1, Dot()), Capture(2, Dot()))), flagSet).capturesSize, 2)
    assertEquals(Pattern(Sequence(Seq(Capture(1, Dot()), Capture(2, Dot()))), flagSet).capturesSize, 2)
    assertEquals(Pattern(Capture(1, Dot()), flagSet).capturesSize, 1)
    assertEquals(Pattern(Capture(1, Capture(2, Dot())), flagSet).capturesSize, 2)
    assertEquals(Pattern(NamedCapture(1, "x", Dot()), flagSet).capturesSize, 1)
    assertEquals(Pattern(NamedCapture(1, "x", Capture(2, Dot())), flagSet).capturesSize, 2)
    assertEquals(Pattern(Group(Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Repeat(Quantifier.Star(false), Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Repeat(Quantifier.Plus(false), Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Repeat(Quantifier.Question(false), Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Repeat(Quantifier.Exact(2, false), Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(LookAhead(false, Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(LookBehind(false, Dot()), flagSet).capturesSize, 0)
    assertEquals(Pattern(Dot(), flagSet).capturesSize, 0)
