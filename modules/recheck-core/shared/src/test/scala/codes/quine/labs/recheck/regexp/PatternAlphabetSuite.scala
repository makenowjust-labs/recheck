package codes.quine.labs.recheck.regexp

import codes.quine.labs.recheck.regexp.PatternAlphabet.CharKind
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode

class PatternAlphabetSuite extends munit.FunSuite:
  val Word: CharSet = CharSet.fromIntervals(Seq(('0', '9' + 1), ('A', 'Z' + 1), ('_', '_' + 1), ('a', 'z' + 1)))

  test("PatternAlphabet.build"):
    assertEquals(
      PatternAlphabet.build(UnitMode.CodePoint).pairs,
      IndexedSeq((UnitMode.CodePoint.universe, CharKind.Normal))
    )
    assertEquals(PatternAlphabet.build(UnitMode.Utf16).pairs, IndexedSeq((UnitMode.Utf16.universe, CharKind.Normal)))
    assertEquals(PatternAlphabet.build(UnitMode.Byte).pairs, IndexedSeq((UnitMode.Byte.universe, CharKind.Normal)))

    assertEquals(
      PatternAlphabet.build(UnitMode.Utf16, Seq(CharSet.single('A'))).pairs,
      IndexedSeq(
        (CharSet.single('A'), CharKind.Normal),
        (CharSet.range(0, 'A' - 1).union(CharSet.range('B', 0xffff)), CharKind.Normal)
      )
    )
    assertEquals(
      PatternAlphabet.build(UnitMode.Utf16, Seq(CharSet.single('a'), CharSet.single('A')), word = Some(Word)).pairs,
      IndexedSeq(
        (CharSet.single('a'), CharKind.Word),
        (CharSet.single('A'), CharKind.Word),
        (Word.diff(CharSet.of('a', 'A')), CharKind.Word),
        (UnitMode.Utf16.universe.diff(Word), CharKind.Normal)
      )
    )
    assertEquals(
      PatternAlphabet
        .build(UnitMode.Utf16, Seq(CharSet.single('\n')), lineTerminator = Some(CharSet.single('\n')))
        .pairs,
      IndexedSeq(
        (CharSet.single('\n'), CharKind.LineTerminator),
        (UnitMode.Utf16.universe.diff(CharSet.single('\n')), CharKind.Normal)
      )
    )
    // `\r` and `\n` have their own kinds with `crlf`, where `\r\n` is one line terminator.
    assertEquals(
      PatternAlphabet
        .build(UnitMode.Utf16, lineTerminator = Some(CharSet.of(0x0a, 0x0d, 0x85)), crlf = true)
        .pairs,
      IndexedSeq(
        (CharSet.single('\r'), CharKind.CarriageReturn),
        (CharSet.single('\n'), CharKind.LineFeed),
        (CharSet.single(0x85), CharKind.LineTerminator),
        (UnitMode.Utf16.universe.diff(CharSet.of(0x0a, 0x0d, 0x85)), CharKind.Normal)
      )
    )
    // A set outside the mode is restricted.
    assertEquals(
      PatternAlphabet.build(UnitMode.Byte, Seq(CharSet.range(0xf0, 0x10ffff))).pairs.map(_._1),
      IndexedSeq(CharSet.range(0xf0, 0xff), CharSet.range(0, 0xef))
    )

  test("PatternAlphabet.build: preferred"):
    val canonical = CharSet.range(0, 'a' - 1).union(CharSet.range('z' + 1, 0xffff))
    val alphabet =
      PatternAlphabet.build(UnitMode.Utf16, Seq(CharSet.of('a', 'A'), CharSet.single('b')), preferred = Some(canonical))
    // An atom is narrowed to its preferred characters, unless it has none.
    assertEquals(
      alphabet.pairs.map(_._1),
      IndexedSeq(
        CharSet.single('A'),
        CharSet.single('b'),
        UnitMode.Utf16.universe.diff(CharSet.of('a', 'b', 'A')).intersect(canonical)
      )
    )
    assertEquals(alphabet.refine(CharSet.of('a', 'A')), Set((CharSet.single('A'), CharKind.Normal)))
    assertEquals(alphabet.refine(CharSet.single('b')), Set((CharSet.single('b'), CharKind.Normal)))

  locally:
    val alphabet =
      PatternAlphabet.build(UnitMode.Utf16, Seq(CharSet.single('A')), word = Some(CharSet.range('A', 'Z')))

    test("PatternAlphabet#refine"):
      assertEquals(
        alphabet.refine(CharSet.range('A', 'Z')),
        Set((CharSet.single('A'), CharKind.Word), (CharSet.range('B', 'Z'), CharKind.Word))
      )
      assertEquals(alphabet.refine(CharSet.range('A', 'B')), Set((CharSet.single('A'), CharKind.Word)))

    test("PatternAlphabet#refineInvert"):
      assertEquals(
        alphabet.refineInvert(CharSet.range('A', 'Z')),
        Set((CharSet.range(0, 'A' - 1).union(CharSet.range('Z' + 1, 0xffff)), CharKind.Normal))
      )

    test("PatternAlphabet#any"):
      assertEquals(alphabet.any, alphabet.pairs.toSet)
