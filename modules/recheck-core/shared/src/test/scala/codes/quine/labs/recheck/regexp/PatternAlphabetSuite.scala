package codes.quine.labs.recheck.regexp

import codes.quine.labs.recheck.regexp.PatternAlphabet.CharKind
import codes.quine.labs.resyntax.charset.CharSet

class PatternAlphabetSuite extends munit.FunSuite:
  test("PatternAlphabet.build"):
    assertEquals(PatternAlphabet.build(false, true).pairs, IndexedSeq((EcmaScriptChars.any(true), CharKind.Normal)))
    assertEquals(PatternAlphabet.build(false, false).pairs, IndexedSeq((EcmaScriptChars.any(false), CharKind.Normal)))

    assert(PatternAlphabet.build(true, false).pairs.head._1.contains('A'))
    assert(!PatternAlphabet.build(true, false).pairs.head._1.contains('a'))
    assert(PatternAlphabet.build(true, true).pairs.head._1.contains('s'))
    assert(!PatternAlphabet.build(true, true).pairs.head._1.contains(0x017f))

    assertEquals(
      PatternAlphabet.build(false, false, Seq(CharSet.single('A'))).pairs,
      IndexedSeq(
        (CharSet.single('A'), CharKind.Normal),
        (CharSet.range(0, 'A' - 1).union(CharSet.range('B', 0xffff)), CharKind.Normal)
      )
    )
    assertEquals(
      PatternAlphabet
        .build(false, false, Seq(CharSet.single('a'), CharSet.single('A')), word = Some(EcmaScriptChars.Word))
        .pairs,
      IndexedSeq(
        (CharSet.single('a'), CharKind.Word),
        (CharSet.single('A'), CharKind.Word),
        (EcmaScriptChars.Word.diff(CharSet.of('a', 'A')), CharKind.Word),
        (EcmaScriptChars.any(false).diff(EcmaScriptChars.Word), CharKind.Normal)
      )
    )
    assertEquals(
      PatternAlphabet.build(false, false, Seq(CharSet.single('\n')), lineTerminator = Some(CharSet.single('\n'))).pairs,
      IndexedSeq(
        (CharSet.single('\n'), CharKind.LineTerminator),
        (EcmaScriptChars.any(false).diff(CharSet.single('\n')), CharKind.Normal)
      )
    )
    // An atom outside all characters, such as a lowercase letter with ignore-case, follows them.
    assertEquals(
      PatternAlphabet.build(true, false, word = Some(EcmaScriptChars.Word)).pairs.last,
      (CharSet.range('a', 'z'), CharKind.Word)
    )

  locally:
    val alphabet = PatternAlphabet.build(false, false, Seq(CharSet.single('A')), word = Some(CharSet.range('A', 'Z')))

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

  test("PatternAlphabet#dot"):
    val alphabet = PatternAlphabet.build(false, false, Seq(EcmaScriptChars.LineTerminator))
    assertEquals(alphabet.dot, alphabet.refineInvert(EcmaScriptChars.LineTerminator))
    assertEquals(alphabet.newline, alphabet.refine(EcmaScriptChars.LineTerminator))
