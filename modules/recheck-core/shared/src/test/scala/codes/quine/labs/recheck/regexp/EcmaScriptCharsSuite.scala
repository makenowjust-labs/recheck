package codes.quine.labs.recheck.regexp

import codes.quine.labs.resyntax.charset.CharSet

class EcmaScriptCharsSuite extends munit.FunSuite:
  test("EcmaScriptChars.canonicalize"):
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single('a'), false), CharSet.single('A'))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single('A'), false), CharSet.single('A'))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single('a'), true), CharSet.single('a'))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single('A'), true), CharSet.single('a'))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single('n'), false), CharSet.single('N'))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single('N'), true), CharSet.single('n'))

    // U+FF21: FULLWIDTH LATIN CAPITAL LETTER A
    // U+FF41: FULLWIDTH LATIN SMALL LETTER A
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single(0xff41), false), CharSet.single(0xff21))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single(0xff21), true), CharSet.single(0xff41))

    // U+017F: LATIN SMALL LETTER LONG S
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single(0x017f), false), CharSet.single(0x017f))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single(0x017f), true), CharSet.single('s'))

  test("EcmaScriptChars.isPrintable"):
    assert(EcmaScriptChars.isPrintable('a'))
    assert(EcmaScriptChars.isPrintable(' '))
    assert(!EcmaScriptChars.isPrintable(0))
    assert(!EcmaScriptChars.isPrintable(0xffff))

  test("EcmaScriptChars.showChar"):
    assertEquals(EcmaScriptChars.showChar(0x09), "\\t")
    assertEquals(EcmaScriptChars.showChar(0x0a), "\\n")
    assertEquals(EcmaScriptChars.showChar(0x0b), "\\v")
    assertEquals(EcmaScriptChars.showChar(0x0c), "\\f")
    assertEquals(EcmaScriptChars.showChar(0x0d), "\\r")
    assertEquals(EcmaScriptChars.showChar(0x5c), "\\\\")
    assertEquals(EcmaScriptChars.showChar(0x01), "\\cA")
    assertEquals(EcmaScriptChars.showChar('a'), "a")
    assertEquals(EcmaScriptChars.showChar(0x1f363), "🍣")
    assertEquals(EcmaScriptChars.showChar(0), "\\x00")
    assertEquals(EcmaScriptChars.showChar(0xffff), "\\uFFFF")
    assertEquals(EcmaScriptChars.showChar(0x10ffff), "\\u{10FFFF}")

  test("EcmaScriptChars.showClass"):
    assertEquals(EcmaScriptChars.showClass(CharSet.range('A', 'Z').union(CharSet.single('a'))), "[A-Za]")
    assertEquals(EcmaScriptChars.showClass(CharSet.of(0x01, '-', 0x10ffff)), "[\\cA\\-\\u{10FFFF}]")
