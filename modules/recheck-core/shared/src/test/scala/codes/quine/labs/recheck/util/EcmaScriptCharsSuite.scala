package codes.quine.labs.recheck.util

import codes.quine.labs.resyntax.charset.CharSet

class EcmaScriptCharsSuite extends munit.FunSuite:
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
