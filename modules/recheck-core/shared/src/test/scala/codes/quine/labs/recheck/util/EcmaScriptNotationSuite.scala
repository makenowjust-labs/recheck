package codes.quine.labs.recheck.util

import codes.quine.labs.resyntax.charset.CharSet

class EcmaScriptNotationSuite extends munit.FunSuite:
  test("EcmaScriptNotation.isPrintable"):
    assert(EcmaScriptNotation.isPrintable('a'))
    assert(EcmaScriptNotation.isPrintable(' '))
    assert(!EcmaScriptNotation.isPrintable(0))
    assert(!EcmaScriptNotation.isPrintable(0xffff))

  test("EcmaScriptNotation.showChar"):
    assertEquals(EcmaScriptNotation.showChar(0x09), "\\t")
    assertEquals(EcmaScriptNotation.showChar(0x0a), "\\n")
    assertEquals(EcmaScriptNotation.showChar(0x0b), "\\v")
    assertEquals(EcmaScriptNotation.showChar(0x0c), "\\f")
    assertEquals(EcmaScriptNotation.showChar(0x0d), "\\r")
    assertEquals(EcmaScriptNotation.showChar(0x5c), "\\\\")
    assertEquals(EcmaScriptNotation.showChar(0x01), "\\cA")
    assertEquals(EcmaScriptNotation.showChar('a'), "a")
    assertEquals(EcmaScriptNotation.showChar(0x1f363), "🍣")
    assertEquals(EcmaScriptNotation.showChar(0), "\\x00")
    assertEquals(EcmaScriptNotation.showChar(0xffff), "\\uFFFF")
    assertEquals(EcmaScriptNotation.showChar(0x10ffff), "\\u{10FFFF}")

  test("EcmaScriptNotation.showClass"):
    assertEquals(EcmaScriptNotation.showClass(CharSet.range('A', 'Z').union(CharSet.single('a'))), "[A-Za]")
    assertEquals(EcmaScriptNotation.showClass(CharSet.of(0x01, '-', 0x10ffff)), "[\\cA\\-\\u{10FFFF}]")
