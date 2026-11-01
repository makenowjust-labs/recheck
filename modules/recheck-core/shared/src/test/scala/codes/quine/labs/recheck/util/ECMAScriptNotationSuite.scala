package codes.quine.labs.recheck.util

import codes.quine.labs.resyntax.charset.CharSet

class ECMAScriptNotationSuite extends munit.FunSuite:
  test("ECMAScriptNotation.isPrintable"):
    assert(ECMAScriptNotation.isPrintable('a'))
    assert(ECMAScriptNotation.isPrintable(' '))
    assert(!ECMAScriptNotation.isPrintable(0))
    assert(!ECMAScriptNotation.isPrintable(0xffff))

  test("ECMAScriptNotation.showChar"):
    assertEquals(ECMAScriptNotation.showChar(0x09), "\\t")
    assertEquals(ECMAScriptNotation.showChar(0x0a), "\\n")
    assertEquals(ECMAScriptNotation.showChar(0x0b), "\\v")
    assertEquals(ECMAScriptNotation.showChar(0x0c), "\\f")
    assertEquals(ECMAScriptNotation.showChar(0x0d), "\\r")
    assertEquals(ECMAScriptNotation.showChar(0x5c), "\\\\")
    assertEquals(ECMAScriptNotation.showChar(0x01), "\\cA")
    assertEquals(ECMAScriptNotation.showChar('a'), "a")
    assertEquals(ECMAScriptNotation.showChar(0x1f363), "🍣")
    assertEquals(ECMAScriptNotation.showChar(0), "\\x00")
    assertEquals(ECMAScriptNotation.showChar(0xffff), "\\uFFFF")
    assertEquals(ECMAScriptNotation.showChar(0x10ffff), "\\u{10FFFF}")

  test("ECMAScriptNotation.showClass"):
    assertEquals(ECMAScriptNotation.showClass(CharSet.range('A', 'Z').union(CharSet.single('a'))), "[A-Za]")
    assertEquals(ECMAScriptNotation.showClass(CharSet.of(0x01, '-', 0x10ffff)), "[\\cA\\-\\u{10FFFF}]")
