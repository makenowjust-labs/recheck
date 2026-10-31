package codes.quine.labs.recheck.regexp

import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode

class EcmaScriptCharsSuite extends munit.FunSuite:
  test("EcmaScriptChars.mode"):
    assertEquals(EcmaScriptChars.mode(false), UnitMode.Utf16)
    assertEquals(EcmaScriptChars.mode(true), UnitMode.CodePoint)

  test("EcmaScriptChars.any"):
    assertEquals(EcmaScriptChars.any(false), CharSet.range(0, 0xffff))
    assertEquals(EcmaScriptChars.any(true), CharSet.range(0, 0x10ffff))

  test("EcmaScriptChars.Digit"):
    assert(EcmaScriptChars.Digit.contains('0'))
    assert(!EcmaScriptChars.Digit.contains('a'))

  test("EcmaScriptChars.Space"):
    assert(EcmaScriptChars.Space.contains('\n'))
    assert(EcmaScriptChars.Space.contains(' '))
    assert(EcmaScriptChars.Space.contains(0x3000))
    assert(!EcmaScriptChars.Space.contains('a'))

  test("EcmaScriptChars.LineTerminator"):
    assertEquals(EcmaScriptChars.LineTerminator, CharSet.of('\n', '\r', 0x2028, 0x2029))

  test("EcmaScriptChars.Word"):
    for c <- Seq('a', 'z', 'A', 'Z', '0', '9', '_') do assert(EcmaScriptChars.Word.contains(c))
    assert(!EcmaScriptChars.Word.contains('!'))

  test("EcmaScriptChars.dot"):
    val any16 = EcmaScriptChars.any(false)
    val any = EcmaScriptChars.any(true)
    val lineTerminator = EcmaScriptChars.LineTerminator
    assertEquals(EcmaScriptChars.dot(false, true, false), any16)
    assertEquals(EcmaScriptChars.dot(false, true, true), any)
    assertEquals(EcmaScriptChars.dot(false, false, false), any16.diff(lineTerminator))
    assertEquals(EcmaScriptChars.dot(false, false, true), any.diff(lineTerminator))
    assertEquals(EcmaScriptChars.dot(true, true, false), EcmaScriptChars.canonicalize(any16, false))
    assertEquals(EcmaScriptChars.dot(true, false, true), EcmaScriptChars.canonicalize(any.diff(lineTerminator), true))

  test("EcmaScriptChars.canonicalize: character"):
    assertEquals(EcmaScriptChars.canonicalize('a', false), 'A'.toInt)
    assertEquals(EcmaScriptChars.canonicalize('A', false), 'A'.toInt)
    assertEquals(EcmaScriptChars.canonicalize('a', true), 'a'.toInt)
    assertEquals(EcmaScriptChars.canonicalize('A', true), 'a'.toInt)

    // U+FF21: FULLWIDTH LATIN CAPITAL LETTER A
    // U+FF41: FULLWIDTH LATIN SMALL LETTER A
    assertEquals(EcmaScriptChars.canonicalize(0xff41, false), 0xff21)
    assertEquals(EcmaScriptChars.canonicalize(0xff21, true), 0xff41)

    // U+017F: LATIN SMALL LETTER LONG S
    assertEquals(EcmaScriptChars.canonicalize(0x017f, false), 0x017f)
    assertEquals(EcmaScriptChars.canonicalize(0x017f, true), 's'.toInt)

  test("EcmaScriptChars.canonicalize: set"):
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single('n'), false), CharSet.single('N'))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single('N'), true), CharSet.single('n'))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single(0x017f), false), CharSet.single(0x017f))
    assertEquals(EcmaScriptChars.canonicalize(CharSet.single(0x017f), true), CharSet.single('s'))

  test("EcmaScriptChars.unicodeProperty"):
    assertEquals(EcmaScriptChars.unicodeProperty("invalid"), None)
    assert(EcmaScriptChars.unicodeProperty("ASCII").get.contains('A'))
    assert(!EcmaScriptChars.unicodeProperty("ASCII").get.contains(0x80))
    assert(EcmaScriptChars.unicodeProperty("AHex").get.contains('a'))
    assert(!EcmaScriptChars.unicodeProperty("AHex").get.contains('z'))
    assert(EcmaScriptChars.unicodeProperty("Any").get.contains(0x10ffff))
    assert(!EcmaScriptChars.unicodeProperty("Assigned").get.contains(0x10ffff))
    assert(EcmaScriptChars.unicodeProperty("Letter").get.contains('A'))
    assert(EcmaScriptChars.unicodeProperty("L").get.contains('A'))
    assert(EcmaScriptChars.unicodeProperty("Z").get.contains(' '))
    assert(!EcmaScriptChars.unicodeProperty("Z").get.contains('+'))
    // Binary properties known only to ICU are not ECMAScript properties.
    assertEquals(EcmaScriptChars.unicodeProperty("alnum"), None)

  test("EcmaScriptChars.unicodePropertyValue"):
    assertEquals(EcmaScriptChars.unicodePropertyValue("invalid", "invalid"), None)
    assertEquals(EcmaScriptChars.unicodePropertyValue("Block", "Basic_Latin"), None)
    assertEquals(EcmaScriptChars.unicodePropertyValue("gc", "invalid"), None)
    assert(EcmaScriptChars.unicodePropertyValue("General_Category", "N").get.contains('0'))
    assert(!EcmaScriptChars.unicodePropertyValue("gc", "Number").get.contains('a'))
    assert(EcmaScriptChars.unicodePropertyValue("Script", "Hira").get.contains('あ'))
    // U+30FC: KATAKANA-HIRAGANA PROLONGED SOUND MARK
    assert(!EcmaScriptChars.unicodePropertyValue("sc", "Hira").get.contains(0x30fc))
    assert(EcmaScriptChars.unicodePropertyValue("Script_Extensions", "Hira").get.contains(0x30fc))
    // Script codes known only to ICU are not Unicode scripts.
    assertEquals(EcmaScriptChars.unicodePropertyValue("sc", "Zsym"), None)

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
