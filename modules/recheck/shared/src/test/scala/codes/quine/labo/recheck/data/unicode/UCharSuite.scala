package codes.quine.labo.recheck.data.unicode

class UCharSuite extends munit.FunSuite {
  test("UChar.canonicalize") {
    assertEquals(UChar.canonicalize('a', false), UChar('A'))
    assertEquals(UChar.canonicalize('A', false), UChar('A'))
    assertEquals(UChar.canonicalize('a', true), UChar('a'))
    assertEquals(UChar.canonicalize('A', true), UChar('a'))

    // U+FF21: FULLWIDTH LATIN CAPITAL LETTER A
    // U+FF41: FULLWIDTH LATIN SMALL LETTER A
    assertEquals(UChar.canonicalize(UChar('\uff41'), false), UChar('\uff21'))
    assertEquals(UChar.canonicalize(UChar('\uff21'), true), UChar('\uff41'))

    // U+017F: LATIN SMALL LETTER LONG S
    assertEquals(UChar.canonicalize(UChar('\u017f'), false), UChar('\u017f'))
    assertEquals(UChar.canonicalize(UChar('\u017f'), true), UChar('s'))
  }

  test("UChar#isValidCodePoint") {
    assert(UChar(0x41).isValidCodePoint)
    assert(UChar(0x0).isValidCodePoint)
    assert(UChar(0x10ffff).isValidCodePoint)
    assert(!UChar(0x110000).isValidCodePoint)
  }

  test("UChar#compare") {
    assertEquals(UChar(0x41).compare(UChar(0x41)), 0)
    assertEquals(UChar(0x41).compare(UChar(0x42)), -1)
    assertEquals(UChar(0x42).compare(UChar(0x41)), 1)
  }

  test("UChar#hashCode") {
    assertEquals(UChar(0x41).hashCode(), 0x41)
  }

  test("UChar#toChars") {
    assertEquals(String.valueOf(UChar(0x41).toChars), "A")
    assertEquals(String.valueOf(UChar(0x1f363).toChars), "🍣")
  }

  test("UChar#asString") {
    assertEquals(UChar(0x41).asString, "A")
    assertEquals(UChar(0x1f363).asString, "🍣")
  }

  test("UChar#toString") {
    assertEquals(UChar(0x09).toString, "\\t")
    assertEquals(UChar(0x0a).toString, "\\n")
    assertEquals(UChar(0x0b).toString, "\\v")
    assertEquals(UChar(0x0c).toString, "\\f")
    assertEquals(UChar(0x0d).toString, "\\r")
    assertEquals(UChar(0x01).toString, "\\cA")
    assertEquals(UChar('a').toString, "a")
    assertEquals(UChar(0).toString, "\\x00")
    assertEquals(UChar(0xffff).toString, "\\uFFFF")
    assertEquals(UChar(0x10ffff).toString, "\\u{10FFFF}")
  }
}
