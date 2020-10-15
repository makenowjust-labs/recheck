package codes.quine.labo.redos.data

import minitest.SimpleTestSuite

object UCharSuite extends SimpleTestSuite {
  test("UChar#compare") {
    assertEquals(UChar(0x41).compare(UChar(0x41)), 0)
    assertEquals(UChar(0x41).compare(UChar(0x42)), -1)
    assertEquals(UChar(0x42).compare(UChar(0x41)), 1)
  }

  test("UChar#toChars") {
    assertEquals(String.valueOf(UChar(0x41).toChars), "A")
    assertEquals(String.valueOf(UChar(0x1f363).toChars), "🍣")
  }

  test("UChar#toString") {
    assertEquals(UChar(0x09).toString, "\\t")
    assertEquals(UChar(0x0a).toString, "\\n")
    assertEquals(UChar(0x0b).toString, "\\v")
    assertEquals(UChar(0x0c).toString, "\\f")
    assertEquals(UChar(0x0d).toString, "\\r")
    assertEquals(UChar('a').toString, "a")
    assertEquals(UChar(0).toString, "\\x00")
    assertEquals(UChar(0xffff).toString, "\\uFFFF")
    assertEquals(UChar(0x10ffff).toString, "\\u{10FFFF}")
  }
}
