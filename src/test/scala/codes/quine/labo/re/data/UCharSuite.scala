package codes.quine.labo.re.data

import minitest.SimpleTestSuite

object UCharSuite extends SimpleTestSuite {
  test("UChar#compare") {
    assertEquals(UChar(0x41).compare(UChar(0x41)), 0)
    assertEquals(UChar(0x41).compare(UChar(0x42)), -1)
    assertEquals(UChar(0x42).compare(UChar(0x41)), 1)
  }

  test("UChar#toString") {
    assertEquals(UChar(0x41).toString, "A")
    assertEquals(UChar(0x1f363).toString, "🍣")
  }
}
