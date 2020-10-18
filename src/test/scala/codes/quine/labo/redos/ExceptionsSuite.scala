package codes.quine.labo.redos

import minitest.SimpleTestSuite

object ExceptionsSuite extends SimpleTestSuite {
  test("RedosException#getMessage") {
    assertEquals(new RedosException("message").getMessage, "message")
  }

  test("UnsupportedException#getMessage") {
    assertEquals(new UnsupportedException("message").getMessage, "message")
  }

  test("InvalidRegExpException#getMessage") {
    assertEquals(new InvalidRegExpException("message").getMessage, "message")
  }
}
