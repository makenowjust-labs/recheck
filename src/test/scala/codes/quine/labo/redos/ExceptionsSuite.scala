package codes.quine.labo.redos

class ExceptionsSuite extends munit.FunSuite {
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
