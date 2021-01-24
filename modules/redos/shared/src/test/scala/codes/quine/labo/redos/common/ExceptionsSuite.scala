package codes.quine.labo.redos.common

class ExceptionsSuite extends munit.FunSuite {
  test("TimeoutException#getMessage") {
    assertEquals(new TimeoutException("message").getMessage, "message")
  }

  test("UnsupportedException#getMessage") {
    assertEquals(new UnsupportedException("message").getMessage, "message")
  }

  test("InvalidRegExpException#getMessage") {
    assertEquals(new InvalidRegExpException("message").getMessage, "message")
  }
}
