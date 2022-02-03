package codes.quine.labs.recheck.common

class ExceptionsSuite extends munit.FunSuite {
  test("TimeoutException#getMessage") {
    assertEquals(new TimeoutException("source").getMessage, "timeout at source")
  }

  test("CancelException#getMessage") {
    assertEquals(new CancelException("source").getMessage, "cancel at source")
  }

  test("UnsupportedException#getMessage") {
    assertEquals(new UnsupportedException("message").getMessage, "message")
  }

  test("InvalidRegExpException#getMessage") {
    assertEquals(new InvalidRegExpException("message").getMessage, "message")
  }
}
