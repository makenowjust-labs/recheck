package codes.quine.labo.recheck.common

class CancellationsSuite extends munit.FunSuite {
  test("CancellationTokenSource#cancel") {
    val source = new CancellationTokenSource
    assertEquals(source.token.isCancelled(), false)
    source.cancel()
    assertEquals(source.token.isCancelled(), true)
  }

  test("CancellationToken.cancelled") {
    assertEquals(CancellationToken.cancelled.isCancelled(), true)
  }
}
