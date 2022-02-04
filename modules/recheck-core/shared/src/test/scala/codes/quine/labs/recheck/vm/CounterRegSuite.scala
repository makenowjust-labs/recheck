package codes.quine.labs.recheck.vm

class CounterRegSuite extends munit.FunSuite {
  test("CounterReg#toString") {
    assertEquals(CounterReg(1).toString, "%1")
  }
}
