package codes.quine.labo.recheck.vm

class CounterRegSuite extends munit.FunSuite {
  test("CounterReg#toString") {
    assertEquals(CounterReg(1).toString, "%1")
  }
}
