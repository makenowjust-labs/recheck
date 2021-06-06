package codes.quine.labo.recheck.vm

class CanaryRegSuite extends munit.FunSuite {
  test("CanaryReg#toString") {
    assertEquals(CanaryReg(1).toString, "%%1")
  }
}
