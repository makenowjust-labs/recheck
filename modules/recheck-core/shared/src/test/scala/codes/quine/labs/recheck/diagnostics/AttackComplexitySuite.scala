package codes.quine.labs.recheck.diagnostics

class AttackComplexitySuite extends munit.FunSuite {
  test("AttackComplexity#toString") {
    assertEquals(AttackComplexity.Safe(false).toString, "safe")
    assertEquals(AttackComplexity.Safe(true).toString, "safe (fuzz)")
    assertEquals(AttackComplexity.Constant.toString, "constant")
    assertEquals(AttackComplexity.Linear.toString, "linear")
    assertEquals(AttackComplexity.Polynomial(2, false).toString, "2nd degree polynomial")
    assertEquals(AttackComplexity.Polynomial(2, true).toString, "2nd degree polynomial (fuzz)")
    assertEquals(AttackComplexity.Exponential(false).toString, "exponential")
    assertEquals(AttackComplexity.Exponential(true).toString, "exponential (fuzz)")
  }
}
