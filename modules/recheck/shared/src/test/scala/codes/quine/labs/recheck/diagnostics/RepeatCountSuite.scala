package codes.quine.labs.recheck.diagnostics

class RepeatCountSuite extends munit.FunSuite:

  test("RepeatCount.of"):
    assertEquals(RepeatCount.of(AttackComplexity.Polynomial(2, false), 25000, 1, 1, 4000), 224)
    assertEquals(RepeatCount.of(AttackComplexity.Exponential(false), 25000, 1, 30, 4000), 10)

  test("RepeatCount.polynomial"):
    assertEquals(RepeatCount.polynomial(2, 25000, 1, 1, 4000), 224)
    assertEquals(RepeatCount.polynomial(2, 25000, 1, 1, 200), 199)

  test("RepeatCount.exponential"):
    assertEquals(RepeatCount.exponential(25000, 1, 30, 4000), 10)
    assertEquals(RepeatCount.exponential(25000, 1, 30, 200), 6)
