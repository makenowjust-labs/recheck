package codes.quine.labs.recheck.diagnostics

class RepeatCountSuite extends munit.FunSuite:

  test("RepeatCount.of"):
    assertEquals(RepeatCount.of(AttackComplexity.Polynomial(2, false), 1, 1, limit = 25000, maxSize = 4000), 224)
    assertEquals(RepeatCount.of(AttackComplexity.Exponential(false), 1, 30, limit = 25000, maxSize = 4000), 10)

  test("RepeatCount.polynomial"):
    assertEquals(RepeatCount.polynomial(2, 1, 1, limit = 25000, maxSize = 4000), 224)
    assertEquals(RepeatCount.polynomial(2, 1, 1, limit = 25000, maxSize = 200), 199)

  test("RepeatCount.exponential"):
    assertEquals(RepeatCount.exponential(1, 30, limit = 25000, maxSize = 4000), 10)
    assertEquals(RepeatCount.exponential(1, 30, limit = 25000, maxSize = 200), 6)
