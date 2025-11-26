package codes.quine.labs.recheck.util

class RepeatUtilSuite extends munit.FunSuite:
  test("RepeatUtil.polynomial"):
    assertEquals(RepeatUtil.polynomial(2, 25000, 1, 1, 4000), 224)
    assertEquals(RepeatUtil.polynomial(2, 25000, 1, 1, 200), 199)

  test("RepeatUtil.exponential"):
    assertEquals(RepeatUtil.exponential(25000, 1, 30, 4000), 10)
    assertEquals(RepeatUtil.exponential(25000, 1, 30, 200), 6)
