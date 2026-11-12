package codes.quine.labs.recheck.fuzz

class GrowthSuite extends munit.FunSuite:

  def steps(counts: Range)(f: Long => Long): IndexedSeq[Long] = counts.map(n => f(n.toLong))

  test("Growth.of: polynomial"):
    assertEquals(Growth.of(1, 1, steps(1 to 3)(_ => 5), 4), Some(Growth.Polynomial(1, 1, Vector(5))))
    assertEquals(Growth.of(1, 1, steps(1 to 4)(n => 3 * n + 1), 4), Some(Growth.Polynomial(1, 1, Vector(4, 3))))
    assertEquals(Growth.of(1, 1, steps(1 to 5)(n => n * n), 4), Some(Growth.Polynomial(1, 1, Vector(1, 3, 2))))
    // The steps of the lower degrees do not hide the degree.
    val quartic = steps(3 to 9)(n => n * n * n * n + 1000 * n * n * n + 7)
    assertEquals(Growth.of(3, 1, quartic, 4).map(_.isSuperLinear), Some(true))
    assertEquals(Growth.of(3, 1, quartic, 4).collect { case p: Growth.Polynomial => p.degree }, Some(4))
    assertEquals(Growth.of(2, 3, steps(2 to 14 by 3)(n => n * n), 4), Some(Growth.Polynomial(2, 3, Vector(4, 21, 18))))

  test("Growth.of: too few steps"):
    assertEquals(Growth.of(1, 1, steps(1 to 2)(_ => 5), 4), None)
    assertEquals(Growth.of(1, 1, steps(1 to 3)(n => 3 * n + 1), 4), None)
    assertEquals(Growth.of(1, 1, steps(1 to 4)(n => n * n), 4), None)
    // The steps of a degree above `maxDegree` are not a polynomial up to it.
    assertEquals(Growth.of(1, 1, steps(1 to 6)(n => n * n * n * n * n), 4), None)

  test("Growth.of: exponential"):
    assertEquals(Growth.of(1, 1, steps(1 to 7)(n => 1L << n), 4), Some(Growth.Exponential))
    assertEquals(Growth.of(1, 1, steps(1 to 7)(n => n * n * n * n * n), 4), Some(Growth.Exponential))
    assertEquals(Growth.of(1, 1, steps(1 to 5)(n => 1L << n), 2), Some(Growth.Exponential))

  test("Growth.of: irregular"):
    // The steps of the period 2 are polynomial only by the stride 2.
    val periodic = steps(1 to 14)(n => n * n + (if n % 2 == 0 then 10 else 0))
    assertEquals(Growth.of(1, 1, periodic.take(7), 4), None)
    val odds = periodic.indices.collect { case i if i % 2 == 0 => periodic(i) }
    assertEquals(Growth.of(1, 2, odds, 4), Some(Growth.Polynomial(1, 2, Vector(1, 8, 8))))

  test("Growth.Polynomial#stepsOn"):
    val square = Growth.Polynomial(1, 1, Vector(1, 3, 2))
    assertEquals(square.stepsOn(1), BigInt(1))
    assertEquals(square.stepsOn(100000), BigInt(100000L * 100000L))
    val strided = Growth.Polynomial(2, 3, Vector(4, 21, 18))
    assertEquals(strided.stepsOn(2), BigInt(4))
    assertEquals(strided.stepsOn(302), BigInt(302 * 302))

  test("Growth#isSuperLinear"):
    assert(!Growth.Polynomial(1, 1, Vector(5)).isSuperLinear)
    assert(!Growth.Polynomial(1, 1, Vector(4, 3)).isSuperLinear)
    assert(Growth.Polynomial(1, 1, Vector(1, 3, 2)).isSuperLinear)
    assert(Growth.Exponential.isSuperLinear)
