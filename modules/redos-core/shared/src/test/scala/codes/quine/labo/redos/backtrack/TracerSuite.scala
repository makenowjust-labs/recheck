package codes.quine.labo.redos
package backtrack

import util.Timeout

class TracerSuite extends munit.FunSuite {
  test("Tracer.Result#coverage") {
    val result = Tracer.Result(0, Map(0 -> Set((0, false)), 1 -> Set((1, true))))
    assertEquals(result.coverage, Set((0, false), (1, true)))
  }

  test("Tracer.LimitTracer") {
    val tracer = new Tracer.LimitTracer(2)
    assertEquals(tracer.limit, 2)
    tracer.trace(0, 0, false)
    assertEquals(tracer.result, Tracer.Result(1, Map(0 -> Set((0, false)))))
    interceptMessage[LimitException]("limit is exceeded")(tracer.trace(0, 1, false))
  }

  test("Tracer.NoTracer") {
    val tracer = Tracer.NoTracer()
    assertEquals(tracer.timeout, Timeout.NoTimeout)
    tracer.trace(0, 0, false)
    intercept[UnsupportedOperationException](tracer.result)
  }
}
