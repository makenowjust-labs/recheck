package codes.quine.labo.redos
package backtrack

import util.Timeout
import Tracer.LimitException

class TracerSuite extends munit.FunSuite {
  test("Tracer.LimitTracer") {
    val ir = IR(
      2,
      Map("foo" -> 2),
      IndexedSeq(
        IR.ForkCont(2),
        IR.Any,
        IR.Jump(-3),
        IR.Done
      )
    )
    val tracer = new Tracer.LimitTracer(ir, 2)
    assertEquals(tracer.limit, 2)
    tracer.trace(0, 0, false, _ => None, Seq.empty)
    assertEquals(tracer.steps, 0)
    tracer.trace(0, 1, false, _ => None, Seq.empty)
    assertEquals(tracer.steps, 1)
    interceptMessage[LimitException]("limit is exceeded")(tracer.trace(0, 1, false, _ => None, Seq.empty))
  }

  test("Tracer.NoTracer") {
    val tracer = Tracer.NoTracer()
    assertEquals(tracer.timeout, Timeout.NoTimeout)
    tracer.trace(0, 0, false, _ => None, Seq.empty)
  }
}
