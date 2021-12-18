package codes.quine.labo.recheck.common

import scala.concurrent.duration._

class ContextSuite extends munit.FunSuite {
  test("Context.apply") {
    assertEquals(Context().isInterrupted(), false)
    assertEquals(Context(timeout = 1.second).isInterrupted(), false)
    assertEquals(Context(timeout = -1.second).isInterrupted(), true)
    assertEquals(Context(timeout = Duration.Inf).isInterrupted(), false)
    assertEquals(Context(timeout = Duration.MinusInf).isInterrupted(), true)

    val source = new CancellationTokenSource
    val ctx = Context(token = Some(source.token))
    assertEquals(ctx.isInterrupted(), false)
    source.cancel()
    assertEquals(ctx.isInterrupted(), true)
  }

  test("Context#interrupt") {
    val source = new CancellationTokenSource
    val ctx = Context(token = Some(source.token))
    assertEquals(ctx.interrupt(42), 42)
    source.cancel()
    interceptMessage[CancelException](
      "cancel at modules/recheck-common/shared/src/test/scala/codes/quine/labo/recheck/common/ContextSuite.scala:27"
    )(ctx.interrupt(42))
    interceptMessage[TimeoutException](
      "timeout at modules/recheck-common/shared/src/test/scala/codes/quine/labo/recheck/common/ContextSuite.scala:30"
    )(Context(timeout = -1.second).interrupt(42))
  }

  test("Context#log") {
    var called: Boolean = false
    var args: Option[String] = None
    val logger: Context.Logger = (message) => {
      args = Some(message)
    }
    val ctx1 = Context()
    val ctx2 = Context(logger = Some(logger))

    ctx1.log {
      called = true
      "foo"
    }
    assertEquals(called, false)
    assertEquals(args, None)

    ctx2.log {
      called = true
      "foo"
    }
    assertEquals(called, true)
    assertEquals(args, Some("foo"))
  }
}
