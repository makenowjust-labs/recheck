package codes.quine.labo.recheck.common

import scala.concurrent.duration._

class ContextSuite extends munit.FunSuite {
  test("Context.apply") {
    assertEquals(Context().isInterrupted(), false)
    assertEquals(Context(timeout = 1.second).isInterrupted(), false)
    assertEquals(Context(timeout = -1.second).isInterrupted(), true)
    assertEquals(Context(timeout = Duration.Inf).isInterrupted(), false)
    assertEquals(Context(timeout = Duration.MinusInf).isInterrupted(), true)
  }

  test("Context.cancellable") {
    val (ctx, cancel) = Context.cancellable()
    assertEquals(ctx.isInterrupted(), false)
    cancel()
    assertEquals(ctx.isInterrupted(), true)
  }

  test("Context#interrupt") {
    val (ctx, cancel) = Context.cancellable()
    assertEquals(ctx.interrupt(42), 42)
    cancel()
    intercept[CancelException](ctx.interrupt(42))
    intercept[TimeoutException](Context(timeout = -1.second).interrupt(42))
  }
}
