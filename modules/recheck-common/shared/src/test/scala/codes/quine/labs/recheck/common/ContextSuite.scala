package codes.quine.labs.recheck.common

import scala.concurrent.duration.*

class ContextSuite extends munit.FunSuite:

  test("Context.apply"):
    assertEquals(Context().isInterrupted(), false)
    assertEquals(Context(timeout = 1.second).isInterrupted(), false)
    assertEquals(Context(timeout = (-1).second).isInterrupted(), true)
    assertEquals(Context(timeout = Duration.Inf).isInterrupted(), false)
    assertEquals(Context(timeout = Duration.MinusInf).isInterrupted(), true)

    val source = new CancellationTokenSource
    val ctx = Context(token = Some(source.token))
    assertEquals(ctx.isInterrupted(), false)
    source.cancel()
    assertEquals(ctx.isInterrupted(), true)

  test("Context#interrupt"):
    val source = new CancellationTokenSource
    val ctx = Context(token = Some(source.token))

    // Case: normal
    var called = false
    val result = ctx.interrupt:
      called = true
      42
    assertEquals(result, 42)
    assertEquals(called, true)

    // Case: cancelled
    source.cancel()
    called = false
    interceptMessage[CancelException](
      "cancel at modules/recheck-common/shared/src/test/scala/codes/quine/labs/recheck/common/ContextSuite.scala:38"
    ):
      ctx.interrupt:
        called = true
        42
    assertEquals(called, false)

    // Case: timeout
    called = false
    interceptMessage[TimeoutException](
      "timeout at modules/recheck-common/shared/src/test/scala/codes/quine/labs/recheck/common/ContextSuite.scala:48"
    ):
      Context(timeout = Duration.MinusInf).interrupt:
        called = true
        42
    assertEquals(called, false)

  /** Waits for the duration without sleeping, which Scala.js cannot do. */
  def spin(duration: FiniteDuration): Unit =
    val end = duration.fromNow
    while end.hasTimeLeft() do ()

  test("Context#pauseTimeout"):
    val ctx = Context(timeout = 50.millis)
    val deadline = ctx.deadline
    val result = ctx.pauseTimeout:
      spin(100.millis)
      // The deadline has passed, but the paused timeout does not interrupt.
      assertEquals(ctx.isInterrupted(), false)
      ctx.interrupt(42)
    assertEquals(result, 42)
    assert(clue(ctx.deadline - deadline) >= 100.millis)
    // A context without a timeout runs the body.
    assertEquals(Context().pauseTimeout(42), 42)
    // An overdue context does not start the body.
    val overdue = Context(timeout = 1.millis)
    spin(10.millis)
    intercept[TimeoutException](overdue.pauseTimeout(fail("the body runs")))

  test("Context#pauseTimeout: nested and repeated bodies"):
    val ctx = Context(timeout = 1.hour)
    val deadline = ctx.deadline
    ctx.pauseTimeout:
      spin(50.millis)
      ctx.pauseTimeout(spin(50.millis))
      // The outer body moves the deadline once, including the time of the inner one.
      assertEquals(ctx.deadline, deadline)
      spin(50.millis)
    val moved = ctx.deadline - deadline
    assert(clue(moved) >= 150.millis)
    // A body throwing an exception moves the deadline too.
    intercept[IllegalStateException]:
      ctx.pauseTimeout:
        spin(50.millis)
        throw new IllegalStateException
    assert(clue(ctx.deadline - deadline) >= moved + 50.millis)

  test("Context#pauseTimeout: cancellation"):
    val source = new CancellationTokenSource
    val ctx = Context(timeout = 1.hour, token = Some(source.token))
    intercept[CancelException]:
      ctx.pauseTimeout:
        source.cancel()
        ctx.interrupt(42)

  test("Context#log"):
    var called: Boolean = false
    var args: Option[String] = None
    val logger: Context.Logger = (message) => args = Some(message)
    val ctx1 = Context()
    val ctx2 = Context(logger = Some(logger))

    ctx1.log:
      called = true
      "foo"
    assertEquals(called, false)
    assertEquals(args, None)

    ctx2.log:
      called = true
      "foo"
    assertEquals(called, true)
    assertEquals(args, Some("foo"))
