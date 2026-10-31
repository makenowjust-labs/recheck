package codes.quine.labs.recheck.exec

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.CancelException
import codes.quine.labs.recheck.common.CancellationTokenSource
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException

class ExecutorSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val node: Command = Command("node", Seq("-"))

  test("Executor.exec"):
    assertEquals(Executor.exec(node, "console.log('foo');", None), ExecResult.Exit(0, "foo\n", ""))
    assertEquals(Executor.exec(node, "console.error('bar'); process.exit(1)", None), ExecResult.Exit(1, "", "bar\n"))
    assertEquals(
      Executor.exec(node, "console.log('é');", None),
      ExecResult.Exit(0, "é\n", "")
    )
    assertEquals(
      Executor.exec(Command("node", Seq("-e", "console.log(process.env.FOO)"), Map("FOO" -> "foo")), "", None),
      ExecResult.Exit(0, "foo\n", "")
    )

  test("Executor.exec: timeout"):
    assertEquals(
      Executor.exec(node, "console.log('foo'); setTimeout(() => {}, 5000);", Some(100.millis)),
      ExecResult.Timeout("foo\n")
    )

  test("Executor.exec: marker"):
    // The startup before the marker does not count toward the timeout.
    val script = "setTimeout(() => { console.log('start'); console.log('end'); }, 500);"
    assertEquals(Executor.exec(node, script, Some(200.millis), Some("start")), ExecResult.Exit(0, "start\nend\n", ""))
    assertEquals(
      Executor.exec(node, "console.log('start'); setTimeout(() => {}, 5000);", Some(200.millis), Some("start")),
      ExecResult.Timeout("start\n")
    )
    // A process exiting without the marker is just finished.
    assertEquals(
      Executor.exec(node, "console.log('foo');", Some(200.millis), Some("start")),
      ExecResult.Exit(0, "foo\n", "")
    )

  test("Executor.exec: interruption"):
    intercept[TimeoutException]:
      Executor.exec(node, "setTimeout(() => {}, 5000);", None, Some("start"))(using Context(timeout = 200.millis))
    val source = new CancellationTokenSource
    source.cancel()
    intercept[CancelException]:
      Executor.exec(node, "setTimeout(() => {}, 5000);", None)(using Context(token = Some(source.token)))

  test("Executor.exec: missing command"):
    intercept[UnexpectedException]:
      Executor.exec(Command("recheck-missing-command"), "", None)
