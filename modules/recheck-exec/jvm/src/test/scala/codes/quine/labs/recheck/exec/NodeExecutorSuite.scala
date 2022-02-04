package codes.quine.labs.recheck.exec

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.MILLISECONDS

import codes.quine.labs.recheck.common.Context

class NodeExecutorSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("NodeExecutor.exec") {
    assertEquals(
      NodeExecutor.exec("console.log('foo');", None),
      Some((0, "foo", ""))
    )
    assertEquals(
      NodeExecutor.exec("process.exit(1)", None),
      Some((1, "", ""))
    )
    assertEquals(
      NodeExecutor.exec("setTimeout(() => {}, 1000);", Some(FiniteDuration(100, MILLISECONDS))),
      None
    )
  }
}
