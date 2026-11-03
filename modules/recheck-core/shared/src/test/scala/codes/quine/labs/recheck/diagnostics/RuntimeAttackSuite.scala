package codes.quine.labs.recheck
package diagnostics

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import scala.concurrent.duration.SECONDS

class RuntimeAttackSuite extends munit.FunSuite:

  test("RuntimeAttack#toString"):
    assertEquals(
      RuntimeAttack("python 3.15.0", Duration.Inf, RuntimeAttackResult.Timeout).toString,
      "confirmed (timed out on python 3.15.0, timeout: none)"
    )
    assertEquals(
      RuntimeAttack(
        "php 8.5.12",
        Duration(1, SECONDS),
        RuntimeAttackResult.EngineLimit("Backtrack limit exhausted")
      ).toString,
      "confirmed (engine limit: Backtrack limit exhausted on php 8.5.12, timeout: 1000 ms)"
    )
    assertEquals(
      RuntimeAttack("java 25", Duration(1, SECONDS), RuntimeAttackResult.EngineError("StackOverflowError")).toString,
      "refuted (engine error: StackOverflowError on java 25, timeout: 1000 ms)"
    )

  test("RuntimeAttack#isConfirmed"):
    assertEquals(RuntimeAttack("node v26", Duration(1, SECONDS), RuntimeAttackResult.Timeout).isConfirmed, true)
    assertEquals(
      RuntimeAttack(
        "node v26",
        Duration(1, SECONDS),
        RuntimeAttackResult.Finish(Duration(3, MILLISECONDS))
      ).isConfirmed,
      false
    )

  test("RuntimeAttackResult#toString"):
    assertEquals(RuntimeAttackResult.Finish(Duration(3, MILLISECONDS)).toString, "finished in 3 ms")
    assertEquals(RuntimeAttackResult.Timeout.toString, "timed out")
    assertEquals(RuntimeAttackResult.EngineLimit("foo").toString, "engine limit: foo")
    assertEquals(RuntimeAttackResult.EngineError("foo").toString, "engine error: foo")

  test("RuntimeAttackResult#isConfirmed"):
    assertEquals(RuntimeAttackResult.Finish(Duration(3, MILLISECONDS)).isConfirmed, false)
    assertEquals(RuntimeAttackResult.Timeout.isConfirmed, true)
    assertEquals(RuntimeAttackResult.EngineLimit("foo").isConfirmed, true)
    assertEquals(RuntimeAttackResult.EngineError("foo").isConfirmed, false)
