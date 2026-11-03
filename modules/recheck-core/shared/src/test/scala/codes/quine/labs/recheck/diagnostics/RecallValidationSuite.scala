package codes.quine.labs.recheck
package diagnostics

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import scala.concurrent.duration.SECONDS

class RecallValidationSuite extends munit.FunSuite:

  test("RecallValidation#toString"):
    assertEquals(
      RecallValidation("python 3.15.0", Duration.Inf, RecallResult.Timeout).toString,
      "confirmed (timed out on python 3.15.0, timeout: none)"
    )
    assertEquals(
      RecallValidation(
        "php 8.5.12",
        Duration(1, SECONDS),
        RecallResult.EngineLimit("Backtrack limit exhausted")
      ).toString,
      "confirmed (engine limit: Backtrack limit exhausted on php 8.5.12, timeout: 1000 ms)"
    )
    assertEquals(
      RecallValidation("java 25", Duration(1, SECONDS), RecallResult.EngineError("StackOverflowError")).toString,
      "refuted (engine error: StackOverflowError on java 25, timeout: 1000 ms)"
    )

  test("RecallValidation#isConfirmed"):
    assertEquals(RecallValidation("node v26", Duration(1, SECONDS), RecallResult.Timeout).isConfirmed, true)
    assertEquals(
      RecallValidation("node v26", Duration(1, SECONDS), RecallResult.Finish(Duration(3, MILLISECONDS))).isConfirmed,
      false
    )

  test("RecallResult#toString"):
    assertEquals(RecallResult.Finish(Duration(3, MILLISECONDS)).toString, "finished in 3 ms")
    assertEquals(RecallResult.Timeout.toString, "timed out")
    assertEquals(RecallResult.EngineLimit("foo").toString, "engine limit: foo")
    assertEquals(RecallResult.EngineError("foo").toString, "engine error: foo")

  test("RecallResult#isConfirmed"):
    assertEquals(RecallResult.Finish(Duration(3, MILLISECONDS)).isConfirmed, false)
    assertEquals(RecallResult.Timeout.isConfirmed, true)
    assertEquals(RecallResult.EngineLimit("foo").isConfirmed, true)
    assertEquals(RecallResult.EngineError("foo").isConfirmed, false)
