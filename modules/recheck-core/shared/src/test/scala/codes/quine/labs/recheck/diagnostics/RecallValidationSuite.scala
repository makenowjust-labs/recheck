package codes.quine.labs.recheck
package diagnostics

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import scala.concurrent.duration.SECONDS

class RecallValidationSuite extends munit.FunSuite:

  test("RecallValidation#toString"):
    assertEquals(
      RecallValidation("python 3.15.0", Duration.Inf, RecallResult.Timeout).toString,
      "succeeded (timed out on python 3.15.0, timeout: none)"
    )
    assertEquals(
      RecallValidation(
        "php 8.5.12",
        Duration(1, SECONDS),
        RecallResult.EngineLimit("Backtrack limit exhausted")
      ).toString,
      "succeeded (engine limit: Backtrack limit exhausted on php 8.5.12, timeout: 1000 ms)"
    )
    assertEquals(
      RecallValidation("java 25", Duration(1, SECONDS), RecallResult.EngineError("StackOverflowError")).toString,
      "failed (engine error: StackOverflowError on java 25, timeout: 1000 ms)"
    )

  test("RecallValidation#isSucceeded"):
    assertEquals(RecallValidation("node v26", Duration(1, SECONDS), RecallResult.Timeout).isSucceeded, true)
    assertEquals(
      RecallValidation("node v26", Duration(1, SECONDS), RecallResult.Finish(Duration(3, MILLISECONDS))).isSucceeded,
      false
    )

  test("RecallResult#toString"):
    assertEquals(RecallResult.Finish(Duration(3, MILLISECONDS)).toString, "finished in 3 ms")
    assertEquals(RecallResult.Timeout.toString, "timed out")
    assertEquals(RecallResult.EngineLimit("foo").toString, "engine limit: foo")
    assertEquals(RecallResult.EngineError("foo").toString, "engine error: foo")

  test("RecallResult#isSucceeded"):
    assertEquals(RecallResult.Finish(Duration(3, MILLISECONDS)).isSucceeded, false)
    assertEquals(RecallResult.Timeout.isSucceeded, true)
    assertEquals(RecallResult.EngineLimit("foo").isSucceeded, true)
    assertEquals(RecallResult.EngineError("foo").isSucceeded, false)
