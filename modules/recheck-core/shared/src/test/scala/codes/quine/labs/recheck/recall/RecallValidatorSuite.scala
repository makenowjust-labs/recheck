package codes.quine.labs.recheck.recall

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class RecallValidatorSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val attack: AttackPattern =
    AttackPattern(Seq((UString.fromString("x"), UString.fromString("y"), 0)), UString.fromString("z"), 2)

  test("RecallValidator.validate"):
    assertEquals(
      RecallValidator.validate("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, Duration.MinusInf),
      None
    )
    assertEquals(RecallValidator.validate("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, Duration.Zero), None)
    // A Perl pattern interpolating a variable is not run.
    assertEquals(RecallValidator.validate("a$x", "", Dialect.Perl(), UnitMode.CodePoint, attack, 1.second), None)

  test("RecallValidator.limit"):
    assertEquals(RecallValidator.limit(Duration.Inf), None)
    assertEquals(RecallValidator.limit(100.seconds), Some(100.seconds))
    val ctx50s = Context(timeout = 50.seconds)
    assert(RecallValidator.limit(100.seconds)(using ctx50s).exists(_ <= 50.seconds))
    assert(RecallValidator.limit(Duration.Inf)(using ctx50s).exists(_ <= 50.seconds))
    assertEquals(RecallValidator.limit(1.second)(using ctx50s), Some(1.second))

  test("RecallValidator.result"):
    def result(output: ExecResult): RecallValidation = RecallValidator.result("python3", "python3 -", 1.second, output)

    assertEquals(
      result(ExecResult.Exit(0, "recheck:runtime python 3.15.0\nrecheck:start\nrecheck:time 12\n", "")),
      RecallValidation("python 3.15.0", 1.second, RecallResult.Finish(12.millis))
    )
    assertEquals(
      result(ExecResult.Exit(0, "recheck:runtime php 8.5\nrecheck:engine-limit Backtrack limit exhausted\n", "")),
      RecallValidation("php 8.5", 1.second, RecallResult.EngineLimit("Backtrack limit exhausted"))
    )
    assertEquals(
      result(
        ExecResult.Exit(0, "recheck:runtime node v26\nrecheck:engine-error Maximum call stack size exceeded\n", "")
      ),
      RecallValidation("node v26", 1.second, RecallResult.EngineError("Maximum call stack size exceeded"))
    )
    // Without the runtime line, the runtime is the program name.
    assertEquals(result(ExecResult.Timeout("")), RecallValidation("python3", 1.second, RecallResult.Timeout))
    interceptMessage[UnexpectedException]("invalid recall validation output: foo")(
      result(ExecResult.Exit(0, "foo\n", ""))
    )
    interceptMessage[UnexpectedException]("error")(result(ExecResult.Exit(1, "", "error\n")))
    interceptMessage[UnexpectedException]("`python3 -` exited with 2")(result(ExecResult.Exit(2, "", "")))
