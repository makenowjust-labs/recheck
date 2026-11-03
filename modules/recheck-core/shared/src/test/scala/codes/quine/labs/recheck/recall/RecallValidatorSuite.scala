package codes.quine.labs.recheck.recall

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UnitMode

class RecallValidatorSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val attack: AttackPattern =
    AttackPattern(
      Seq((UnitMode.CodePoint.decode("x"), UnitMode.CodePoint.decode("y"), 0)),
      UnitMode.CodePoint.decode("z"),
      2
    )

  test("RecallValidator.validate"):
    assertEquals(
      RecallValidator.validate("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, Duration.MinusInf, Map.empty),
      None
    )
    assertEquals(
      RecallValidator.validate("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, Duration.Zero, Map.empty),
      None
    )
    // A Perl pattern interpolating a variable is not run.
    assertEquals(
      RecallValidator.validate("a$x", "", Dialect.Perl(), UnitMode.CodePoint, attack, 1.second, Map.empty),
      None
    )

  test("RecallValidator.boundTimeout"):
    assertEquals(RecallValidator.boundTimeout(Duration.Inf), None)
    assertEquals(RecallValidator.boundTimeout(100.seconds), Some(100.seconds))
    val ctx50s = Context(timeout = 50.seconds)
    assert(RecallValidator.boundTimeout(100.seconds)(using ctx50s).exists(_ <= 50.seconds))
    assert(RecallValidator.boundTimeout(Duration.Inf)(using ctx50s).exists(_ <= 50.seconds))
    assertEquals(RecallValidator.boundTimeout(1.second)(using ctx50s), Some(1.second))

  test("RecallValidator.parseOutput"):
    def parse(output: ExecResult): RecallValidation =
      RecallValidator.parseOutput("python3", "python3 -", 1.second, output)

    assertEquals(
      parse(ExecResult.Exit(0, "recheck:runtime python 3.15.0\nrecheck:start\nrecheck:time 12\n", "")),
      RecallValidation("python 3.15.0", 1.second, RecallResult.Finish(12.millis))
    )
    assertEquals(
      parse(ExecResult.Exit(0, "recheck:runtime php 8.5\nrecheck:engine-limit Backtrack limit exhausted\n", "")),
      RecallValidation("php 8.5", 1.second, RecallResult.EngineLimit("Backtrack limit exhausted"))
    )
    assertEquals(
      parse(
        ExecResult.Exit(0, "recheck:runtime node v26\nrecheck:engine-error Maximum call stack size exceeded\n", "")
      ),
      RecallValidation("node v26", 1.second, RecallResult.EngineError("Maximum call stack size exceeded"))
    )
    // Without the runtime line, the runtime is the program name.
    assertEquals(parse(ExecResult.Timeout("")), RecallValidation("python3", 1.second, RecallResult.Timeout))
    interceptMessage[UnexpectedException]("invalid recall validation output: foo")(
      parse(ExecResult.Exit(0, "foo\n", ""))
    )
    interceptMessage[UnexpectedException]("error")(parse(ExecResult.Exit(1, "", "error\n")))
    interceptMessage[UnexpectedException]("`python3 -` exited with 2")(parse(ExecResult.Exit(2, "", "")))
