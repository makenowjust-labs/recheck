package codes.quine.labs.recheck.verification

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

class RuntimeAttackVerifierSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val attack: AttackPattern =
    AttackPattern(
      Seq((UnitMode.CodePoint.decode("x"), UnitMode.CodePoint.decode("y"), 0)),
      UnitMode.CodePoint.decode("z"),
      2
    )

  test("RuntimeAttackVerifier.verify"):
    assertEquals(
      RuntimeAttackVerifier
        .verify("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, Duration.MinusInf, Map.empty),
      None
    )
    assertEquals(
      RuntimeAttackVerifier.verify("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, Duration.Zero, Map.empty),
      None
    )
    // A Perl pattern interpolating a variable is not run.
    assertEquals(
      RuntimeAttackVerifier.verify("a$x", "", Dialect.Perl(), UnitMode.CodePoint, attack, 1.second, Map.empty),
      None
    )

  test("RuntimeAttackVerifier.boundTimeout"):
    assertEquals(RuntimeAttackVerifier.boundTimeout(Duration.Inf), None)
    assertEquals(RuntimeAttackVerifier.boundTimeout(100.seconds), Some(100.seconds))
    val ctx50s = Context(timeout = 50.seconds)
    assert(RuntimeAttackVerifier.boundTimeout(100.seconds)(using ctx50s).exists(_ <= 50.seconds))
    assert(RuntimeAttackVerifier.boundTimeout(Duration.Inf)(using ctx50s).exists(_ <= 50.seconds))
    assertEquals(RuntimeAttackVerifier.boundTimeout(1.second)(using ctx50s), Some(1.second))

  test("RuntimeAttackVerifier.parseOutput"):
    def parse(output: ExecResult): RuntimeAttack =
      RuntimeAttackVerifier.parseOutput("python3", "python3 -", 1.second, output)

    assertEquals(
      parse(ExecResult.Exit(0, "recheck:runtime python 3.15.0\nrecheck:start\nrecheck:time 12\n", "")),
      RuntimeAttack("python 3.15.0", 1.second, RuntimeAttackResult.Finish(12.millis))
    )
    assertEquals(
      parse(ExecResult.Exit(0, "recheck:runtime php 8.5\nrecheck:engine-limit Backtrack limit exhausted\n", "")),
      RuntimeAttack("php 8.5", 1.second, RuntimeAttackResult.EngineLimit("Backtrack limit exhausted"))
    )
    assertEquals(
      parse(
        ExecResult.Exit(0, "recheck:runtime node v26\nrecheck:engine-error Maximum call stack size exceeded\n", "")
      ),
      RuntimeAttack("node v26", 1.second, RuntimeAttackResult.EngineError("Maximum call stack size exceeded"))
    )
    // Without the runtime line, the runtime is the program name.
    assertEquals(parse(ExecResult.Timeout("")), RuntimeAttack("python3", 1.second, RuntimeAttackResult.Timeout))
    interceptMessage[UnexpectedException]("invalid runtime attack output: foo")(
      parse(ExecResult.Exit(0, "foo\n", ""))
    )
    interceptMessage[UnexpectedException]("error")(parse(ExecResult.Exit(1, "", "error\n")))
    interceptMessage[UnexpectedException]("`python3 -` exited with 2")(parse(ExecResult.Exit(2, "", "")))
