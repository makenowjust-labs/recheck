package codes.quine.labs.recheck.recall

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.exec.Executor
import codes.quine.labs.recheck.exec.PythonExecutor
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
      RecallValidator.validate("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, Duration.MinusInf)(
        (_, _, _, _) => fail("unreachable")
      ),
      None
    )
    assertEquals(
      RecallValidator.validate("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, Duration.Zero)((_, _, _, _) =>
        fail("unreachable")
      ),
      None
    )
    // A Perl pattern interpolating a variable is not run.
    assertEquals(
      RecallValidator.validate("a$x", "", Dialect.Perl(), UnitMode.CodePoint, attack, 1.second)((_, _, _, _) =>
        fail("unreachable")
      ),
      None
    )

    var executor: Executor = null
    assertEquals(
      RecallValidator.validate("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, 1.second) { (e, _, _, _) =>
        executor = e
        ExecResult.Timeout("recheck:runtime python 3.15.0\nrecheck:start\n")
      },
      Some(RecallValidation("python 3.15.0", 1.second, RecallResult.Timeout))
    )
    assertEquals(executor, PythonExecutor)

  test("RecallValidator.validate: environment variables"):
    def run(env: Map[String, String], output: ExecResult = ExecResult.Timeout("")): RecallValidation =
      RecallValidator
        .validate("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, 1.second, env = env) { (_, _, _, e) =>
          assertEquals(e, env)
          output
        }
        .get

    // Without the runtime line, the runtime is the command line of the variable.
    assertEquals(
      run(Map("RECHECK_PYTHON" -> " \"/opt/my python/bin/python3\" -X dev ")).runtime,
      "\"/opt/my python/bin/python3\" -X dev"
    )
    interceptMessage[UnexpectedException]("`\"/opt/my python/bin/python3\" -X dev -I -` exited with 2")(
      run(Map("RECHECK_PYTHON" -> "\"/opt/my python/bin/python3\" -X dev"), ExecResult.Exit(2, "", ""))
    )
    // An empty value and the variables of the other dialects are ignored.
    assertEquals(run(Map("RECHECK_PYTHON" -> " ", "RECHECK_RUBY" -> "ruby")).runtime, "python3")

  test("RecallValidator.validate: timeout"):
    def limit(timeout: Duration)(using Context): Option[FiniteDuration] =
      var limit: Option[FiniteDuration] = null
      RecallValidator.validate("foo", "", Dialect.Python(), UnitMode.CodePoint, attack, timeout) { (_, _, t, _) =>
        limit = t
        ExecResult.Timeout("")
      }
      limit
    assertEquals(limit(Duration.Inf), None)
    assertEquals(limit(100.seconds), Some(100.seconds))
    val ctx50s = Context(timeout = 50.seconds)
    assert(limit(100.seconds)(using ctx50s).exists(_ <= 50.seconds))
    assert(limit(Duration.Inf)(using ctx50s).exists(_ <= 50.seconds))
    assertEquals(limit(1.second)(using ctx50s), Some(1.second))

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
