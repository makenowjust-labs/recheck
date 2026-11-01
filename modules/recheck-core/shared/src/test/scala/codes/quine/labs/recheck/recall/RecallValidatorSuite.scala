package codes.quine.labs.recheck.recall

import scala.concurrent.duration.*

import codes.quine.labs.recheck.SourcePattern
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.exec.Command
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.exec.Executor
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString

class RecallValidatorSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val attack: AttackPattern =
    AttackPattern(Seq((UString.fromString("x"), UString.fromString("y"), 0)), UString.fromString("z"), 2)

  def parse(source: String, dialect: Dialect = Dialect.Python()): SourcePattern =
    SourcePattern.parse(source, "", dialect).toTry.get._1

  test("RecallValidator.validate"):
    assertEquals(
      RecallValidator.validate(parse("foo"), attack, Duration.MinusInf)((_, _, _) => fail("unreachable")),
      None
    )
    assertEquals(RecallValidator.validate(parse("foo"), attack, Duration.Zero)((_, _, _) => fail("unreachable")), None)
    // A Perl pattern interpolating a variable is not run.
    assertEquals(
      RecallValidator.validate(parse("a$x", Dialect.Perl()), attack, 1.second)((_, _, _) => fail("unreachable")),
      None
    )

    var command: Command = null
    assertEquals(
      RecallValidator.validate(parse("foo"), attack, 1.second) { (c, _, _) =>
        command = c
        ExecResult.Timeout("recheck:runtime python 3.15.0\nrecheck:start\n")
      },
      Some(RecallValidation("python 3.15.0", 1.second, RecallResult.Timeout))
    )
    assertEquals(command.program, "python3")

  test("RecallValidator.validate: environment variables"):
    def run(env: Map[String, String], output: ExecResult = ExecResult.Timeout("")): (Command, RecallValidation) =
      var command: Command = null
      val result = RecallValidator.validate(parse("foo"), attack, 1.second, env = env) { (c, _, _) =>
        command = c
        output
      }
      (command, result.get)

    val (command, result) = run(Map("RECHECK_PYTHON" -> " \"/opt/my python/bin/python3\" -X dev "))
    assertEquals(
      command,
      Command(Executor.shell, Seq("-c", "\"/opt/my python/bin/python3\" -X dev \"$@\"", "sh", "-I", "-"))
    )
    // Without the runtime line, the runtime is the command line of the variable.
    assertEquals(result.runtime, "\"/opt/my python/bin/python3\" -X dev")
    interceptMessage[UnexpectedException]("`\"/opt/my python/bin/python3\" -X dev -I -` exited with 2")(
      run(Map("RECHECK_PYTHON" -> "\"/opt/my python/bin/python3\" -X dev"), ExecResult.Exit(2, "", ""))
    )
    // An empty value and the variables of the other dialects are ignored.
    assertEquals(run(Map("RECHECK_PYTHON" -> " ", "RECHECK_RUBY" -> "ruby"))._1, Command("python3", Seq("-I", "-")))

  test("RecallValidator.validate: timeout"):
    def limit(timeout: Duration)(using Context): Option[FiniteDuration] =
      var limit: Option[FiniteDuration] = null
      RecallValidator.validate(parse("foo"), attack, timeout) { (_, _, t) =>
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
