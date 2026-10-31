package codes.quine.labs.recheck.recall

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.exec.Command
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString

class RecallValidatorSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val attack: AttackPattern =
    AttackPattern(Seq((UString.fromString("x"), UString.fromString("y"), 0)), UString.fromString("z"), 2)

  def parse(source: String, dialect: Dialect = Dialect.JavaScript): ParsedPattern =
    ParsedPattern.parse(source, "", dialect).toTry.get

  test("RecallValidator.validate"):
    assertEquals(
      RecallValidator.validate(parse("foo"), attack, Duration.MinusInf)((_, _, _) => fail("unreachable")),
      None
    )
    assertEquals(RecallValidator.validate(parse("foo"), attack, Duration.Zero)((_, _, _) => fail("unreachable")), None)
    // A dialect without a runner is not run.
    assertEquals(
      RecallValidator.validate(parse("foo", Dialect.Python), attack, 1.second)((_, _, _) => fail("unreachable")),
      None
    )

    var command: Command = null
    assertEquals(
      RecallValidator.validate(parse("foo"), attack, 1.second) { (c, _, _) =>
        command = c
        ExecResult.Timeout("recheck:runtime node v26.11.1\nrecheck:start\n")
      },
      Some(RecallValidation("node v26.11.1", 1.second, RecallResult.Timeout))
    )
    assertEquals(command.program, "node")

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
    val command = Command("python3")
    def result(output: ExecResult): RecallValidation = RecallValidator.result(command, 1.second, output)
    assertEquals(
      result(ExecResult.Exit(0, "recheck:runtime python 3.15.0\nrecheck:start\nrecheck:time 12\n", "")),
      RecallValidation("python 3.15.0", 1.second, RecallResult.Finish(12.millis))
    )
    assertEquals(
      result(ExecResult.Exit(0, "recheck:runtime php 8.5\nrecheck:engine-error Backtrack limit exhausted\n", "")),
      RecallValidation("php 8.5", 1.second, RecallResult.EngineError("Backtrack limit exhausted"))
    )
    // Without the runtime line, the runtime is the program name.
    assertEquals(result(ExecResult.Timeout("")), RecallValidation("python3", 1.second, RecallResult.Timeout))
    interceptMessage[UnexpectedException]("invalid recall validation output: foo")(
      result(ExecResult.Exit(0, "foo\n", ""))
    )
    interceptMessage[UnexpectedException]("error")(result(ExecResult.Exit(1, "", "error\n")))
    interceptMessage[UnexpectedException]("`python3` exited with 2")(result(ExecResult.Exit(2, "", "")))
