package codes.quine.labs.recheck.recall

import java.nio.file.Files
import java.nio.file.Paths
import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString

/** Runs recall validation on the runtimes in `PATH`, skipping the missing ones.
  *
  * The comments note the behavior of the versions at the time of writing.
  */
class RecallRuntimeSuite extends munit.FunSuite:

  override val munitTimeout: Duration = 60.seconds

  /** A default context. */
  given ctx: Context = Context()

  /** Checks whether the program is in `PATH`. */
  def available(program: String): Boolean =
    sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparatorChar).exists { dir =>
      dir.nonEmpty && Files.isExecutable(Paths.get(dir, program))
    }

  /** `s.repeat(n) + t` */
  def attack(s: String, n: Int, t: String): AttackPattern =
    AttackPattern(Seq((UString.empty, UString.fromString(s), 0)), UString.fromString(t), n)

  def validate(
      source: String,
      flags: String,
      dialect: Dialect,
      attack: AttackPattern,
      timeout: Duration = 1.second
  ): RecallValidation =
    val pattern = ParsedPattern.parse(source, flags, dialect).toTry.get
    RecallValidator.validate(pattern, attack, timeout)(RecallValidator.defaultExec.get).get

  /** Defines a test running only when the program is available. */
  def runtimeTest(name: String, program: String)(body: => Unit)(using munit.Location): Unit =
    test(name):
      assume(available(program), s"`$program` is not in PATH")
      body

  def assertFinish(r: RecallValidation)(using munit.Location): Unit =
    assert(clue(r).result.isInstanceOf[RecallResult.Finish])
    assert(!r.succeeded)

  runtimeTest("JavaScript", "node"):
    val r = validate("^(a|a)*$", "", Dialect.JavaScript, attack("a", 31, "\u0000"))
    assertEquals(r.result, RecallResult.Timeout)
    assert(clue(r.runtime).startsWith("node v"))
    assertFinish(validate("^(a|a)*$", "", Dialect.JavaScript, attack("a", 3, "\u0000")))

  test("RecallValidator.validate: the deadline of the context"):
    val pattern = ParsedPattern.parse("foo", "", Dialect.JavaScript).toTry.get
    intercept[TimeoutException]:
      RecallValidator.validate(pattern, attack("a", 1, ""), 50.millis) { (_, _, _) =>
        Thread.sleep(100)
        ExecResult.Timeout("")
      }(using Context(timeout = 50.millis))
