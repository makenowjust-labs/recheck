package codes.quine.labs.recheck.recall

import java.nio.file.Files
import java.nio.file.Paths
import scala.concurrent.duration.*

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
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

  val params: Parameters =
    Parameters(recallTimeout = 1.second, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)

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

  runtimeTest("Python", "python3"):
    // Python 3.15 doubles its time for each `a`.
    val r = validate("^(a|a)*$", "", Dialect.Python, attack("a", 31, "\u0000"))
    assertEquals(r.result, RecallResult.Timeout)
    assert(clue(r.runtime).startsWith("python 3."))
    assertEquals(validate("^(?:é|é)*$", "b", Dialect.Python, attack("é", 31, "\u0000")).result, RecallResult.Timeout)
    assertFinish(validate("^(?:é|é)*$", "b", Dialect.Python, attack("é", 3, "\u0000")))

  runtimeTest("Ruby", "ruby"):
    // Ruby 4.0's memoization makes it fast.
    val r = validate("^(a|a)*$", "", Dialect.Ruby, attack("a", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("ruby "))
    assertFinish(validate("^(?:é|é)*$", "n", Dialect.Ruby, attack("Ã©", 31, "\u0000")))

  runtimeTest("Java", "java"):
    val r = validate("^(a|a)*$", "", Dialect.Java, attack("a", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("java "))
    // `java.util.regex` recurses on each iteration of a group.
    assertEquals(
      validate("^(?:a|b)*$", "", Dialect.Java, attack("a", 100000, "!")).result,
      RecallResult.EngineError("StackOverflowError")
    )

  runtimeTest(".NET", "dotnet"):
    // The first run of `dotnet run -` builds the program, which takes some seconds.
    val r = validate("^(a|a)*$", "", Dialect.DotNet, attack("a", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith(".NET "))

  runtimeTest("Perl", "perl"):
    val r = validate("^(a|a)*$", "", Dialect.Perl, attack("é", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("perl 5."))
    // A string pattern may contain `$`, which a regex literal would interpolate.
    val pattern = ParsedPattern.parse("^(?:a|a)*$x", "", Dialect.Perl).toTry.get
    val string = RecallValidator.validate(pattern, attack("a", 31, ""), 1.second, perlLiteral = false)(
      RecallValidator.defaultExec.get
    )
    assertFinish(string.get)

  runtimeTest("PCRE", "php"):
    // PHP's default `pcre.backtrack_limit` stops the match.
    val r = validate("^(?:é|é)*$", "", Dialect.PCRE, attack("Ã©", 31, "\u0000"))
    assertEquals(r.result, RecallResult.EngineError("Backtrack limit exhausted"))
    assert(r.succeeded)
    assert(clue(r.runtime).startsWith("php "))
    assertEquals(
      validate("^(?:é|é)*$", "u", Dialect.PCRE, attack("é", 31, "\u0000")).result,
      RecallResult.EngineError("Backtrack limit exhausted")
    )
    assertFinish(validate("^(?:é|é)*$", "u", Dialect.PCRE, attack("é", 3, "\u0000")))

  runtimeTest("ReDoS.checkAll: Ruby", "ruby"):
    // The failed recall validation is kept, and the checking continues to the safe result.
    val ds = ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby))
    assert(clue(ds).size >= 2)
    ds.init.foreach:
      case d: Diagnostics.Vulnerable => assertFinish(d.recallValidation.get)
      case d                         => fail(s"not vulnerable: $d")
    assert(clue(ds.last).isInstanceOf[Diagnostics.Safe])
    assertEquals(ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby)), ds.last)

  runtimeTest("ReDoS.checkAll: Python", "python3"):
    ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.Python)) match
      case List(d: Diagnostics.Vulnerable) => assertEquals(d.recallValidation.map(_.result), Some(RecallResult.Timeout))
      case ds                              => fail(s"unexpected: $ds")

  runtimeTest("ReDoS.checkAll: PCRE", "php"):
    ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.PCRE)) match
      case List(d: Diagnostics.Vulnerable) =>
        assertEquals(d.recallValidation.map(_.result), Some(RecallResult.EngineError("Backtrack limit exhausted")))
      case ds => fail(s"unexpected: $ds")

  test("RecallValidator.validate: the deadline of the context"):
    val pattern = ParsedPattern.parse("foo", "", Dialect.JavaScript).toTry.get
    intercept[TimeoutException]:
      RecallValidator.validate(pattern, attack("a", 1, ""), 50.millis) { (_, _, _) =>
        Thread.sleep(100)
        ExecResult.Timeout("")
      }(using Context(timeout = 50.millis))
