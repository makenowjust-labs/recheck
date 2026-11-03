package codes.quine.labs.recheck.verification

import java.nio.file.Path
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.RuntimeHelper
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.recheck.exec.DotNetExecutor
import codes.quine.labs.recheck.exec.JavaExecutor
import codes.quine.labs.recheck.exec.PythonExecutor
import codes.quine.labs.recheck.exec.SourceFile
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class RuntimeAttackVerifierRuntimeSuite extends munit.FunSuite with RuntimeHelper:

  override val munitTimeout: Duration = 60.seconds

  /** A default context. */
  given ctx: Context = Context()

  /** Returns the attack string `s.repeat(n) + t`. */
  def attack(s: String, n: Int, t: String): AttackPattern =
    AttackPattern(Seq((UString.empty, UnitMode.CodePoint.decode(s), 0)), UnitMode.CodePoint.decode(t), n)

  def verify(
      source: String,
      flags: String,
      dialect: Dialect,
      attack: AttackPattern,
      timeout: Duration = 1.second,
      env: Map[String, String] = Map.empty
  ): RuntimeAttack =
    val mode = ReDoS.parse(source, flags, dialect).mode
    RuntimeAttackVerifier.verify(source, flags, dialect, mode, attack, timeout, env).get

  /** Creates a shell script in a new directory with a space in its name. */
  def script(name: String, body: String): Path =
    val file = createFile("recheck runtime ", name, s"#!/bin/sh\n$body\n")
    file.toFile.setExecutable(true)
    file

  def assertFinish(r: RuntimeAttack)(using munit.Location): Unit =
    assert(clue(r).result.isInstanceOf[RuntimeAttackResult.Finish])
    assert(!r.isConfirmed)

  runtimeTest("JavaScript", "node"):
    val r = verify("^(a|a)*$", "", Dialect.JavaScript, attack("a", 31, "\u0000"))
    assertEquals(r.result, RuntimeAttackResult.Timeout)
    assert(clue(r.runtime).startsWith("node v"))
    assertFinish(verify("^(a|a)*$", "", Dialect.JavaScript, attack("a", 3, "\u0000")))
    // V8 overflows its backtracking stack even on a linear matching.
    val overflow = verify("^(?:a|b)*$", "", Dialect.JavaScript, attack("a", 10_000_000, "!"), 10.seconds)
    assertEquals(overflow.result, RuntimeAttackResult.EngineError("Maximum call stack size exceeded"))
    assert(!overflow.isConfirmed)

  runtimeTest("Python", "python3"):
    // Python 3.15 doubles its time for each `a`.
    val r = verify("^(a|a)*$", "", Dialect.Python(), attack("a", 31, "\u0000"))
    assertEquals(r.result, RuntimeAttackResult.Timeout)
    assert(clue(r.runtime).startsWith("python 3."))
    assertEquals(
      verify("^(?:é|é)*$", "", Dialect.Python(Dialect.Python.Type.Bytes), attack("é", 31, "\u0000")).result,
      RuntimeAttackResult.Timeout
    )
    assertFinish(verify("^(?:é|é)*$", "", Dialect.Python(Dialect.Python.Type.Bytes), attack("é", 3, "\u0000")))

  runtimeTest("Python: the C locale", "python3"):
    // `re.L` has no non-ASCII word byte in a UTF-8 locale, so it needs a Latin-1 locale to differ from the C locale.
    val env = Map("RECHECK_PYTHON" -> "env LC_ALL=en_US.ISO8859-1 python3")
    val probe = run(PythonExecutor, "import re; print(re.match(rb'\\w', b'\\xe9', re.L))", env)
    assert(probe.trim != "None", "no en_US.ISO8859-1 locale")
    val bytes = Dialect.Python(Dialect.Python.Type.Bytes)
    assertFinish(verify("^(\\w|\\w)*$", "L", bytes, attack("é", 31, "\u0000"), env = env))
    val unicode = verify("^(\\w|\\w)*$", "", Dialect.Python(), attack("é", 31, "\u0000"), env = env)
    assertEquals(unicode.result, RuntimeAttackResult.Timeout)

  runtimeTest("Ruby", "ruby"):
    // Ruby 4.0's memoization makes it fast.
    val r = verify("^(a|a)*$", "", Dialect.Ruby(), attack("a", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("ruby "))
    assertFinish(verify("^(?:é|é)*$", "n", Dialect.Ruby(), attack("Ã©", 31, "\u0000")))

  runtimeTest("Ruby: the program in UTF-8", "ruby"):
    // The C locale would read a non-ASCII character of the program as an invalid US-ASCII byte.
    val env = Map("RECHECK_RUBY" -> "env LC_ALL=C ruby")
    assertFinish(verify("^(?:é|é)*$", "", Dialect.Ruby(), attack("é", 3, "\u0000"), env = env))

  runtimeTest("Java", "java"):
    val r = verify("^(a|a)*$", "", Dialect.Java, attack("a", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("java "))
    // `java.util.regex` recurses on each iteration of a group, which is not an effective attack.
    val overflow = verify("^(?:a|b)*$", "", Dialect.Java, attack("a", 100000, "!"))
    assertEquals(overflow.result, RuntimeAttackResult.EngineError("StackOverflowError"))
    assert(!overflow.isConfirmed)
    // The data of the program keeps a character out of the BMP.
    val astral =
      AttackPattern(Seq((UString.empty, UnitMode.CodePoint.decode("😀"), 0)), UnitMode.CodePoint.decode("!"), 3)
    assertFinish(verify("^(?:😀)*!$", "", Dialect.Java, astral))

  runtimeTest(".NET", "dotnet"):
    // The first run of `dotnet run -` builds the program, which takes some seconds.
    val r = verify("^(a|a)*$", "", Dialect.DotNet, attack("a", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith(".NET "))
    // The runs share the program, which the first run builds.
    val rs = (1 to 4).map(_ => Future(verify("^(a|a)*$", "i", Dialect.DotNet, attack("A", 3, "\u0000"), 10.seconds)))
    Await.result(Future.sequence(rs), 60.seconds).foreach(assertFinish)

  runtimeTest(".NET: the invariant culture", "dotnet"):
    // The Turkish culture matches `i` with `İ` and `I` with `ı` under `IgnoreCase`.
    val env = Map("RECHECK_DOTNET" -> "env LC_ALL=tr_TR.UTF-8 dotnet")
    def verifyTurkish(c: String): RuntimeAttack =
      verify("^[ia]*[ib]*[ic]*$", "i", Dialect.DotNet, attack(c, 2000, "!"), env = env)

    assertFinish(verifyTurkish("İ"))
    assertEquals(verifyTurkish("I").result, RuntimeAttackResult.Timeout)

  runtimeTest("Perl", "perl"):
    val r = verify("^(a|a)*$", "", Dialect.Perl(), attack("é", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("perl 5."))
    // A string pattern may contain `$`, which a regex literal would interpolate.
    val perlString = Dialect.Perl(source = PerlSource.String)
    assertFinish(verify("^(?:a|a)*$x", "", perlString, attack("a", 31, "")))

  runtimeTest("Perl: the C locale", "perl"):
    // Perl takes the locale of the environment, whose `\w` under `l` matches `é` in a UTF-8 locale.
    val env = Map("RECHECK_PERL" -> "env LC_ALL=en_US.UTF-8 perl")
    assertFinish(verify("^\\w*\\w*\\w*$", "l", Dialect.Perl(), attack("é", 3000, "\u0000"), env = env))
    val unicode = verify("^\\w*\\w*\\w*$", "u", Dialect.Perl(), attack("é", 3000, "\u0000"), env = env)
    assertEquals(unicode.result, RuntimeAttackResult.Timeout)

  runtimeTest("PCRE", "php"):
    // PHP's default `pcre.backtrack_limit` stops the match.
    val r = verify("^(?:é|é)*$", "", Dialect.PCRE, attack("Ã©", 31, "\u0000"))
    assertEquals(r.result, RuntimeAttackResult.EngineLimit("Backtrack limit exhausted"))
    assert(r.isConfirmed)
    assert(clue(r.runtime).startsWith("php "))
    assertEquals(
      verify("^(?:é|é)*$", "u", Dialect.PCRE, attack("é", 31, "\u0000")).result,
      RuntimeAttackResult.EngineLimit("Backtrack limit exhausted")
    )
    assertFinish(verify("^(?:é|é)*$", "u", Dialect.PCRE, attack("é", 3, "\u0000")))

  /** Runs the standalone program on the runtime of the program of the dialect, and returns the output. */
  def runScript(script: RuntimeAttackProgram.Script, program: RuntimeAttackProgram): String = program.executor match
    case _: JavaExecutor   => run(JavaExecutor(SourceFile(script.name, script.content)))
    case _: DotNetExecutor => run(DotNetExecutor(SourceFile(script.name, script.content)))
    case executor          => run(executor, script.content)

  for (dialect, program) <- Seq(
      Dialect.JavaScript -> "node",
      Dialect.Python() -> "python3",
      Dialect.Ruby() -> "ruby",
      Dialect.Java -> "java",
      Dialect.DotNet -> "dotnet",
      Dialect.Perl() -> "perl",
      Dialect.PCRE -> "php"
    )
  do
    runtimeTest(s"RuntimeAttackProgram.generateScript: $dialect", program):
      val source = "^(?:a|é|😀)*$"
      val mode = ReDoS.parse(source, "", dialect).mode
      def units(s: String): UString = mode.fromCodePoints(UnitMode.CodePoint.decode(s))

      val astral = AttackPattern(Seq((units("é"), units("😀a"), 0)), UString(0), 3)
      val script = RuntimeAttackProgram.generateScript(source, "", dialect, mode, astral).toOption.get
      val out = runScript(script, RuntimeAttackProgram.generate(source, "", dialect, mode, astral).toOption.get)
      val lines = out.linesIterator.map(_.takeWhile(_ != ' ')).toSeq
      assertEquals(lines.filter(_.startsWith("recheck:")), Seq("recheck:runtime", "recheck:start", "recheck:time"))

  runtimeTest("RECHECK_RUBY", "ruby"):
    // The wrapper's runtime line comes first, and it shows the arguments.
    val ruby = script("my ruby", s"echo recheck:runtime wrapped \"$$@\"; exec '${which("ruby").get}' \"$$@\"")
    val env = Map("RECHECK_RUBY" -> s"'$ruby' --disable-gems")
    val r = verify("^(a|a)*$", "", Dialect.Ruby(), attack("a", 3, "\u0000"), env = env)
    assertEquals(r.runtime, "wrapped --disable-gems -")
    assertFinish(r)

  test("RECHECK_JAVA: a fake runtime"):
    // A fake runtime reads the data of the program, and gets the path of the program as the last argument.
    val java = script(
      "fake java",
      """|cat > /dev/null
         |eval "file=\${$#}"
         |test -f "$file" || exit 2
         |echo "recheck:runtime fake $1 $2"
         |echo recheck:start
         |echo recheck:engine-error StackOverflowError""".stripMargin
    )
    val r = verify("a", "", Dialect.Java, attack("a", 3, ""), env = Map("RECHECK_JAVA" -> s"\"$java\""))
    assertEquals(r, RuntimeAttack("fake --source 17", 1.second, RuntimeAttackResult.EngineError("StackOverflowError")))
    interceptMessage[UnexpectedException]("`sh -c 'exit 3' --source 17 RecheckRuntimeAttack.java` exited with 3"):
      verify("a", "", Dialect.Java, attack("a", 3, ""), env = Map("RECHECK_JAVA" -> "sh -c 'exit 3'"))

  runtimeTest("RuntimeAttackVerifier.verify: the deadline of the context", "node"):
    given Context = Context(timeout = 50.millis)
    val exponential = attack("a", 31, "\u0000")
    intercept[TimeoutException]:
      RuntimeAttackVerifier.verify(
        "^(a|a)*$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        exponential,
        10.seconds,
        Map.empty
      )
