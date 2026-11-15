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
import codes.quine.labs.recheck.exec.BoostRegexExecutor
import codes.quine.labs.recheck.exec.DotNetExecutor
import codes.quine.labs.recheck.exec.ICUExecutor
import codes.quine.labs.recheck.exec.JavaExecutor
import codes.quine.labs.recheck.exec.PythonExecutor
import codes.quine.labs.recheck.exec.SourceFile
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.PerlSource

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
    RuntimeAttackVerifier.verify(source, flags, dialect, mode, attack, timeout, Duration.Inf, env).get

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

  // Checked with regex 2026.9.29 on Python 3.15.2.
  runtimeTest("python-regex", "python3"):
    val r = verify("^(a|a)*$", "", Dialect.PythonRegex(), attack("a", 31, "\u0000"))
    assertEquals(r.result, RuntimeAttackResult.Timeout)
    assert(clue(r.runtime).startsWith("python-regex 2026."))
    // The guard of regex makes `(a+)+` fast, which refutes the attack.
    assertFinish(verify("^(a+)+$", "", Dialect.PythonRegex(), attack("a", 31, "\u0000")))
    val bytes = Dialect.PythonRegex(Dialect.Python.Type.Bytes)
    assertEquals(
      verify("^(?:é|é)*$", "", bytes, attack("é", 31, "\u0000")).result,
      RuntimeAttackResult.Timeout
    )

  runtimeTest("python-regex: a Python without the package", "python3"):
    // `-S` leaves out the site packages, and the program tells why the import fails on the standard error.
    val env = Map("RECHECK_PYTHON_REGEX" -> "python3 -S")
    val ex = intercept[UnexpectedException](verify("a", "", Dialect.PythonRegex(), attack("a", 3, ""), env = env))
    assert(clue(ex.getMessage).startsWith("The `regex` package is not available for "))

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
    // The first run builds the program, which takes some seconds.
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
    // The class of the Java program is `RecheckRuntimeAttack`, which names the program file.
    case _: JavaExecutor               => run(JavaExecutor(SourceFile("RecheckRuntimeAttack.java", script.content)))
    case _: DotNetExecutor             => run(DotNetExecutor(SourceFile(script.name, script.content)))
    case _: ICUExecutor                => run(ICUExecutor(SourceFile(script.name, script.content)), env = icuEnv)
    case BoostRegexExecutor(_, hasICU) =>
      run(BoostRegexExecutor(SourceFile(script.name, script.content), hasICU), env = boostRegexEnv)
    case executor => run(executor, script.content)

  for (dialect, program) <- Seq(
      Dialect.JavaScript -> "node",
      Dialect.Python() -> "python3",
      Dialect.Ruby() -> "ruby",
      Dialect.Java -> "java",
      Dialect.DotNet -> "dotnet",
      Dialect.Perl() -> "perl",
      Dialect.PCRE -> "php",
      Dialect.ICU -> "c++",
      Dialect.BoostRegex() -> "c++",
      Dialect.BoostRegex(Dialect.BoostRegex.Type.U32Regex) -> "c++"
    )
  do
    runtimeTest(s"RuntimeAttackProgram.generateScript: $dialect", program):
      val source = "^(?:a|é|😀)*$"
      val mode = ReDoS.parse(source, "", dialect).mode
      def units(s: String): UString = mode.fromCodePoints(IArray.from(UnitMode.CodePoint.decode(s).iterator))

      val astral = AttackPattern(Seq((units("é"), units("😀a"), 0)), UString(0), 3)
      val script = RuntimeAttackProgram.generateScript(source, "", dialect, mode, astral).toOption.get
      val out = runScript(script, RuntimeAttackProgram.generate(source, "", dialect, mode, astral).toOption.get)
      val lines = out.linesIterator.map(_.takeWhile(_ != ' ')).toSeq
      assertEquals(lines.filter(_.startsWith("recheck:")), Seq("recheck:runtime", "recheck:start", "recheck:time"))

  // Checked with ICU 79.1, which has no time limit and a backtrack stack of 8,000,000 bytes by default.
  runtimeTest("ICU", "c++"):
    val r = verify("^(a|a)*$", "", Dialect.ICU, attack("a", 31, "\u0000"), env = icuEnv)
    assertEquals(r.result, RuntimeAttackResult.Timeout)
    assertEquals(r.runtime, "icu 79.1 (Unicode 18.0)")
    assertFinish(verify("^(a|a)*$", "", Dialect.ICU, attack("a", 3, "\u0000"), env = icuEnv))
    // ICU's full case folding makes `ß` and `ss` ambiguous.
    val folding = verify("^(?:ss|ß)*$", "i", Dialect.ICU, attack("ß", 31, "!"), env = icuEnv)
    assertEquals(folding.result, RuntimeAttackResult.Timeout)
    // The backtrack stack overflows even on a linear matching.
    val overflow = verify("^(?:a|b)*$", "", Dialect.ICU, attack("a", 1_000_000, "!"), 10.seconds, icuEnv)
    assertEquals(overflow.result, RuntimeAttackResult.EngineError("U_REGEX_STACK_OVERFLOW"))

  runtimeTest("RECHECK_ICU", "c++"):
    // A failed compiler, which runs first for its version, tells its output.
    val env = Map("RECHECK_ICU" -> "sh -c 'echo no ICU >&2; exit 1'")
    val ex = intercept[UnexpectedException](verify("a", "", Dialect.ICU, attack("a", 3, ""), env = env))
    assertEquals(ex.getMessage, "cannot run `sh -c 'echo no ICU >&2; exit 1' --version`:\nno ICU")
    // A pattern that ICU rejects, though recheck accepts it, exits with the error.
    val invalid = intercept[UnexpectedException](verify("[&-c]", "", Dialect.ICU, attack("a", 3, ""), env = icuEnv))
    assertEquals(invalid.getMessage, "U_REGEX_INVALID_RANGE at 4")

  // Checked with Boost.Regex 1.92.0, whose complexity limit stops most attacks of short patterns within a second.
  runtimeTest("Boost.Regex", "c++"):
    val r = verify("^(a|a)*$", "", Dialect.BoostRegex(), attack("a", 31, "\u0000"), env = boostRegexEnv)
    assert(clue(r.result).isInstanceOf[RuntimeAttackResult.EngineLimit])
    assert(r.isConfirmed)
    assertEquals(r.runtime, "boost-regex 1.92.0")
    assertFinish(verify("^(a|a)*$", "", Dialect.BoostRegex(), attack("a", 3, "\u0000"), env = boostRegexEnv))
    // `p` (`match_posix`) tries every path for the longest match.
    val posix = verify("(a|a)*", "p", Dialect.BoostRegex(), attack("a", 31, "\u0000"), env = boostRegexEnv)
    assert(clue(posix.result).isInstanceOf[RuntimeAttackResult.EngineLimit])
    assertFinish(verify("(a|a)*", "", Dialect.BoostRegex(), attack("a", 31, "\u0000"), env = boostRegexEnv))
    // `E` (`match_not_null`) and `I` (`match_not_initial_null`) reject the empty match at the start.
    for flags <- Seq("E", "I") do
      val notNull = verify("(?:|(?:a|a)*c)", flags, Dialect.BoostRegex(), attack("a", 31, ""), env = boostRegexEnv)
      assert(clue(notNull.result).isInstanceOf[RuntimeAttackResult.EngineLimit])
    // The limit grows with the length of the pattern, so a long one makes a polynomial attack slow.
    val long = s"^a*a*${"(?:z)?" * 50}$$"
    val slow = verify(long, "", Dialect.BoostRegex(), attack("a", 100_000, "b"), env = boostRegexEnv)
    assertEquals(slow.result, RuntimeAttackResult.Timeout)
    // The backtracking stack overflows even on a linear matching.
    val overflow = verify("^(a)*$", "", Dialect.BoostRegex(), attack("a", 100_000, ""), env = boostRegexEnv)
    assertEquals(
      overflow.result,
      RuntimeAttackResult.EngineError("Ran out of stack space trying to match the regular expression.")
    )
    // `u32regex` has the classes of ICU, where `\w` has `é`.
    val u32 = Dialect.BoostRegex(Dialect.BoostRegex.Type.U32Regex)
    val unicode = verify("^(\\w|\u00e9)*$", "", u32, attack("\u00e9", 31, "!"), env = boostRegexEnv)
    assert(clue(unicode.result).isInstanceOf[RuntimeAttackResult.EngineLimit])
    assertEquals(unicode.runtime, "boost-regex 1.92.0 (ICU 79.1)")

  runtimeTest("RECHECK_BOOST_REGEX", "c++"):
    // A failed compiler, which runs first for its version, tells its output.
    val env = Map("RECHECK_BOOST_REGEX" -> "sh -c 'echo no Boost >&2; exit 1'")
    val ex = intercept[UnexpectedException](verify("a", "", Dialect.BoostRegex(), attack("a", 3, ""), env = env))
    assertEquals(ex.getMessage, "cannot run `sh -c 'echo no Boost >&2; exit 1' --version`:\nno Boost")

  runtimeTest("RECHECK_RUBY", "ruby"):
    // The wrapper's runtime line comes first, and it shows the arguments.
    val ruby = script("my ruby", s"echo recheck:runtime wrapped \"$$@\"; exec '${which("ruby").get}' \"$$@\"")
    val env = Map("RECHECK_RUBY" -> s"'$ruby' --yjit")
    val r = verify("^(a|a)*$", "", Dialect.Ruby(), attack("a", 3, "\u0000"), env = env)
    assertEquals(r.runtime, "wrapped --yjit --disable-gems -")
    assertFinish(r)

  test("RECHECK_JAVA: a fake runtime"):
    // A fake runtime prints its version, creates the class file of a compilation, and reads the data of the program on
    // a run.
    val java = script(
      "fake java",
      """|if [ "$1" = -version ]; then echo fake 1.0; exit 0; fi
         |if [ "$1" = -m ]; then mkdir "$6"; touch "$6/RecheckRuntimeAttack.class"; exit 0; fi
         |cat > /dev/null
         |test -d "$2" || exit 2
         |echo "recheck:runtime fake $1 $3"
         |echo recheck:start
         |echo recheck:engine-error StackOverflowError""".stripMargin
    )
    val r = verify("a", "", Dialect.Java, attack("a", 3, ""), env = Map("RECHECK_JAVA" -> s"\"$java\""))
    assertEquals(
      r,
      RuntimeAttack("fake -cp RecheckRuntimeAttack", 1.second, RuntimeAttackResult.EngineError("StackOverflowError"))
    )
    // A run exiting without a message tells the command line.
    val exit =
      "sh -c 'case \"$1\" in -version) echo 1.0 ;; -m) mkdir \"$6\"; touch \"$6/RecheckRuntimeAttack.class\" ;; " +
        "*) exit 3 ;; esac' sh"
    interceptMessage[UnexpectedException](
      s"`$exit -m jdk.compiler/com.sun.tools.javac.Main --release 17 -d RecheckRuntimeAttack RecheckRuntimeAttack.java && " +
        s"$exit -cp RecheckRuntimeAttack RecheckRuntimeAttack` exited with 3"
    ):
      verify("a", "", Dialect.Java, attack("a", 3, ""), env = Map("RECHECK_JAVA" -> exit))

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
        Duration.Inf,
        Map.empty
      )
