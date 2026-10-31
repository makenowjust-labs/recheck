package codes.quine.labs.recheck.recall

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.IRNodeData

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
      timeout: Duration = 1.second,
      env: Map[String, String] = Map.empty
  ): RecallValidation =
    val pattern = ParsedPattern.parse(source, flags, dialect).toTry.get
    RecallValidator.validate(pattern, attack, timeout, env = env)(RecallValidator.defaultExec.get).get

  /** Returns the path of the program in `PATH`. */
  def which(program: String): Path =
    sys.env("PATH").split(java.io.File.pathSeparatorChar).map(Paths.get(_, program)).find(Files.isExecutable).get

  /** Creates a shell script in a new directory with a space in its name. */
  def script(name: String, body: String): Path =
    val dir = Files.createTempDirectory("recheck runtime ")
    val file = dir.resolve(name)
    Files.writeString(file, s"#!/bin/sh\n$body\n")
    file.toFile.setExecutable(true)
    file.toFile.deleteOnExit()
    dir.toFile.deleteOnExit()
    file

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
    // V8 overflows its backtracking stack even on a linear matching.
    val overflow = validate("^(?:a|b)*$", "", Dialect.JavaScript, attack("a", 10_000_000, "!"), 10.seconds)
    assertEquals(overflow.result, RecallResult.EngineError("Maximum call stack size exceeded"))
    assert(!overflow.succeeded)

  runtimeTest("Python", "python3"):
    // Python 3.15 doubles its time for each `a`.
    val r = validate("^(a|a)*$", "", Dialect.Python(), attack("a", 31, "\u0000"))
    assertEquals(r.result, RecallResult.Timeout)
    assert(clue(r.runtime).startsWith("python 3."))
    assertEquals(
      validate("^(?:é|é)*$", "", Dialect.Python(Dialect.Python.Type.Bytes), attack("é", 31, "\u0000")).result,
      RecallResult.Timeout
    )
    assertFinish(validate("^(?:é|é)*$", "", Dialect.Python(Dialect.Python.Type.Bytes), attack("é", 3, "\u0000")))

  runtimeTest("Python: the C locale", "python3"):
    // `re.L` has no non-ASCII word byte in a UTF-8 locale, so it needs a Latin-1 locale to differ from the C locale.
    val probe = new ProcessBuilder("python3", "-I", "-c", "import re; print(re.match(rb'\\w', b'\\xe9', re.L))")
    probe.environment().put("LC_ALL", "en_US.ISO8859-1")
    assume(new String(probe.start().getInputStream.readAllBytes()).trim != "None", "no en_US.ISO8859-1 locale")
    val env = Map("RECHECK_PYTHON" -> "env LC_ALL=en_US.ISO8859-1 python3")
    val bytes = Dialect.Python(Dialect.Python.Type.Bytes)
    assertFinish(validate("^(\\w|\\w)*$", "L", bytes, attack("é", 31, "\u0000"), env = env))
    val unicode = validate("^(\\w|\\w)*$", "", Dialect.Python(), attack("é", 31, "\u0000"), env = env)
    assertEquals(unicode.result, RecallResult.Timeout)

  runtimeTest("Ruby", "ruby"):
    // Ruby 4.0's memoization makes it fast.
    val r = validate("^(a|a)*$", "", Dialect.Ruby(), attack("a", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("ruby "))
    assertFinish(validate("^(?:é|é)*$", "n", Dialect.Ruby(), attack("Ã©", 31, "\u0000")))

  runtimeTest("Ruby: the program in UTF-8", "ruby"):
    // The C locale would read a non-ASCII character of the program as an invalid US-ASCII byte.
    val env = Map("RECHECK_RUBY" -> "env LC_ALL=C ruby")
    assertFinish(validate("^(?:é|é)*$", "", Dialect.Ruby(), attack("é", 3, "\u0000"), env = env))

  runtimeTest("Ruby: the character set options", "ruby"):
    // `u` is `(?u)`, not `/u` (FIXEDENCODING), so `\w` matches `é`, and a backreference turns off the memoization.
    val pattern = "^(?:(\\w)|\\w)*\\1$"
    assertEquals(validate(pattern, "u", Dialect.Ruby(), attack("é", 31, "!")).result, RecallResult.Timeout)
    assertFinish(validate(pattern, "ua", Dialect.Ruby(), attack("é", 31, "!")))

  runtimeTest("Java", "java"):
    val r = validate("^(a|a)*$", "", Dialect.Java, attack("a", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("java "))
    // `java.util.regex` recurses on each iteration of a group, which is not an effective attack.
    val overflow = validate("^(?:a|b)*$", "", Dialect.Java, attack("a", 100000, "!"))
    assertEquals(overflow.result, RecallResult.EngineError("StackOverflowError"))
    assert(!overflow.succeeded)
    // The data of the program keeps a character out of the BMP.
    val astral = AttackPattern(Seq((UString.empty, UString.fromString("😀"), 0)), UString.fromString("!"), 3)
    assertFinish(validate("^(?:😀)*!$", "", Dialect.Java, astral))

  runtimeTest(".NET", "dotnet"):
    // The first run of `dotnet run -` builds the program, which takes some seconds.
    val r = validate("^(a|a)*$", "", Dialect.DotNet, attack("a", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith(".NET "))
    // The runs share the program, which the first run builds.
    val rs = (1 to 4).map(_ => Future(validate("^(a|a)*$", "i", Dialect.DotNet, attack("A", 3, "\u0000"), 10.seconds)))
    Await.result(Future.sequence(rs), 60.seconds).foreach(assertFinish)

  runtimeTest(".NET: the invariant culture", "dotnet"):
    // The Turkish culture matches `i` with `İ` and `I` with `ı` under `IgnoreCase`.
    val env = Map("RECHECK_DOTNET" -> "env LC_ALL=tr_TR.UTF-8 dotnet")
    def validateTurkish(c: String): RecallValidation =
      validate("^[ia]*[ib]*[ic]*$", "i", Dialect.DotNet, attack(c, 2000, "!"), env = env)
    assertFinish(validateTurkish("İ"))
    assertEquals(validateTurkish("I").result, RecallResult.Timeout)

  runtimeTest("Perl", "perl"):
    val r = validate("^(a|a)*$", "", Dialect.Perl(), attack("é", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("perl 5."))
    // A string pattern may contain `$`, which a regex literal would interpolate.
    val pattern = ParsedPattern.parse("^(?:a|a)*$x", "", Dialect.Perl(source = PerlSource.String)).toTry.get
    val string = RecallValidator.validate(pattern, attack("a", 31, ""), 1.second)(
      RecallValidator.defaultExec.get
    )
    assertFinish(string.get)

  runtimeTest("Perl: the C locale", "perl"):
    // Perl takes the locale of the environment, whose `\w` under `l` matches `é` in a UTF-8 locale.
    val env = Map("RECHECK_PERL" -> "env LC_ALL=en_US.UTF-8 perl")
    assertFinish(validate("^\\w*\\w*\\w*$", "l", Dialect.Perl(), attack("é", 3000, "\u0000"), env = env))
    val unicode = validate("^\\w*\\w*\\w*$", "u", Dialect.Perl(), attack("é", 3000, "\u0000"), env = env)
    assertEquals(unicode.result, RecallResult.Timeout)

  runtimeTest("Perl: the character set rules of utf8", "perl"):
    // The program matches `0x80..0xFF` as recheck's character sets have them.
    def check(source: String, dialect: Dialect.Perl): Unit =
      val pattern = ParsedPattern.parse(source, "", dialect).toTry.get
      def last(data: IRNodeData): CharSet = data match
        case IRNodeData.Chars(set)      => set
        case IRNodeData.Sequence(nodes) => last(nodes.last.data)
        case data                       => fail(s"Unexpected: $data")
      val modeled = (0x80 to 0xff).filter(last(pattern.node.data).contains)
      val program = RecallProgram.generate(pattern, attack("a", 1, "")).get
      val lines = program.script.linesIterator.toSeq
      val pragma = lines.filter(_ == "use utf8;")
      val regex = lines.find(_.startsWith("my $regex = ")).get
      val flag = lines.find(_.startsWith("utf8::")).get
      val loop =
        s"""for my $$b (0x80..0xFF) { my $$input = chr($$b); $flag print "$$b\\n" if $$input =~ /\\A$$regex\\z/ }"""
      val process = new ProcessBuilder("perl", "-e", (pragma ++ Seq(regex, loop)).mkString("\n")).start()
      val out = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      assertEquals(out.linesIterator.map(_.toInt).toSeq, modeled, s"$source in $dialect")
    for
      source <- Seq("\\w", "\\d", "\\s", "[[:alpha:]]", "(?i)\\xC9", "(?a)\\w", "(?u)\\w", "\\x{100}?\\w", "\\p{L}?\\w")
      utf8 <- Seq(true, false)
      perlSource <- Seq(PerlSource.Literal, PerlSource.String)
    do check(source, Dialect.Perl(utf8, perlSource))

  runtimeTest("PCRE", "php"):
    // PHP's default `pcre.backtrack_limit` stops the match.
    val r = validate("^(?:é|é)*$", "", Dialect.PCRE, attack("Ã©", 31, "\u0000"))
    assertEquals(r.result, RecallResult.EngineLimit("Backtrack limit exhausted"))
    assert(r.succeeded)
    assert(clue(r.runtime).startsWith("php "))
    assertEquals(
      validate("^(?:é|é)*$", "u", Dialect.PCRE, attack("é", 31, "\u0000")).result,
      RecallResult.EngineLimit("Backtrack limit exhausted")
    )
    assertFinish(validate("^(?:é|é)*$", "u", Dialect.PCRE, attack("é", 3, "\u0000")))

  runtimeTest("PCRE: the r modifier", "php"):
    val probe = new ProcessBuilder("php", "-r", "exit(@preg_match('/a/r', 'a') === false ? 1 : 0);").start()
    assume(probe.waitFor() == 0, "PHP before 8.4 has no r modifier")
    // `r` (PCRE2_EXTRA_CASELESS_RESTRICT) keeps `k` from matching the Kelvin sign.
    val kelvin = attack("\u212a", 31, "\u0000")
    assertEquals(
      validate("^(?:k|k)*$", "iu", Dialect.PCRE, kelvin).result,
      RecallResult.EngineLimit("Backtrack limit exhausted")
    )
    assertFinish(validate("^(?:k|k)*$", "iur", Dialect.PCRE, kelvin))

  /** Runs the standalone program in a new directory, and returns the output. */
  def runScript(script: RecallProgram.Script): String =
    val dir = Files.createTempDirectory("recheck script ")
    val file = dir.resolve(script.name)
    Files.writeString(file, script.content)
    file.toFile.deleteOnExit()
    dir.toFile.deleteOnExit()
    val process = new ProcessBuilder(script.command.split(' ')*).directory(dir.toFile).redirectErrorStream(true).start()
    process.getOutputStream.close()
    val out = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    assertEquals(process.waitFor(), 0, out)
    out

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
    runtimeTest(s"RecallProgram.script: $dialect", program):
      val pattern = ParsedPattern.parse("^(?:a|é|😀)*$", "", dialect).toTry.get
      val astral = AttackPattern(Seq((UString.fromString("é"), UString.fromString("😀a"), 0)), UString(0), 3)
      val out = runScript(RecallProgram.script(pattern, astral).get)
      val lines = out.linesIterator.map(_.takeWhile(_ != ' ')).toSeq
      assertEquals(lines.filter(_.startsWith("recheck:")), Seq("recheck:runtime", "recheck:start", "recheck:time"))

  runtimeTest("ReDoS.checkAll: Ruby", "ruby"):
    // The failed recall validation is kept, and the checking continues to the safe result.
    val ds = ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby()))
    assert(clue(ds).size >= 2)
    ds.init.foreach:
      case d: Diagnostics.Vulnerable => assertFinish(d.recallValidation.get)
      case d                         => fail(s"not vulnerable: $d")
    assert(clue(ds.last).isInstanceOf[Diagnostics.Safe])
    assertEquals(ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby())), ds.last)

  runtimeTest("ReDoS.checkAll: Python", "python3"):
    ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.Python())) match
      case List(d: Diagnostics.Vulnerable) => assertEquals(d.recallValidation.map(_.result), Some(RecallResult.Timeout))
      case ds                              => fail(s"unexpected: $ds")

  runtimeTest("ReDoS.checkAll: PCRE", "php"):
    ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.PCRE)) match
      case List(d: Diagnostics.Vulnerable) =>
        assertEquals(d.recallValidation.map(_.result), Some(RecallResult.EngineLimit("Backtrack limit exhausted")))
      case ds => fail(s"unexpected: $ds")

  runtimeTest("RECHECK_RUBY", "ruby"):
    // The wrapper's runtime line comes first, and it shows the arguments.
    val ruby = script("my ruby", s"echo recheck:runtime wrapped \"$$@\"; exec '${which("ruby")}' \"$$@\"")
    val env = Map("RECHECK_RUBY" -> s"'$ruby' --disable-gems")
    val r = validate("^(a|a)*$", "", Dialect.Ruby(), attack("a", 3, "\u0000"), env = env)
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
    val r = validate("a", "", Dialect.Java, attack("a", 3, ""), env = Map("RECHECK_JAVA" -> s"\"$java\""))
    assertEquals(r, RecallValidation("fake --source 17", 1.second, RecallResult.EngineError("StackOverflowError")))
    interceptMessage[UnexpectedException]("`sh -c 'exit 3' --source 17 RecheckRecall.java` exited with 3"):
      validate("a", "", Dialect.Java, attack("a", 3, ""), env = Map("RECHECK_JAVA" -> "sh -c 'exit 3'"))

  test("ReDoS.iterator: environment variables"):
    val node = script("node", "echo recheck:runtime fake; echo recheck:start; echo recheck:time 0")
    val ds = ReDoS
      .iterator("^(a|a)*$", "", params.copy(checker = Checker.Automaton), None, env = Map("RECHECK_NODE" -> s"'$node'"))
      .toList
    assertEquals(
      ds.collect { case d: Diagnostics.Vulnerable => d.recallValidation },
      List(Some(RecallValidation("fake", 1.second, RecallResult.Finish(0.millis))))
    )
    assert(clue(ds.last).isInstanceOf[Diagnostics.Safe])

  test("RecallValidator.validate: the deadline of the context"):
    val pattern = ParsedPattern.parse("foo", "", Dialect.JavaScript).toTry.get
    intercept[TimeoutException]:
      RecallValidator.validate(pattern, attack("a", 1, ""), 50.millis) { (_, _, _) =>
        Thread.sleep(100)
        ExecResult.Timeout("")
      }(using Context(timeout = 50.millis))
