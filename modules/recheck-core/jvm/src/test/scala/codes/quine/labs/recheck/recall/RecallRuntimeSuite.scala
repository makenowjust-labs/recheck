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
import codes.quine.labs.recheck.RuntimeHelper
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRNodeData

class RecallRuntimeSuite extends munit.FunSuite with RuntimeHelper:

  override val munitTimeout: Duration = 60.seconds

  /** A default context. */
  given ctx: Context = Context()

  val params: Parameters =
    Parameters(recallTimeout = 1.second, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)

  /** Returns the attack string `s.repeat(n) + t`. */
  def attack(s: String, n: Int, t: String): AttackPattern =
    AttackPattern(Seq((UString.empty, UnitMode.CodePoint.decode(s), 0)), UnitMode.CodePoint.decode(t), n)

  def validate(
      source: String,
      flags: String,
      dialect: Dialect,
      attack: AttackPattern,
      timeout: Duration = 1.second
  ): RecallValidation =
    val mode = ReDoS.parse(source, flags, dialect).mode
    RecallValidator.validate(source, flags, dialect, mode, attack, timeout).get

  /** Runs the recall program as [[RecallValidator.validate]] does, where the environment variables replace the commands
    * of the runtimes.
    */
  def validateIn(env: Map[String, String])(
      source: String,
      flags: String,
      dialect: Dialect,
      attack: AttackPattern
  ): RecallValidation =
    val mode = ReDoS.parse(source, flags, dialect).mode
    val program = RecallProgram.generate(source, flags, dialect, mode, attack).toOption.get
    val executor = program.executor
    val output = executor.exec(program.script, Some(1.second), RecallProgram.StartLine, env)
    RecallValidator.result(executor.runtime(env), executor.commandLine(env), 1.second, output)

  /** Returns the path of the program in `PATH`. */
  def which(program: String): Path =
    sys.env("PATH").split(java.io.File.pathSeparatorChar).map(Paths.get(_, program)).find(Files.isExecutable).get

  /** Creates a shell script in a new directory with a space in its name. */
  def script(name: String, body: String): Path =
    val file = createFile("recheck runtime ", name, s"#!/bin/sh\n$body\n")
    file.toFile.setExecutable(true)
    file

  def assertFinish(r: RecallValidation)(using munit.Location): Unit =
    assert(clue(r).result.isInstanceOf[RecallResult.Finish])
    assert(!r.isSucceeded)

  runtimeTest("JavaScript", "node"):
    val r = validate("^(a|a)*$", "", Dialect.JavaScript, attack("a", 31, "\u0000"))
    assertEquals(r.result, RecallResult.Timeout)
    assert(clue(r.runtime).startsWith("node v"))
    assertFinish(validate("^(a|a)*$", "", Dialect.JavaScript, attack("a", 3, "\u0000")))
    // V8 overflows its backtracking stack even on a linear matching.
    val overflow = validate("^(?:a|b)*$", "", Dialect.JavaScript, attack("a", 10_000_000, "!"), 10.seconds)
    assertEquals(overflow.result, RecallResult.EngineError("Maximum call stack size exceeded"))
    assert(!overflow.isSucceeded)

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
    assert(new String(probe.start().getInputStream.readAllBytes()).trim != "None", "no en_US.ISO8859-1 locale")
    val env = Map("RECHECK_PYTHON" -> "env LC_ALL=en_US.ISO8859-1 python3")
    val bytes = Dialect.Python(Dialect.Python.Type.Bytes)
    assertFinish(validateIn(env)("^(\\w|\\w)*$", "L", bytes, attack("é", 31, "\u0000")))
    val unicode = validateIn(env)("^(\\w|\\w)*$", "", Dialect.Python(), attack("é", 31, "\u0000"))
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
    assertFinish(validateIn(env)("^(?:é|é)*$", "", Dialect.Ruby(), attack("é", 3, "\u0000")))

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
    assert(!overflow.isSucceeded)
    // The data of the program keeps a character out of the BMP.
    val astral =
      AttackPattern(Seq((UString.empty, UnitMode.CodePoint.decode("😀"), 0)), UnitMode.CodePoint.decode("!"), 3)
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
      validateIn(env)("^[ia]*[ib]*[ic]*$", "i", Dialect.DotNet, attack(c, 2000, "!"))

    assertFinish(validateTurkish("İ"))
    assertEquals(validateTurkish("I").result, RecallResult.Timeout)

  runtimeTest("Perl", "perl"):
    val r = validate("^(a|a)*$", "", Dialect.Perl(), attack("é", 31, "\u0000"))
    assertFinish(r)
    assert(clue(r.runtime).startsWith("perl 5."))
    // A string pattern may contain `$`, which a regex literal would interpolate.
    val perlString = Dialect.Perl(source = PerlSource.String)
    val string =
      RecallValidator.validate("^(?:a|a)*$x", "", perlString, UnitMode.CodePoint, attack("a", 31, ""), 1.second)
    assertFinish(string.get)

  runtimeTest("Perl: the C locale", "perl"):
    // Perl takes the locale of the environment, whose `\w` under `l` matches `é` in a UTF-8 locale.
    val env = Map("RECHECK_PERL" -> "env LC_ALL=en_US.UTF-8 perl")
    assertFinish(validateIn(env)("^\\w*\\w*\\w*$", "l", Dialect.Perl(), attack("é", 3000, "\u0000")))
    val unicode = validateIn(env)("^\\w*\\w*\\w*$", "u", Dialect.Perl(), attack("é", 3000, "\u0000"))
    assertEquals(unicode.result, RecallResult.Timeout)

  runtimeTest("Perl: the character set rules of utf8", "perl"):
    // The program matches `0x80..0xFF` as recheck's character sets have them.
    def check(source: String, dialect: Dialect.Perl): Unit =
      val ir = ReDoS.parse(source, "", dialect)
      def last(data: IRNodeData): CharSet = data match
        case IRNodeData.Chars(set)      => set
        case IRNodeData.Sequence(nodes) => last(nodes.last.data)
        case data                       => fail(s"Unexpected: $data")

      val modeled = (0x80 to 0xff).filter(last(ir.node.data).contains)
      val program = RecallProgram.generate(source, "", dialect, ir.mode, attack("a", 1, "")).toOption.get
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
    assert(r.isSucceeded)
    assert(clue(r.runtime).startsWith("php "))
    assertEquals(
      validate("^(?:é|é)*$", "u", Dialect.PCRE, attack("é", 31, "\u0000")).result,
      RecallResult.EngineLimit("Backtrack limit exhausted")
    )
    assertFinish(validate("^(?:é|é)*$", "u", Dialect.PCRE, attack("é", 3, "\u0000")))

  runtimeTest("PCRE: the r modifier", "php"):
    // `r` (PCRE2_EXTRA_CASELESS_RESTRICT) keeps `k` from matching the Kelvin sign.
    val kelvin = attack("\u212a", 31, "\u0000")
    assertEquals(
      validate("^(?:k|k)*$", "iu", Dialect.PCRE, kelvin).result,
      RecallResult.EngineLimit("Backtrack limit exhausted")
    )
    assertFinish(validate("^(?:k|k)*$", "iur", Dialect.PCRE, kelvin))

  /** Runs the standalone program in a new directory, and returns the output. */
  def runScript(script: RecallProgram.Script): String =
    val file = createFile("recheck script ", script.name, script.content)
    run(script.command.split(' ').toSeq, dir = Some(file.getParent))

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
      val source = "^(?:a|é|😀)*$"
      val mode = ReDoS.parse(source, "", dialect).mode
      def units(s: String): UString = mode.fromCodePoints(UnitMode.CodePoint.decode(s))

      val astral = AttackPattern(Seq((units("é"), units("😀a"), 0)), UString(0), 3)
      val out = runScript(RecallProgram.script(source, "", dialect, mode, astral).toOption.get)
      val lines = out.linesIterator.map(_.takeWhile(_ != ' ')).toSeq
      assertEquals(lines.filter(_.startsWith("recheck:")), Seq("recheck:runtime", "recheck:start", "recheck:time"))

  runtimeTest("ReDoS.checkAll: Ruby", "ruby"):
    // The failed recall validation is kept, and the checking continues to the safe result.
    val ds = ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby())).toList
    assert(clue(ds).size >= 2)
    ds.init.foreach:
      case d: Diagnostics.Vulnerable => assertFinish(d.recallValidation.get)
      case d                         => fail(s"not vulnerable: $d")
    assert(clue(ds.last).isInstanceOf[Diagnostics.Safe])
    assertEquals(ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby())), ds.last)

  runtimeTest("ReDoS.checkAll: Python", "python3"):
    ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.Python())).toList match
      case List(d: Diagnostics.Vulnerable) => assertEquals(d.recallValidation.map(_.result), Some(RecallResult.Timeout))
      case ds                              => fail(s"unexpected: $ds")

  runtimeTest("ReDoS.checkAll: PCRE", "php"):
    ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.PCRE)).toList match
      case List(d: Diagnostics.Vulnerable) =>
        assertEquals(d.recallValidation.map(_.result), Some(RecallResult.EngineLimit("Backtrack limit exhausted")))
      case ds => fail(s"unexpected: $ds")

  /** Returns the attack string `"\uD800\uDC00".repeat(n) + "!"`, which repeats a high surrogate followed by a low
    * surrogate.
    */
  def surrogatePairs(n: Int): AttackPattern =
    AttackPattern(Seq((UString.empty, UString(0xd800, 0xdc00), 0)), UString('!'), n)

  val surrogates: String = "^(?:[\\uD800-\\uDBFF][\\uDC00-\\uDFFF]|[\\uD800-\\uDBFF]|[\\uDC00-\\uDFFF])*$"

  runtimeTest("Surrogates: JavaScript", "node"):
    // A high surrogate followed by a low surrogate is one character with `u`, which the alternatives cannot match.
    assertFinish(validate(surrogates, "u", Dialect.JavaScript, surrogatePairs(31)))
    assertEquals(validate(surrogates, "", Dialect.JavaScript, surrogatePairs(31)).result, RecallResult.Timeout)
    assertEquals(ReDoS.check(surrogates, "u", params).getClass, classOf[Diagnostics.Safe])

  runtimeTest("Surrogates: Python", "python3"):
    assertEquals(validate(surrogates, "", Dialect.Python(), surrogatePairs(31)).result, RecallResult.Timeout)
    ReDoS.check(surrogates, "", params.copy(dialect = Dialect.Python())) match
      case d: Diagnostics.Vulnerable => assertEquals(d.recallValidation.map(_.result), Some(RecallResult.Timeout))
      case d                         => fail(s"not vulnerable: $d")

  runtimeTest("Surrogates: PCRE", "php"):
    // A subject with a surrogate is invalid UTF-8, which the attack does not take.
    val source = "^(?:[^\\x00-\\x{D7FF}]|[^\\x00-\\x{D7FF}])*$"
    ReDoS.check(source, "u", params.copy(dialect = Dialect.PCRE)) match
      case d: Diagnostics.Vulnerable =>
        assert(!d.attack.asUString.iterator.exists(c => 0xd800 <= c && c <= 0xdfff), d.attack)
        assertEquals(d.recallValidation.map(_.result), Some(RecallResult.EngineLimit("Backtrack limit exhausted")))
      case d => fail(s"not vulnerable: $d")

  runtimeTest("RECHECK_RUBY", "ruby"):
    // The wrapper's runtime line comes first, and it shows the arguments.
    val ruby = script("my ruby", s"echo recheck:runtime wrapped \"$$@\"; exec '${which("ruby")}' \"$$@\"")
    val env = Map("RECHECK_RUBY" -> s"'$ruby' --disable-gems")
    val r = validateIn(env)("^(a|a)*$", "", Dialect.Ruby(), attack("a", 3, "\u0000"))
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
    val r = validateIn(Map("RECHECK_JAVA" -> s"\"$java\""))("a", "", Dialect.Java, attack("a", 3, ""))
    assertEquals(r, RecallValidation("fake --source 17", 1.second, RecallResult.EngineError("StackOverflowError")))
    interceptMessage[UnexpectedException]("`sh -c 'exit 3' --source 17 RecheckRecall.java` exited with 3"):
      validateIn(Map("RECHECK_JAVA" -> "sh -c 'exit 3'"))("a", "", Dialect.Java, attack("a", 3, ""))

  runtimeTest("RecallValidator.validate: the deadline of the context", "node"):
    given Context = Context(timeout = 50.millis)
    val exponential = attack("a", 31, "\u0000")
    intercept[TimeoutException]:
      RecallValidator.validate("^(a|a)*$", "", Dialect.JavaScript, UnitMode.UTF16, exponential, 10.seconds)
