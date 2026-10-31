package codes.quine.labs.recheck.cli

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.InputStreamReader
import java.io.SequenceInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.util.concurrent.Semaphore
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.SECONDS

import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.codec.encodeDiagnostics
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource

class MainSuite extends munit.FunSuite:
  private class ExitException(val status: Int) extends SecurityException

  def runMain(
      args: Seq[String],
      newIn: InputStream = InputStream.nullInputStream(),
      newOut: ByteArrayOutputStream = new ByteArrayOutputStream()
  ): (String, String, Int) =
    val newErr = new ByteArrayOutputStream()

    val main = new Main:
      override def exit(exitCode: Int): Unit = throw new ExitException(exitCode)

    // The input is read as UTF-8 like `Main.main`.
    val newInReader = new InputStreamReader(newIn, StandardCharsets.UTF_8)
    val status =
      try
        Console.withIn(newInReader)(Console.withOut(newOut)(Console.withErr(newErr)(main.run(args.toArray))))
        0
      catch case ex: ExitException => ex.status
    (newOut.toString(StandardCharsets.UTF_8), newErr.toString(StandardCharsets.UTF_8), status)

  test("Main.command"):
    assert(clue(Main.command.parse(Seq.empty)).isLeft)
    assertEquals(Main.command.parse(Seq("/foo/")), Right(Main.CheckAction(InputPattern("foo", ""), Parameters())))
    assertEquals(
      Main.command.parse(
        Seq("/foo/", "--dialect=perl", "--encoding=bytes", "--perl-source=string", "--perl-utf8=true")
      ),
      Right(
        Main.CheckAction(
          InputPattern("foo", ""),
          Parameters(dialect = Dialect.Perl(Dialect.Perl.Encoding.Bytes, PerlSource.String, Some(true)))
        )
      )
    )
    assertEquals(
      Main.command.parse(Seq("/foo/", "--dialect=ruby", "--encoding=ASCII-8BIT")),
      Right(
        Main.CheckAction(InputPattern("foo", ""), Parameters(dialect = Dialect.Ruby(Dialect.Ruby.Encoding.Ascii8Bit)))
      )
    )
    assertEquals(
      Main.command.parse(Seq("/foo/", "--dialect=python")),
      Right(Main.CheckAction(InputPattern("foo", ""), Parameters(dialect = Dialect.Python())))
    )
    for (args, message) <- Seq(
        Seq("--encoding=str") -> "--encoding is not an option of javascript",
        Seq("--dialect=python", "--perl-source=string") -> "--perl-source is not an option of python",
        Seq("--dialect=ruby", "--perl-utf8=true") -> "--perl-utf8 is not an option of ruby",
        Seq("--dialect=ruby", "--encoding=str") -> "unknown encoding of ruby: str"
      )
    do
      val result = Main.command.parse("/foo/" +: args)
      assert(clue(result).left.exists(_.errors == List(message)))
    assertEquals(
      Main.command.parse(Seq("/foo/", "--max-recall-failures=3")),
      Right(Main.CheckAction(InputPattern("foo", ""), Parameters(maxRecallFailures = 3)))
    )
    assertEquals(
      Main.command.parse(Seq("/foo/", "--max-degree=2", "--timeout=1s")),
      Right(Main.CheckAction(InputPattern("foo", ""), Parameters(maxDegree = 2, timeout = Duration(1, SECONDS))))
    )
    assertEquals(
      Main.command.parse(Seq("--all", "/foo/")),
      Right(Main.CheckAction(InputPattern("foo", ""), Parameters(), all = true))
    )
    assertEquals(
      Main.command.parse(Seq("--json", "/foo/")),
      Right(Main.CheckAction(InputPattern("foo", ""), Parameters(), json = true))
    )
    assertEquals(Main.command.parse(Seq("generate-recall-script", "-")), Right(Main.RecallScriptAction("-")))
    assert(clue(Main.command.parse(Seq("generate-recall-script"))).isLeft)
    assertEquals(Main.command.parse(Seq("agent", "--thread-size=2")), Right(Main.BatchAction(2)))
    assertEquals(Main.command.parse(Seq("agent")), Right(Main.BatchAction(sys.runtime.availableProcessors())))

  test("Main#run"):

    assertEquals(
      runMain(Seq.empty),
      (
        "",
        "Missing expected command (agent or generate-recall-script), or positional argument!\n\n" + Main.command.showHelp + "\n",
        2
      )
    )
    assertEquals(runMain(Seq("/foo/"))._3, 0)
    assertEquals(runMain(Seq("/foo/", "--enable-log"))._1.linesIterator.size, 9)
    assertEquals(
      runMain(Seq("/foo/", "--dialect=perl", "--perl-source=string"))._1.linesIterator.toSeq(1),
      "Dialect   : perl (encoding: characters, source: string, utf8: false)"
    )
    assertEquals(runMain(Seq("/(a|a)*$/"))._3, 1)
    assertEquals(runMain(Seq("agent")), ("", "", 0))

    // The agent reads/writes UTF-8 regardless of the platform's default charset.
    // See #1910.
    val request =
      s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":1,"method":"check","params":{"source":"“[^”]*”","flags":"u","params":{"checker":"automaton"}}}"""
    // The input is closed after the response is written.
    // Otherwise, the check would be canceled on closing the input before it finishes.
    val responded = new Semaphore(0)
    val agentIn = new SequenceInputStream(
      new ByteArrayInputStream(s"$request\n".getBytes(StandardCharsets.UTF_8)),
      new InputStream:
        def read(): Int =
          responded.tryAcquire(30, TimeUnit.SECONDS)
          -1
    )
    val agentOut = new ByteArrayOutputStream:
      override def flush(): Unit = responded.release()
    val (out, err, status) = runMain(Seq("agent"), agentIn, agentOut)
    assertEquals(err, "")
    assertEquals(status, 0)
    assert(clue(out).contains(""""source":"“[^”]*”""""))
    assert(clue(out).contains(""""status":"vulnerable""""))
    assert(clue(out).contains(""""summary":"2nd degree polynomial""""))

  test("Main#run: --all"):
    // Without recall validation, it shows the same as `check`.
    assertEquals(runMain(Seq("--all", "/(a|a)*$/")), runMain(Seq("/(a|a)*$/")))
    assertEquals(runMain(Seq("--all", "/foo/"))._3, 0)

  test("Main#run: --json"):
    val params = Parameters(checker = Checker.Automaton)
    val (out, err, status) = runMain(Seq("--json", "--checker=automaton", "/(a|a)*$/"))
    assertEquals((err, status), ("", 1))
    assertEquals(parse(out), Right(encodeDiagnostics(ReDoS.check("(a|a)*$", "", params))))
    val (all, _, allStatus) = runMain(Seq("--all", "--json", "--checker=automaton", "/foo/"))
    assertEquals(allStatus, 0)
    assertEquals(parse(all), Right(Json.arr(encodeDiagnostics(ReDoS.check("foo", "", params)))))

    // The output is the input of `generate-recall-script`.
    val stdin = new ByteArrayInputStream(out.getBytes(StandardCharsets.UTF_8))
    val (script, scriptErr, scriptStatus) = runMain(Seq("generate-recall-script", "-"), stdin)
    assertEquals((scriptErr, scriptStatus), ("", 0))
    assert(clue(script).startsWith("// Run: node recall.cjs\n"))

    // The diagnostics have the dialect with its settings, which `generate-recall-script` reads.
    def roundTrip(args: String*): String =
      val (out, err, status) = runMain(Seq("--json", "--checker=automaton") ++ args)
      assertEquals((err, status), ("", 1))
      val stdin = new ByteArrayInputStream(out.getBytes(StandardCharsets.UTF_8))
      val (script, scriptErr, scriptStatus) = runMain(Seq("generate-recall-script", "-"), stdin)
      assertEquals((scriptErr, scriptStatus), ("", 0))
      script
    val python = roundTrip("--dialect=python", "--encoding=bytes", "/^(a|a)*$/")
    assert(clue(python).startsWith("# Run: python3 -I recall.py\n"))
    assert(clue(python).contains("string = b'a' * "))
    val perl = roundTrip("--dialect=perl", "--perl-source=string", "--perl-utf8=true", "/^(a|a)*$x/")
    assert(clue(perl).startsWith("# Run: perl recall.pl\n"))
    assert(clue(perl).contains("my $pattern = \"^(a|a)*\\$x\"; utf8::upgrade($pattern);"))

  test("Main#run: --all with failed recall validation"):
    assume(
      sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparatorChar).exists { dir =>
        dir.nonEmpty && Files.isExecutable(Paths.get(dir, "ruby"))
      },
      "`ruby` is not in PATH"
    )
    // Ruby's memoization makes the attacks fail, and the last diagnostics is safe.
    val (out, err, status) = runMain(Seq("--all", "--dialect=ruby", "--recall-timeout=1s", "/^(a|a)*$/"))
    assertEquals(err, "")
    assertEquals(status, 0)
    val blocks = out.stripSuffix("\n").split("\n\n").toSeq
    assert(clue(blocks).sizeIs >= 2)
    blocks.init.foreach(b => assert(clue(b).contains("Status       : vulnerable")))
    assert(clue(blocks.last).contains("Status    : safe"))

  test("Main#run: generate-recall-script"):
    def diagnostics(source: String, flags: String, params: Parameters = Parameters()): Json =
      encodeDiagnostics(ReDoS.check(source, flags, params))

    def file(json: String): String =
      val path = Files.createTempFile("recheck diagnostics ", ".json")
      Files.writeString(path, json)
      path.toFile.deleteOnExit()
      path.toString

    val js = diagnostics("^(a|a)*$", "")
    val (out, err, status) = runMain(Seq("generate-recall-script", file(js.noSpaces)))
    assertEquals((err, status), ("", 0))
    assert(clue(out).startsWith("// Run: node recall.cjs\nconst "))
    assert(clue(out).contains("const input = 'a'.repeat("))
    // `-` reads the standard input as UTF-8.
    val stdin = new ByteArrayInputStream(diagnostics("^(é|é)*$", "u").noSpaces.getBytes(StandardCharsets.UTF_8))
    val (utf8, _, _) = runMain(Seq("generate-recall-script", "-"), stdin)
    assert(clue(utf8).contains("const re = new RegExp('^(é|é)*$', 'u');"))

    // The dialect comes from the `dialect` field.
    val python = diagnostics("^(a|a)*$", "", Parameters(dialect = Dialect.Python()))
    val bytes = python.deepMerge(Json.obj("dialect" := Json.obj("name" := "python", "encoding" := "bytes")))
    val (pythonOut, _, _) = runMain(Seq("generate-recall-script", file(bytes.noSpaces)))
    assert(clue(pythonOut).startsWith("# Run: python3 -I recall.py\n"))
    assert(clue(pythonOut).contains("string = b'a' * "))
    val java = diagnostics("^(a|a)*$", "", Parameters(dialect = Dialect.Java)).deepMerge(Json.obj("dialect" := "java"))
    val (javaOut, _, _) = runMain(Seq("generate-recall-script", file(java.noSpaces)))
    assert(clue(javaOut).startsWith("// Run: java --source 17 recall.java\n"))

    def error(json: String): (String, String, Int) = runMain(Seq("generate-recall-script", file(json)))
    assertEquals(error("{"), ("", "Invalid JSON: exhausted input\n", 1))
    assertEquals(
      error(diagnostics("foo", "").noSpaces),
      ("", "The diagnostics is not vulnerable: safe\n", 1)
    )
    assertEquals(
      error(js.mapObject(_.remove("attack")).noSpaces),
      ("", "Invalid diagnostics: DecodingFailure at .attack: Missing required field\n", 1)
    )
    assertEquals(
      error(js.mapObject(_.add("source", "(".asJson)).noSpaces)._2,
      "Invalid pattern: Unclosed group at 1\n"
    )
    assertEquals(
      error(js.mapObject(_.add("dialect", "cobol".asJson)).noSpaces),
      ("", "Invalid diagnostics: DecodingFailure at .dialect: Unknown dialect: cobol\n", 1)
    )
    assertEquals(
      error(js.mapObject(_.add("dialect", Json.obj("name" := "ruby", "encoding" := "bytes"))).noSpaces),
      ("", "Invalid diagnostics: DecodingFailure at .dialect: Unknown encoding of ruby: bytes\n", 1)
    )
    assertEquals(
      error(js.mapObject(_.remove("dialect")).noSpaces),
      ("", "Invalid diagnostics: DecodingFailure at .dialect: Missing required field\n", 1)
    )
    val perl = js.mapObject(_.add("source", "^(a|a)*$x".asJson).add("dialect", "perl".asJson))
    assertEquals(
      error(perl.noSpaces),
      (
        "",
        "The pattern cannot be a Perl regex literal, which would interpolate a variable, run code, " +
          "or be UTF-8 against `utf8`\n",
        1
      )
    )
    val perlString = perl.mapObject(_.add("dialect", Json.obj("name" := "perl", "source" := "string")))
    assert(clue(error(perlString.noSpaces)._1).contains("my $pattern = \"^(a|a)*\\$x\";"))
    val missing = runMain(Seq("generate-recall-script", "no such file.json"))
    assertEquals(missing, ("", "No such file: no such file.json\n", 1))
