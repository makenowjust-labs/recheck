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
          Parameters(
            dialect = Dialect.Perl,
            encoding = Some("bytes"),
            perlSource = PerlSource.String,
            perlUtf8 = Some(true)
          )
        )
      )
    )
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
    assertEquals(Main.command.parse(Seq("agent", "--thread-size=2")), Right(Main.BatchAction(2)))
    assertEquals(Main.command.parse(Seq("agent")), Right(Main.BatchAction(sys.runtime.availableProcessors())))

  test("Main#run"):

    assertEquals(
      runMain(Seq.empty),
      ("", "Missing expected command (agent), or positional argument!\n\n" + Main.command.showHelp + "\n", 2)
    )
    assertEquals(runMain(Seq("/foo/"))._3, 0)
    assertEquals(runMain(Seq("/foo/", "--enable-log"))._1.linesIterator.size, 8)
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
