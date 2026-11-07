package codes.quine.labs.recheck.exec

import java.nio.file.Files
import java.nio.file.Paths
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import codes.quine.labs.recheck.common.CancelException
import codes.quine.labs.recheck.common.CancellationTokenSource
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException

class PlatformSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  def sh(script: String, timeout: Option[FiniteDuration])(using Context): ExecResult =
    Platform.run("sh", Seq.empty, None, Map.empty, None, script, timeout, "start")

  test("Platform.run"):
    assertEquals(sh("echo foo", None), ExecResult.Exit(0, "foo\n", ""))
    assertEquals(sh("echo bar >&2; exit 1", None), ExecResult.Exit(1, "", "bar\n"))
    assertEquals(sh("echo é", None), ExecResult.Exit(0, "é\n", ""))
    assertEquals(
      Platform.run("sh", Seq("-c", "echo $FOO"), None, Map("FOO" -> "foo"), None, "", None, "start"),
      ExecResult.Exit(0, "foo\n", "")
    )

  test("Platform.run: timeout"):
    assertEquals(sh("echo start; echo foo; sleep 5", Some(100.millis)), ExecResult.Timeout("start\nfoo\n"))

  test("Platform.run: marker"):
    // The startup before the marker does not count toward the timeout.
    assertEquals(sh("sleep 0.5; echo start; echo end", Some(200.millis)), ExecResult.Exit(0, "start\nend\n", ""))
    assertEquals(sh("echo start; sleep 5", Some(200.millis)), ExecResult.Timeout("start\n"))
    // A process exiting without the marker is just finished.
    assertEquals(sh("echo foo", Some(200.millis)), ExecResult.Exit(0, "foo\n", ""))

  test("Platform.run: interruption"):
    intercept[TimeoutException]:
      sh("sleep 5", None)(using Context(timeout = 200.millis))
    val source = new CancellationTokenSource
    source.cancel()
    intercept[CancelException]:
      sh("sleep 5", None)(using Context(token = Some(source.token)))
    // A descendant holding the standard output after the process exits does not keep it waiting.
    // The process exits after the reader blocks, since the JDK ends the stream at the exit unless a read is blocked.
    intercept[TimeoutException]:
      sh("sleep 5 & sleep 0.1", None)(using Context(timeout = 200.millis))

  test("Platform.run: missing command"):
    intercept[UnexpectedException]:
      Platform.run("recheck-missing-command", Seq.empty, None, Map.empty, None, "", None, "start")

  test("Platform.run: file"):
    val file = SourceFile("recheck-test.sh", "echo \"$# $(cat)\"")
    def run(input: String): ExecResult =
      Platform.run("sh", Seq("-e"), None, Map.empty, Some(file), input, None, "start")

    try
      assertEquals(run("foo"), ExecResult.Exit(0, "0 foo\n", ""))
      assertEquals(run("bar"), ExecResult.Exit(0, "0 bar\n", ""))
    finally Files.deleteIfExists(SourceDirectory.path(file))

  test("Platform.run: a file whose first run fails"):
    // The content is unique to this test, so that no other run marks the same file.
    val file = SourceFile("recheck-test.sh", s"exit 3 # ${System.nanoTime()}")
    val path = SourceDirectory.path(file)
    try
      assertEquals(
        Platform.run("sh", Seq.empty, None, Map.empty, Some(file), "", None, "start"),
        ExecResult.Exit(3, "", "")
      )
      // The next run is a first run again.
      assert(SourceDirectory.firstRunLock(path).isDefined)
    finally Files.deleteIfExists(path)

  test("Platform.run: a process exiting before reading the input"):
    val input = "a" * 1000000
    assertEquals(
      Platform.run("sh", Seq("-c", "exit 0"), None, Map.empty, None, input, None, "start"),
      ExecResult.Exit(0, "", "")
    )

  test("Platform.run: shell"):
    val dir = Files.createTempDirectory("recheck exec ")
    val runtime = dir.resolve("my runtime")
    try
      Files.writeString(runtime, "#!/bin/sh\necho \"$# $*\"\n")
      runtime.toFile.setExecutable(true)
      val replacement = Some(s"'$runtime' --flag")
      assertEquals(
        Platform.run("runtime", Seq("-", "a b"), replacement, Map.empty, None, "", None, "start"),
        ExecResult.Exit(0, "3 --flag - a b\n", "")
      )
    finally
      Files.deleteIfExists(runtime)
      Files.deleteIfExists(dir)

  test("Platform.run: the working directory"):
    assertEquals(sh("pwd -P", None), ExecResult.Exit(0, s"${SourceDirectory.directory.toRealPath()}\n", ""))

  test("Platform.compile"):
    // A fake compiler logs its arguments, prints its version for `-v`, and otherwise copies the program, which is a
    // shell script, to the binary.
    val dir = Files.createTempDirectory("recheck compile ")
    val compiler = dir.resolve("my compiler")
    val log = dir.resolve("log")
    Files.writeString(
      compiler,
      s"#!/bin/sh\necho \"$$*\" >> '$log'\nfor a in \"$$@\"; do\n  if [ \"$$a\" = -v ]; then echo fake 1.0; exit 0; fi\n  if [ \"$$prev\" = -o ]; then out=\"$$a\"; fi\n  case \"$$a\" in *.sh) src=\"$$a\" ;; esac\n  prev=\"$$a\"\ndone\ncp \"$$src\" \"$$out\"\nchmod +x \"$$out\"\n"
    )
    compiler.toFile.setExecutable(true)
    val program = SourceFile("recheck-test.sh", s"#!/bin/sh\necho \"$$(cat)\" # ${System.nanoTime()}\n")
    val line = s"'$compiler' -O"
    try
      val binary = Platform.compile("cc", Some(line), program, Seq("-v"), Seq("-lfoo"))
      assertEquals(Paths.get(binary), SourceDirectory.binaryPath(program, Seq(line, "-lfoo"), "fake 1.0\n"))
      assertEquals(
        Platform.run(binary, Seq.empty, None, Map.empty, None, "input", None, "start"),
        ExecResult.Exit(0, "input\n", "")
      )
      // The binary of the same source and command line is compiled once, and the version is printed once.
      assertEquals(Platform.compile("cc", Some(line), program, Seq("-v"), Seq("-lfoo")), binary)
      val Seq(version, args) = Files.readAllLines(log).asScala.toSeq: @unchecked
      assertEquals(version, "-O -v")
      assert(clue(args).matches("-O .*/recheck-test-[0-9a-f]{16}\\.sh -o .*/recheck-test-[0-9a-f]{16} -lfoo"))
      // The command runs without the replacement.
      val direct = Platform.compile(compiler.toString, None, program, Seq("-v"), Seq("-O", "-lfoo"))
      assertEquals(Files.readAllLines(log).size, 4)
      Files.deleteIfExists(Paths.get(direct))
      // A failed command printing the version tells its output.
      val failing = Some("sh -c 'echo bad; echo worse >&2; exit 1'")
      interceptMessage[UnexpectedException]("cannot run `sh -c 'echo bad; echo worse >&2; exit 1' -v`:\nbad\nworse"):
        Platform.compile("cc", failing, program, Seq("-v"), Seq.empty)
      // A command printing no version fails too.
      interceptMessage[UnexpectedException]("cannot run `true -v`: no output"):
        Platform.compile("cc", Some("true"), program, Seq("-v"), Seq.empty)
      // A failed compilation tells the output of the compiler.
      val failingCompilation = Some("sh -c 'echo bad; echo worse >&2; test \"$1\" = -v' sh")
      interceptMessage[UnexpectedException](
        "cannot compile `sh -c 'echo bad; echo worse >&2; test \"$1\" = -v' sh recheck-test.sh -o recheck-test`:\nbad\nworse"
      ):
        Platform.compile("cc", failingCompilation, program, Seq("-v"), Seq.empty)
      Files.deleteIfExists(Paths.get(binary))
    finally
      Files.deleteIfExists(SourceDirectory.path(program))
      Files.deleteIfExists(compiler)
      Files.deleteIfExists(log)
      Files.deleteIfExists(dir)
