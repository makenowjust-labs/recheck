package codes.quine.labs.recheck.exec

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Comparator
import java.util.concurrent.CountDownLatch
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import codes.quine.labs.recheck.context.CancellationTokenSource
import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.exception.CancelException
import codes.quine.labs.recheck.exception.TimeoutException
import codes.quine.labs.recheck.exception.UnexpectedException

class PlatformSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  def sh(script: String, timeout: Option[FiniteDuration])(using Context): ExecResult =
    Platform.run("sh", Seq.empty, None, Map.empty, script, timeout, "start")

  test("Platform.run"):
    assertEquals(sh("echo foo", None), ExecResult.Exit(0, "foo\n", ""))
    assertEquals(sh("echo bar >&2; exit 1", None), ExecResult.Exit(1, "", "bar\n"))
    assertEquals(sh("echo é", None), ExecResult.Exit(0, "é\n", ""))
    assertEquals(
      Platform.run("sh", Seq("-c", "echo $FOO"), None, Map("FOO" -> "foo"), "", None, "start"),
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
      Platform.run("recheck-missing-command", Seq.empty, None, Map.empty, "", None, "start")

  test("Platform.run: a process exiting before reading the input"):
    val input = "a" * 1000000
    assertEquals(
      Platform.run("sh", Seq("-c", "exit 0"), None, Map.empty, input, None, "start"),
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
        Platform.run("runtime", Seq("-", "a b"), replacement, Map.empty, "", None, "start"),
        ExecResult.Exit(0, "3 --flag - a b\n", "")
      )
    finally
      Files.deleteIfExists(runtime)
      Files.deleteIfExists(dir)

  test("Platform.run: the working directory"):
    assertEquals(sh("pwd -P", None), ExecResult.Exit(0, s"${SourceDirectory.directory.toRealPath()}\n", ""))

  /** Deletes the file or the directory with its contents. */
  def delete(path: Path): Unit =
    if Files.exists(path) then Files.walk(path).sorted(Comparator.reverseOrder()).forEach(Files.delete)

  /** The arguments of a C compiler compiling the source into the output with a library. */
  def ccArgs(source: String, output: String): Seq[String] = Seq(source, "-o", output, "-lfoo")

  test("Platform.compile"):
    // A fake compiler logs its arguments and a variable, prints its version for `-v`, and otherwise copies the program,
    // which is a shell script, to the output.
    val dir = Files.createTempDirectory("recheck compile ")
    val compiler = dir.resolve("my compiler")
    val log = dir.resolve("log")
    Files.writeString(
      compiler,
      s"#!/bin/sh\necho \"$$FOO $$*\" >> '$log'\nfor a in \"$$@\"; do\n  if [ \"$$a\" = -v ]; then echo fake 1.0; exit 0; fi\n  if [ \"$$prev\" = -o ]; then out=\"$$a\"; fi\n  case \"$$a\" in *.sh) src=\"$$a\" ;; esac\n  prev=\"$$a\"\ndone\ncp \"$$src\" \"$$out\"\nchmod +x \"$$out\"\n"
    )
    compiler.toFile.setExecutable(true)
    val program = SourceFile("recheck-test.sh", s"#!/bin/sh\necho \"$$(cat)\" # ${System.nanoTime()}\n")
    val line = s"'$compiler' -O"
    val env = Map("FOO" -> "foo")
    def compile(command: String, replacement: Option[String], env: Map[String, String] = Map.empty): String =
      Platform.compile(command, replacement, program, Seq(Seq("-v")), ccArgs, identity, env, Duration.Inf)

    val binary = compile("cc", Some(line), env)
    try
      assertEquals(
        Paths.get(binary),
        SourceDirectory.outputPath(program, Seq(line, "recheck-test.sh", "-o", "recheck-test", "-lfoo"), "fake 1.0\n")
      )
      assertEquals(
        Platform.run(binary, Seq.empty, None, Map.empty, "input", None, "start"),
        ExecResult.Exit(0, "input\n", "")
      )
      // The output of the same source and command line is compiled once, and the version is printed once.
      assertEquals(compile("cc", Some(line), env), binary)
      val Seq(version, args) = Files.readAllLines(log).asScala.toSeq: @unchecked
      assertEquals(version, "foo -O -v")
      assert(clue(args).matches("foo -O .*/recheck-test\\.sh -o .*/recheck-test-[0-9a-f]{16} -lfoo"))
      // The command runs without the replacement.
      val direct = compile(compiler.toString, None)
      assertEquals(Files.readAllLines(log).size, 4)
      delete(Paths.get(direct))
      // A failed command printing the version tells its output.
      val failing = Some("sh -c 'echo bad; echo worse >&2; exit 1'")
      interceptMessage[UnexpectedException]("cannot run `sh -c 'echo bad; echo worse >&2; exit 1' -v`:\nbad\nworse"):
        compile("cc", failing)
      // A command printing no version fails too.
      interceptMessage[UnexpectedException]("cannot run `true -v`: no output")(compile("cc", Some("true")))
      // A failed compilation tells the output of the compiler.
      val failingCompilation = Some("sh -c 'echo bad; echo worse >&2; test \"$1\" = -v' sh")
      interceptMessage[UnexpectedException](
        "cannot compile `sh -c 'echo bad; echo worse >&2; test \"$1\" = -v' sh recheck-test.sh -o recheck-test -lfoo`:\nbad\nworse"
      ):
        compile("cc", failingCompilation)
      // A compilation writing no output fails too.
      val noOutput = Some("sh -c 'if [ \"$1\" = -v ]; then echo 1.0; fi' sh")
      interceptMessage[UnexpectedException](
        "cannot compile `sh -c 'if [ \"$1\" = -v ]; then echo 1.0; fi' sh recheck-test.sh -o recheck-test -lfoo`: no output"
      ):
        compile("cc", noOutput)
    finally
      delete(Paths.get(binary))
      delete(dir)

  test("Platform.compile: a directory"):
    // A fake compiler writes a class file into the directory of `-d`.
    val program = SourceFile("Main.java", s"class Main {} // ${System.nanoTime()}")
    val compile = "mkdir \"$2\" && touch \"$2/Main.class\""
    val versionArgs = Seq(Seq("-c", "echo 1.0"), Seq("-c", "echo 2.0"))
    def output(): Path =
      val args = (s: String, o: String) => Seq("-c", compile, "sh", "-d", o, s)
      Paths.get(Platform.compile("sh", None, program, versionArgs, args, _ + "/Main.class", Map.empty, 1.minute))

    val dir = output()
    try
      assert(Files.isRegularFile(dir.resolve("Main.class")))
      // The key of the output has the outputs of the commands printing the version.
      val line = Seq("sh", "-c", compile, "sh", "-d", "Main", "Main.java")
      assertEquals(dir, SourceDirectory.outputPath(program, line, "1.0\n2.0\n"))
      // An output without its entry file is compiled again.
      Files.delete(dir.resolve("Main.class"))
      assertEquals(output(), dir)
      assert(Files.isRegularFile(dir.resolve("Main.class")))
    finally delete(dir)

  test("Platform.compile: timeout"):
    val program = SourceFile("Main.sh", s"# ${System.nanoTime()}")
    def compile(version: String, compile: String, timeout: Duration = 200.millis): String =
      val args = (s: String, o: String) => Seq("-c", compile, "sh", s, o)
      Platform.compile("sh", None, program, Seq(Seq("-c", version)), args, identity, Map.empty, timeout)

    // The version of `echo 1.0` is printed once in this process, so that the timeouts below come after it.
    intercept[UnexpectedException](compile("echo 1.0", "exit 1", 1.minute))
    // The command printing the version and the compilation are killed at the timeout.
    interceptMessage[UnexpectedException]("cannot run `sh -c sleep 5`: timed out")(compile("sleep 5", "true"))
    interceptMessage[UnexpectedException]("cannot compile `sh -c sleep 5 sh Main.sh Main`: timed out"):
      compile("echo 1.0", "sleep 5")
    // Waiting for another compilation of the output in this process stops at the timeout too.
    val path = SourceDirectory.outputPath(program, Seq("sh", "-c", "true", "sh", "Main.sh", "Main"), "1.0\n")
    val locked = new CountDownLatch(1)
    val done = new CountDownLatch(1)
    val holder = new Thread(() =>
      val lock = SourceDirectory.lock(path)
      lock.lock()
      locked.countDown()
      done.await()
      lock.unlock()
    )
    holder.start()
    locked.await()
    try
      interceptMessage[UnexpectedException]("cannot compile `sh -c true sh Main.sh Main`: timed out"):
        compile("echo 1.0", "true")
    finally done.countDown()

  test("Platform.compile: a file that cannot be written"):
    // The source file cannot be written into a missing directory.
    val program = SourceFile("missing/Main.sh", s"# ${System.nanoTime()}")
    val args = (s: String, o: String) => Seq("-c", "true", "sh", s, o)
    val ex = intercept[UnexpectedException]:
      Platform.compile("sh", None, program, Seq(Seq("-c", "echo 1.0")), args, identity, Map.empty, 1.minute)
    assert(clue(ex.getMessage).startsWith("cannot compile `sh -c true sh missing/Main.sh missing/Main`: "))
