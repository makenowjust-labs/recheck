package codes.quine.labs.recheck.exec

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermissions
import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.CancelException
import codes.quine.labs.recheck.common.CancellationTokenSource
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException

class ExecutorSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val node: Command = Command("node", Seq("-"))

  test("Executor.exec"):
    assertEquals(Executor.exec(node, "console.log('foo');", None), ExecResult.Exit(0, "foo\n", ""))
    assertEquals(Executor.exec(node, "console.error('bar'); process.exit(1)", None), ExecResult.Exit(1, "", "bar\n"))
    assertEquals(
      Executor.exec(node, "console.log('é');", None),
      ExecResult.Exit(0, "é\n", "")
    )
    assertEquals(
      Executor.exec(Command("node", Seq("-e", "console.log(process.env.FOO)"), Map("FOO" -> "foo")), "", None),
      ExecResult.Exit(0, "foo\n", "")
    )

  test("Executor.exec: timeout"):
    assertEquals(
      Executor.exec(node, "console.log('foo'); setTimeout(() => {}, 5000);", Some(100.millis)),
      ExecResult.Timeout("foo\n")
    )

  test("Executor.exec: marker"):
    // The startup before the marker does not count toward the timeout.
    val script = "setTimeout(() => { console.log('start'); console.log('end'); }, 500);"
    assertEquals(Executor.exec(node, script, Some(200.millis), Some("start")), ExecResult.Exit(0, "start\nend\n", ""))
    assertEquals(
      Executor.exec(node, "console.log('start'); setTimeout(() => {}, 5000);", Some(200.millis), Some("start")),
      ExecResult.Timeout("start\n")
    )
    // A process exiting without the marker is just finished.
    assertEquals(
      Executor.exec(node, "console.log('foo');", Some(200.millis), Some("start")),
      ExecResult.Exit(0, "foo\n", "")
    )

  test("Executor.exec: interruption"):
    intercept[TimeoutException]:
      Executor.exec(node, "setTimeout(() => {}, 5000);", None, Some("start"))(using Context(timeout = 200.millis))
    val source = new CancellationTokenSource
    source.cancel()
    intercept[CancelException]:
      Executor.exec(node, "setTimeout(() => {}, 5000);", None)(using Context(token = Some(source.token)))

  test("Executor.exec: missing command"):
    intercept[UnexpectedException]:
      Executor.exec(Command("recheck-missing-command"), "", None)

  test("Executor.exec: source"):
    val source =
      Command.Source("recheck-test.js", "console.log(process.argv.length, require('fs').readFileSync(0, 'utf8'))")
    val command = Command("node", Seq("--no-warnings"), source = Some(source))
    try
      assertEquals(Executor.exec(command, "foo", None), ExecResult.Exit(0, "2 foo\n", ""))
      assertEquals(Executor.exec(command, "bar", None), ExecResult.Exit(0, "2 bar\n", ""))
      assertEquals(command.toString, "node --no-warnings recheck-test.js")
    finally Files.deleteIfExists(SourceFiles.path(source))

  test("Executor.exec: shell"):
    val dir = Files.createTempDirectory("recheck exec ")
    val node = dir.resolve("my node")
    try
      Files.writeString(node, "#!/bin/sh\necho \"$# $*\"\n")
      node.toFile.setExecutable(true)
      val command = Command("node", Seq("-", "a b")).withShell(Executor.shell, s"'$node' --flag")
      assertEquals(command.args.take(2), Seq("-c", s"'$node' --flag \"$$@\""))
      assertEquals(Executor.exec(command, "", None), ExecResult.Exit(0, "3 --flag - a b\n", ""))
    finally
      Files.deleteIfExists(node)
      Files.deleteIfExists(dir)

  test("SourceFiles.path"):
    val dir = Files.createTempDirectory("recheck-exec-")
    try
      val source = Command.Source("Foo.java", "class Foo {}")
      val file = SourceFiles.path(dir, source)
      assertEquals(file.getParent, dir)
      assert(clue(file.getFileName.toString).matches("Foo-[0-9a-f]{16}\\.java"))
      assertEquals(Files.readString(file), "class Foo {}")
      val modified = Files.getLastModifiedTime(file)
      assertEquals(SourceFiles.path(dir, source), file)
      assertEquals(Files.getLastModifiedTime(file), modified)
      // A changed file is written again.
      Files.writeString(file, "broken")
      assertEquals(SourceFiles.path(dir, source), file)
      assertEquals(Files.readString(file), "class Foo {}")
      assertNotEquals(SourceFiles.path(dir, source.copy(content = "class Foo { }")), file)
      assertEquals(Files.list(dir).count(), 2L)
    finally
      Files.list(dir).forEach(Files.delete)
      Files.delete(dir)

  test("SourceFiles.sharedDirectory"):
    val parent = Files.createTempDirectory("recheck-exec-")
    try
      val created = parent.resolve("new")
      assertEquals(SourceFiles.sharedDirectory(created), Some(created))
      assertEquals(Files.getPosixFilePermissions(created), PosixFilePermissions.fromString("rwx------"))
      assertEquals(Files.list(created).count(), 0L)
      assertEquals(SourceFiles.sharedDirectory(created), Some(created))
      // A directory others can write is not used.
      val open = Files.createDirectory(parent.resolve("open"))
      Files.setPosixFilePermissions(open, PosixFilePermissions.fromString("rwxrwxrwx"))
      assertEquals(SourceFiles.sharedDirectory(open), None)
      val link = Files.createSymbolicLink(parent.resolve("link"), created)
      assertEquals(SourceFiles.sharedDirectory(link), None)
    finally Files.walk(parent).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
