package codes.quine.labs.recheck.exec

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermissions
import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.CancelException
import codes.quine.labs.recheck.common.CancellationTokenSource
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException

class PlatformSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  def node(script: String, timeout: Option[FiniteDuration])(using Context): ExecResult =
    Platform.run("node", Seq("-"), None, Map.empty, None, script, timeout, "start")

  test("Platform.run"):
    assertEquals(node("console.log('foo');", None), ExecResult.Exit(0, "foo\n", ""))
    assertEquals(node("console.error('bar'); process.exit(1)", None), ExecResult.Exit(1, "", "bar\n"))
    assertEquals(
      node("console.log('é');", None),
      ExecResult.Exit(0, "é\n", "")
    )
    assertEquals(
      Platform
        .run("node", Seq("-e", "console.log(process.env.FOO)"), None, Map("FOO" -> "foo"), None, "", None, "start"),
      ExecResult.Exit(0, "foo\n", "")
    )

  test("Platform.run: timeout"):
    assertEquals(
      node("console.log('start'); console.log('foo'); setTimeout(() => {}, 5000);", Some(100.millis)),
      ExecResult.Timeout("start\nfoo\n")
    )

  test("Platform.run: marker"):
    // The startup before the marker does not count toward the timeout.
    val script = "setTimeout(() => { console.log('start'); console.log('end'); }, 500);"
    assertEquals(node(script, Some(200.millis)), ExecResult.Exit(0, "start\nend\n", ""))
    assertEquals(
      node("console.log('start'); setTimeout(() => {}, 5000);", Some(200.millis)),
      ExecResult.Timeout("start\n")
    )
    // A process exiting without the marker is just finished.
    assertEquals(
      node("console.log('foo');", Some(200.millis)),
      ExecResult.Exit(0, "foo\n", "")
    )

  test("Platform.run: interruption"):
    intercept[TimeoutException]:
      node("setTimeout(() => {}, 5000);", None)(using Context(timeout = 200.millis))
    val source = new CancellationTokenSource
    source.cancel()
    intercept[CancelException]:
      node("setTimeout(() => {}, 5000);", None)(using Context(token = Some(source.token)))

  test("Platform.run: missing command"):
    intercept[UnexpectedException]:
      Platform.run("recheck-missing-command", Seq.empty, None, Map.empty, None, "", None, "start")

  test("Platform.run: file"):
    val file = SourceFile("recheck-test.js", "console.log(process.argv.length, require('fs').readFileSync(0, 'utf8'))")
    def run(input: String): ExecResult =
      Platform.run("node", Seq("--no-warnings"), None, Map.empty, Some(file), input, None, "start")

    try
      assertEquals(run("foo"), ExecResult.Exit(0, "2 foo\n", ""))
      assertEquals(run("bar"), ExecResult.Exit(0, "2 bar\n", ""))
    finally Files.deleteIfExists(SourceDirectory.path(file))

  test("Platform.run: shell"):
    val dir = Files.createTempDirectory("recheck exec ")
    val node = dir.resolve("my node")
    try
      Files.writeString(node, "#!/bin/sh\necho \"$# $*\"\n")
      node.toFile.setExecutable(true)
      val replacement = Some(s"'$node' --flag")
      assertEquals(
        Platform.run("node", Seq("-", "a b"), replacement, Map.empty, None, "", None, "start"),
        ExecResult.Exit(0, "3 --flag - a b\n", "")
      )
    finally
      Files.deleteIfExists(node)
      Files.deleteIfExists(dir)

  test("SourceDirectory.path"):
    val dir = Files.createTempDirectory("recheck-exec-")
    try
      val source = SourceFile("Foo.java", "class Foo {}")
      val file = SourceDirectory.path(dir, source)
      assertEquals(file.getParent, dir)
      assert(clue(file.getFileName.toString).matches("Foo-[0-9a-f]{16}\\.java"))
      assertEquals(Files.readString(file), "class Foo {}")
      val modified = Files.getLastModifiedTime(file)
      assertEquals(SourceDirectory.path(dir, source), file)
      assertEquals(Files.getLastModifiedTime(file), modified)
      // A changed file is written again.
      Files.writeString(file, "broken")
      assertEquals(SourceDirectory.path(dir, source), file)
      assertEquals(Files.readString(file), "class Foo {}")
      assertNotEquals(SourceDirectory.path(dir, source.copy(content = "class Foo { }")), file)
      assertEquals(Files.list(dir).count(), 2L)
    finally
      Files.list(dir).forEach(Files.delete)
      Files.delete(dir)

  test("SourceDirectory.sharedDirectory"):
    val parent = Files.createTempDirectory("recheck-exec-")
    try
      val created = parent.resolve("new")
      assertEquals(SourceDirectory.sharedDirectory(created), Some(created))
      assertEquals(Files.getPosixFilePermissions(created), PosixFilePermissions.fromString("rwx------"))
      assertEquals(Files.list(created).count(), 0L)
      assertEquals(SourceDirectory.sharedDirectory(created), Some(created))
      // A directory others can write is not used.
      val open = Files.createDirectory(parent.resolve("open"))
      Files.setPosixFilePermissions(open, PosixFilePermissions.fromString("rwxrwxrwx"))
      assertEquals(SourceDirectory.sharedDirectory(open), None)
      val link = Files.createSymbolicLink(parent.resolve("link"), created)
      assertEquals(SourceDirectory.sharedDirectory(link), None)
    finally Files.walk(parent).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
