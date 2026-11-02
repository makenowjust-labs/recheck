package codes.quine.labs.recheck

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

/** RuntimeHelper provides the helpers of the tests running the runtimes in `PATH`. */
trait RuntimeHelper:
  self: munit.FunSuite =>

  /** Checks whether the program is in `PATH`. */
  def available(program: String): Boolean =
    sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparatorChar).exists { dir =>
      dir.nonEmpty && Files.isExecutable(Paths.get(dir, program))
    }

  /** Defines a test, which fails when the program is not available. */
  def runtimeTest(name: String, program: String)(body: => Unit)(using munit.Location): Unit =
    test(name):
      assert(available(program), s"`$program` is not in PATH")
      body

  /** Creates a new directory having the file, and returns the file's path. */
  def createFile(prefix: String, name: String, content: String): Path =
    val dir = Files.createTempDirectory(prefix)
    val path = dir.resolve(name)
    Files.writeString(path, content)
    path.toFile.deleteOnExit()
    dir.toFile.deleteOnExit()
    path

  /** Runs the command with the input, and returns the output with the error output. */
  def run(command: Seq[String], input: String = "", dir: Option[Path] = None)(using munit.Location): String =
    val builder = new ProcessBuilder(command*).redirectErrorStream(true)
    dir.foreach(d => builder.directory(d.toFile))
    builder.environment().put("DOTNET_NOLOGO", "1")
    builder.environment().put("DOTNET_CLI_TELEMETRY_OPTOUT", "1")
    val process = builder.start()
    process.getOutputStream.write(input.getBytes(StandardCharsets.UTF_8))
    process.getOutputStream.close()
    val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val code = process.waitFor()
    if code != 0 then fail(s"${command.mkString(" ")} exited with $code\n$output\n$input")
    output
