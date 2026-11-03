package codes.quine.labs.recheck

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.exec.Executor

/** RuntimeHelper provides the helpers of the tests running the runtimes in `PATH`. */
trait RuntimeHelper:
  self: munit.FunSuite =>

  /** Returns the path of the program in `PATH`, or `None` if it is not there. */
  def which(program: String): Option[Path] =
    sys.env
      .getOrElse("PATH", "")
      .split(java.io.File.pathSeparatorChar)
      .iterator
      .filter(_.nonEmpty)
      .map(Paths.get(_, program))
      .find(Files.isExecutable)

  /** Checks whether the program is in `PATH`. */
  def isAvailable(program: String): Boolean = which(program).isDefined

  /** Defines a test, which fails when the program is not available. */
  def runtimeTest(name: String, program: String)(body: => Unit)(using munit.Location): Unit =
    test(name):
      assert(isAvailable(program), s"`$program` is not in PATH")
      body

  /** Creates a new directory having the file, and returns the file's path. */
  def createFile(prefix: String, name: String, content: String): Path =
    val dir = Files.createTempDirectory(prefix)
    val path = dir.resolve(name)
    Files.writeString(path, content)
    path.toFile.deleteOnExit()
    dir.toFile.deleteOnExit()
    path

  /** Runs the input on the runtime of the executor in the environment variables, and returns the standard output. */
  def run(executor: Executor, input: String = "", env: Map[String, String] = Map.empty)(using munit.Location): String =
    given Context = Context()
    executor.exec(input, None, "", env) match
      case ExecResult.Exit(0, out, _) => out
      case result                     => fail(s"`${executor.commandLine(env)}` failed: $result\n$input")

  /** Runs the input on the runtime of the executor, and returns the lines of the output marked by `case:`. */
  def cases(executor: Executor, input: String = "")(using munit.Location): Seq[String] =
    run(executor, input).linesIterator.collect { case s"case:$line" => line }.toSeq
