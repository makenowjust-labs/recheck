package codes.quine.labs.recheck

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.concurrent.duration.Duration

import codes.quine.labs.recheck.context.Context
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

  /** Runs the input on the runtime of the executor, or on the command line replacing it, and returns the standard
    * output.
    */
  def run(executor: Executor, input: String = "", command: Option[String] = None)(using munit.Location): String =
    given Context = Context()
    executor.exec(input, None, Duration.Inf, "", command) match
      case ExecResult.Exit(0, out, _) => out
      case result                     => fail(s"`${executor.commandLine(command)}` failed: $result\n$input")

  /** Runs the input on the runtime of the executor, or on the command line replacing it, and returns the lines of the
    * output marked by `case:`.
    */
  def cases(executor: Executor, input: String = "", command: Option[String] = None)(using munit.Location): Seq[String] =
    run(executor, input, command).linesIterator.collect { case s"case:$line" => line }.toSeq

  /** The command line of ICU's compiler, which mise sets as `RECHECK_TEST_ICU` with the paths of the ICU 79.1 that it
    * installs.
    */
  def icuCommand: Option[String] = sys.env.get("RECHECK_TEST_ICU")

  /** The command line of Boost.Regex's compiler, which mise sets as `RECHECK_TEST_BOOST_REGEX` with the paths of the
    * Boost.Regex 1.92.0 and the ICU 79.1 that it installs.
    */
  def boostRegexCommand: Option[String] = sys.env.get("RECHECK_TEST_BOOST_REGEX")
