package codes.quine.labs.recheck.exec

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.exception.UnexpectedException

/** Platform runs the commands of the executors, which is not supported on Scala.js. */
private[exec] object Platform:

  val isAvailable: Boolean = false

  def run(
      command: String,
      args: Seq[String],
      replacement: Option[String],
      env: Map[String, String],
      input: String,
      timeout: Option[FiniteDuration],
      marker: String
  )(using Context): ExecResult =
    // $COVERAGE-OFF$
    throw new UnexpectedException("Running a command is not supported on Scala.js.")
    // $COVERAGE-ON$

  def compile(
      command: String,
      replacement: Option[String],
      program: SourceFile,
      versionArgs: Seq[Seq[String]],
      compileArgs: (String, String) => Seq[String],
      entry: String => String,
      env: Map[String, String],
      timeout: Duration
  )(using Context): String =
    // $COVERAGE-OFF$
    throw new UnexpectedException("Running a command is not supported on Scala.js.")
    // $COVERAGE-ON$
