package codes.quine.labs.recheck.exec

import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException

/** Platform runs the commands of the executors, which is not supported on Scala.js. */
private[exec] object Platform:

  val isAvailable: Boolean = false

  def run(
      command: String,
      args: Seq[String],
      replacement: Option[String],
      env: Map[String, String],
      file: Option[SourceFile],
      input: String,
      timeout: Option[FiniteDuration],
      marker: String
  )(using Context): ExecResult =
    // $COVERAGE-OFF$
    throw new UnexpectedException("recall validation is not supported.")
    // $COVERAGE-ON$
