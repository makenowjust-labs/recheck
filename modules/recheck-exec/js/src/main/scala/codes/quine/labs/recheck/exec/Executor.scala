package codes.quine.labs.recheck.exec

import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException

/** Executor runs commands, which is not supported on Scala.js. */
object Executor:

  /** Whether this platform can run commands. */
  val isAvailable: Boolean = false

  /** Runs the command, which is not supported on Scala.js. */
  def exec(command: Command, script: String, timeout: Option[FiniteDuration], marker: Option[String] = None)(using
      ctx: Context
  ): ExecResult =
    // $COVERAGE-OFF$
    throw new UnexpectedException("running a command is not supported")
    // $COVERAGE-ON$
