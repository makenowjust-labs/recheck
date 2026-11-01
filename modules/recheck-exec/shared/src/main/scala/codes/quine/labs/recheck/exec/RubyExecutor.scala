package codes.quine.labs.recheck.exec

import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.common.Context

/** RubyExecutor runs a Ruby program on `ruby`, or the command line of `RECHECK_RUBY`.
  *
  * The program is read as UTF-8, while `ruby -` reads it in the encoding of the locale by default.
  */
object RubyExecutor extends Executor.Stdin("ruby", "RECHECK_RUBY"):

  override def exec(input: String, timeout: Option[FiniteDuration], marker: Option[String], env: Map[String, String])(
      using Context
  ): ExecResult =
    super.exec("# encoding: UTF-8\n" + input, timeout, marker, env)
