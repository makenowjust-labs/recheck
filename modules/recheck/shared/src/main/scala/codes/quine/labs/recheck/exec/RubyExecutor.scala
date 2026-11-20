package codes.quine.labs.recheck.exec

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.context.Context

/** RubyExecutor runs a Ruby program on `ruby` without RubyGems, or the command line of `RECHECK_RUBY`.
  *
  * The program is read as UTF-8.
  */
object RubyExecutor extends Executor.Stdin("ruby", "RECHECK_RUBY", Seq("--disable-gems")):

  override def exec(
      input: String,
      timeout: Option[FiniteDuration],
      compileTimeout: Duration,
      marker: String,
      env: Map[String, String]
  )(using Context): ExecResult =
    // `ruby -` reads the program in the encoding of the locale by default.
    super.exec("# encoding: UTF-8\n" + input, timeout, compileTimeout, marker, env)
