package codes.quine.labs.recheck.exec
import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException

/** NodeExecutor is the `node` executor. */
object NodeExecutor:

  /** Executes `node` command. */
  def exec(code: String, timeout: Option[FiniteDuration])(using ctx: Context): Option[(Int, String, String)] =
    // $COVERAGE-OFF$
    throw new UnexpectedException("recall validation is not supported.")
    // $COVERAGE-ON$
