package codes.quine.labo.recheck.exec

import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.UnexpectedException

/** NodeExecutor is the `node` executor. */
object NodeExecutor {

  /** Executes `node` command. */
  @nowarn
  def exec(code: String, timeout: Option[FiniteDuration])(implicit ctx: Context): Option[(Int, String, String)] =
    // $COVERAGE-OFF$
    throw new UnexpectedException("recall validation is not supported.")
  // $COVERAGE-ON$
}
