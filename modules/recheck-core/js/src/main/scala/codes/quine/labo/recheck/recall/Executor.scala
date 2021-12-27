package codes.quine.labo.recheck.recall

import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.UnexpectedException

/** Executor is the recall validation executor. */
object Executor {

  /** Executes a new process. */
  @nowarn
  private[recall] def exec(code: String, timeout: Option[FiniteDuration])(implicit
      ctx: Context
  ): (Int, String, String) = throw new UnexpectedException("recall validation is not supported.")
}
