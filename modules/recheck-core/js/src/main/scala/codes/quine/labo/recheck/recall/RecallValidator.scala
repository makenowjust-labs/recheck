package codes.quine.labo.recheck.recall

import scala.annotation.nowarn
import scala.concurrent.duration.Duration

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.UnexpectedException
import codes.quine.labo.recheck.diagnostics.AttackPattern

object RecallValidator {
  @nowarn
  def validate(source: String, flags: String, pattern: AttackPattern, timeout: Duration)(implicit
      ctx: Context
  ): RecallResult = throw new UnexpectedException("recall validation is not supported.")
}
