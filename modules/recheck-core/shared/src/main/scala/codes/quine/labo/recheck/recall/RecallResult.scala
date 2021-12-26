package codes.quine.labo.recheck.recall

import scala.concurrent.duration.FiniteDuration

sealed abstract class RecallResult extends Product with Serializable

object RecallResult {
  final case class Finish(time: FiniteDuration) extends RecallResult

  final case class Error(message: String) extends RecallResult

  case object Timeout extends RecallResult
}
