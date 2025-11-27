package codes.quine.labs.recheck.recall

import scala.concurrent.duration.FiniteDuration

/** RecallResult represents the result of recall validation. */
enum RecallResult:
  case Finish(time: FiniteDuration)
  case Error(message: String)
  case Timeout
