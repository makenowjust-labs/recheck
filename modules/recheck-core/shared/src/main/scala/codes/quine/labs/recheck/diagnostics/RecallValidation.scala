package codes.quine.labs.recheck
package diagnostics

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

/** RecallValidation is a run of an attack string on the real regex engine of the dialect.
  *
  * @param runtime
  *   the runtime name and version, e.g. `python 3.15.0`
  * @param timeout
  *   the timeout of the matching
  * @param result
  *   the result of the matching
  */
final case class RecallValidation(runtime: String, timeout: Duration, result: RecallResult):

  /** Whether the attack is effective on the engine. */
  def succeeded: Boolean = result.succeeded

  override def toString: String =
    val limit = timeout match
      case d: FiniteDuration => s"${d.toMillis} ms"
      case _                 => "none"
    s"${if succeeded then "succeeded" else "failed"} ($result on $runtime, timeout: $limit)"

/** RecallResult is the result of matching an attack string on the real regex engine. */
enum RecallResult:

  /** Finish is a matching finished in time, so the attack is not effective. */
  case Finish(time: FiniteDuration)

  /** Timeout is a matching not finished in time. */
  case Timeout

  /** EngineError is a matching that the engine gave up with a resource error, e.g. PHP's backtrack limit or Java's
    * `StackOverflowError`.
    */
  case EngineError(message: String)

  /** Whether the attack is effective on the engine. */
  def succeeded: Boolean = this match
    case Finish(_)      => false
    case Timeout        => true
    case EngineError(_) => true

  override def toString: String = this match
    case Finish(time)         => s"finished in ${time.toMillis} ms"
    case Timeout              => "timed out"
    case EngineError(message) => s"engine error: $message"
