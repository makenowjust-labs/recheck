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

  /** EngineLimit is a matching that the engine stopped on its limit of the matching steps, e.g. PHP's backtrack limit.
    *
    * The engine counts the steps of backtracking, so reaching the limit shows that the attack is effective.
    */
  case EngineLimit(message: String)

  /** EngineError is a matching that the engine gave up with an error on the stack depth or the memory, e.g. Java's
    * `StackOverflowError`.
    *
    * The engine may raise it for a long string even if the matching takes linear time, so the attack is not considered
    * effective.
    */
  case EngineError(message: String)

  /** Whether the attack is effective on the engine. */
  def succeeded: Boolean = this match
    case Finish(_)      => false
    case Timeout        => true
    case EngineLimit(_) => true
    case EngineError(_) => false

  override def toString: String = this match
    case Finish(time)         => s"finished in ${time.toMillis} ms"
    case Timeout              => "timed out"
    case EngineLimit(message) => s"engine limit: $message"
    case EngineError(message) => s"engine error: $message"
