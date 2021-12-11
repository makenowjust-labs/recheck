package codes.quine.labo.recheck.common

import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.SECONDS
import scala.language.experimental.macros

/** Context is an execution context which has a deadline and can be canceled.
  *
  * Note that it is NOT reusable. We must create a new instance on each execution.
  */
final class Context private (
    private[recheck] val deadline: Deadline,
    private[recheck] val token: CancellationToken
) {

  /** Checks whether this context is interrupted or not. */
  def isInterrupted(): Boolean =
    token != null && token.isCancelled() || deadline != null && deadline.isOverdue()

  /** Run the body with checking interruption on this context. */
  def interrupt[A](body: A): A = macro ContextMacro.interrupt
}

/** Context creators. */
object Context {

  /** Creates a new context with the given timeout and the given cancellation token. */
  def apply(timeout: Duration = Duration.Inf, token: Option[CancellationToken] = None): Context = timeout match {
    case d if d < Duration.Zero => new Context(Deadline(Duration(-1, SECONDS)), null)
    case d: FiniteDuration      => new Context(d.fromNow, token.orNull)
    case _: Duration.Infinite   => new Context(null, token.orNull)
  }
}
