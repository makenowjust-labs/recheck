package codes.quine.labo.redos.common

import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

/** Context is an execution context which has a deadline and can be cancelled.
  *
  * Note that it is NOT reusable. We must create a new instance on each execution.
  */
final class Context private (
    private[this] val deadline: Option[Deadline] = None,
    private var cancelled: Boolean = false
) {

  /** Checks whether this context is interrupted or not. */
  @inline def isInterrupted: Boolean = cancelled || deadline.exists(_.isOverdue())

  /** Run the body with checking interruption on this context. */
  @inline def interrupt[A](body: => A)(implicit enclosing: sourcecode.Enclosing): A = {
    if (isInterrupted) throw new TimeoutException(enclosing.value)
    val result = body
    result
  }
}

/** Context creators. */
object Context {

  /** Creates a new context with the given timeout. */
  def apply(timeout: Duration = Duration.Inf): Context = timeout match {
    case Duration.Inf | Duration.Undefined => new Context()
    case Duration.MinusInf                 => new Context(cancelled = true)
    case d: FiniteDuration                 => new Context(Some(Deadline(d)))
  }

  /** Creates a new context and returns it with a cancellation function. */
  def cancellable(timeout: Duration = Duration.Inf): (Context, () => Unit) = {
    val ctx = Context(timeout)
    val cancel = () => ctx.cancelled = true
    (ctx, cancel)
  }
}
