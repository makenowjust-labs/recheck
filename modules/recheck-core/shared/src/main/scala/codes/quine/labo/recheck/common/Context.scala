package codes.quine.labo.recheck.common

import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

/** Context is an execution context which has a deadline and can be canceled.
  *
  * Note that it is NOT reusable. We must create a new instance on each execution.
  */
final class Context private (
    private[this] val deadline: Option[Deadline] = None,
    private var canceled: Boolean = false
) {

  /** Checks whether this context is interrupted or not. */
  @inline def isInterrupted(): Boolean = canceled || deadline.exists(_.isOverdue())

  /** Run the body with checking interruption on this context. */
  @inline def interrupt[A](body: => A)(implicit enclosing: sourcecode.Enclosing): A = {
    if (isInterrupted()) {
      if (canceled) throw new CancelException(enclosing.value)
      else throw new TimeoutException(enclosing.value)
    }
    val result = body
    result
  }
}

/** Context creators. */
object Context {

  /** Creates a new context with the given timeout. */
  def apply(timeout: Duration = Duration.Inf): Context = timeout match {
    case d: FiniteDuration                         => new Context(Some(d.fromNow))
    case d: Duration.Infinite if d < Duration.Zero => new Context(canceled = true)
    case _: Duration.Infinite                      => new Context(None)
  }

  /** Creates a new context and returns it with a cancellation function. */
  def cancellable(timeout: Duration = Duration.Inf): (Context, () => Unit) = {
    val ctx = Context(timeout)
    val cancel = () => ctx.canceled = true
    (ctx, cancel)
  }
}
