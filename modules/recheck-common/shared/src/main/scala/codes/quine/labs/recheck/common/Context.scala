package codes.quine.labs.recheck.common

import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.SECONDS
import scala.language.experimental.macros
import scala.quoted.*

/** Context is an execution context which has a deadline and can be canceled.
  *
  * Note that it is NOT reusable. We must create a new instance on each execution.
  */
final class Context private (
    private[recheck] val deadline: Deadline,
    private[recheck] val token: CancellationToken,
    private[recheck] val logger: Context.Logger
):

  /** */
  private[recheck] val hasLogger: Boolean = logger ne null

  /** Checks whether this context is interrupted or not. */
  def isInterrupted(): Boolean =
    (token ne null) && token.isCancelled() || (deadline ne null) && deadline.isOverdue()

  /** Runs the body with checking interruption on this context. */
  private[recheck] inline def interrupt[A](inline body: A): A =
    val isCancelled = (token ne null) && token.isCancelled()
    val isInterrupted = isCancelled || (deadline ne null) && deadline.isOverdue()

    if isInterrupted then
      if isCancelled then throw new CancelException(Context.source())
      else throw TimeoutException(Context.source())

    body

  /** Writes a given log message when logging is enabled. */
  private[recheck] inline def log(inline message: String): Unit =
    if hasLogger then logger(message)

/** Context creators. */
object Context:

  /** Logger is a logger function. */
  abstract class Logger extends (String => Unit):
    def apply(message: String): Unit

  /** Creates a new context with the given timeout and the given cancellation token. */
  def apply(
      timeout: Duration = Duration.Inf,
      token: Option[CancellationToken] = None,
      logger: Option[Logger] = None
  ): Context = timeout match
    case d if d < Duration.Zero => new Context(Deadline(Duration(-1, SECONDS)), null, logger.orNull)
    case d: FiniteDuration      => new Context(d.fromNow, token.orNull, logger.orNull)
    case _: Duration.Infinite   => new Context(null, token.orNull, logger.orNull)

  private inline def source(): String = ${ locationImpl() }

  private def locationImpl()(using Quotes): Expr[String] =
    import quotes.reflect.*

    val pos = Position.ofMacroExpansion
    val path = pos.sourceFile.path.replaceFirst("\\A.*/(?=modules)", "")
    val source = s"$path:${pos.startLine + 1}"

    Expr(source)
