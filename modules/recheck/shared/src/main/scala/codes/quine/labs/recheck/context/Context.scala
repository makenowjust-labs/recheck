package codes.quine.labs.recheck.context

import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.SECONDS
import scala.language.experimental.macros
import scala.quoted.*

import codes.quine.labs.recheck.exception.CancelException
import codes.quine.labs.recheck.exception.TimeoutException

/** Context is an execution context which has a deadline and can be canceled.
  *
  * Note that it is NOT reusable. We must create a new instance on each execution.
  */
final class Context private (
    initialDeadline: Deadline,
    private[recheck] val token: CancellationToken,
    private[recheck] val logger: Context.Logger
):

  /** The deadline, which [[pauseTimeout]] puts off by the time that its bodies take. */
  @volatile private var currentDeadline: Deadline = initialDeadline

  /** The number of the bodies of [[pauseTimeout]] running now. */
  @volatile private var pauseCount: Int = 0

  /** The time when the running bodies of [[pauseTimeout]] started. */
  private var pauseStart: Deadline = null

  /** Whether this context has a logger. */
  private[recheck] val hasLogger: Boolean = logger ne null

  /** The deadline, or `null` if this context has no timeout. */
  private[recheck] def deadline: Deadline = currentDeadline

  /** Checks whether this context is interrupted or not. */
  def isInterrupted(): Boolean =
    (token ne null) && token.isCancelled() || isOverdue

  /** Checks whether the deadline has passed, which never holds while [[pauseTimeout]] runs a body. */
  private[recheck] def isOverdue: Boolean =
    // The end of a pause puts off the deadline before it decreases the count, so the deadline is read after the count.
    (currentDeadline ne null) && pauseCount == 0 && currentDeadline.isOverdue()

  /** Runs the body with the timeout paused, so that the time that the body takes does not count toward the deadline.
    *
    * The cancellation still interrupts the body. The deadline moves by the time from the start of the first running
    * body to the end of the last one, so that nested or concurrent bodies count once. An interrupted context does not
    * start the body.
    */
  inline def pauseTimeout[A](inline body: A): A =
    interrupt(())
    paused(body)

  /** Runs the body with the timeout paused, which is [[pauseTimeout]] without checking the interruption. */
  private[recheck] def paused[A](body: => A): A =
    if currentDeadline eq null then body
    else
      synchronized:
        if pauseCount == 0 then pauseStart = Deadline.now
        pauseCount += 1
      try body
      finally
        synchronized:
          if pauseCount == 1 then currentDeadline = currentDeadline + (Deadline.now - pauseStart)
          pauseCount -= 1

  /** Runs the body with checking interruption on this context. */
  private[recheck] inline def interrupt[A](inline body: A): A =
    val isCancelled = (token ne null) && token.isCancelled()
    val isInterrupted = isCancelled || isOverdue

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
