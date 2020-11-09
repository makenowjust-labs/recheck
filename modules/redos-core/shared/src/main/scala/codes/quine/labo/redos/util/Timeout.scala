package codes.quine.labo.redos.util

import java.util.concurrent.TimeoutException

import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

/** Timeout is a interface of a timeout checker. */
trait Timeout {

  /** Checks a timeout, and returns the body value. */
  def checkTimeout[A](phase: String)(body: => A): A
}

/** Timeout types. */
object Timeout {

  /** Creates a timeout from the duration.
    *
    * If the duration is `Duration.Inf`, it returns [[NoTimeout]],
    * otherwise it returns [[DeadlineTimeout]].
    * When the duration is neither finite nor `Duration.Inf`, it throws an IllegalArgumentException.
    */
  def from(atMost: Duration): Timeout = atMost match {
    case Duration.Inf      => Timeout.NoTimeout
    case d: FiniteDuration => Timeout.DeadlineTimeout(d.fromNow)
    case _                 => throw new IllegalArgumentException
  }

  /** DeadlineTimeout is a timeout checker having a timeout as deadline. */
  final case class DeadlineTimeout(deadline: Deadline) extends Timeout {
    def checkTimeout[A](phase: String)(body: => A): A = {
      if (deadline.isOverdue()) throw new TimeoutException(phase)
      val result = body
      if (deadline.isOverdue()) throw new TimeoutException(phase)
      result
    }
  }

  /** NoTimeout is a timeout checker which is never timeout. */
  case object NoTimeout extends Timeout {
    def checkTimeout[A](phase: String)(body: => A): A = body
  }
}
