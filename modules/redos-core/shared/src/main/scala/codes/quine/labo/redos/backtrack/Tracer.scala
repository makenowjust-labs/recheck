package codes.quine.labo.redos
package backtrack

import data.UString
import util.Timeout

/** Tracer is a common interface of VM execution tracers. */
trait Tracer {

  /** An underlying timeout instance. */
  def timeout: Timeout

  /** Traces an execution. */
  def trace(pos: Int, pc: Int, backtrack: Boolean, capture: Int => Option[UString], cnts: Seq[Int]): Unit

  /** An alias to `timeout.checkTimeout`. */
  def checkTimeout[A](phase: String)(body: => A): A = timeout.checkTimeout(phase)(body)
}

/** Tracer instances. */
object Tracer {

  /** LimitTracer is a tracer implementation which can trace execution on a limit.
    * When an execution step exceeds the limit, it throws [[LimitException]].
    */
  class LimitTracer(val limit: Int = Int.MaxValue, val timeout: Timeout = Timeout.NoTimeout) extends Tracer {
    private[this] var counter = 0

    def steps: Int = counter

    def trace(pos: Int, pc: Int, backtrack: Boolean, capture: Int => Option[UString], cnts: Seq[Int]): Unit = {
      counter += 1
      if (counter >= limit) throw new LimitException("limit is exceeded")
    }
  }

  /** NoTracer is a tracer implementation which traces notthing in fact. */
  final case class NoTracer(timeout: Timeout = Timeout.NoTimeout) extends Tracer {
    def trace(pos: Int, pc: Int, backtrack: Boolean, capture: Int => Option[UString], cnts: Seq[Int]): Unit = ()
  }
}
