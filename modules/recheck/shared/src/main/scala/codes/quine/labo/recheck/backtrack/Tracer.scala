package codes.quine.labo.recheck
package backtrack
import data.UString

/** Tracer is a common interface of VM execution tracers. */
trait Tracer {

  /** Traces an execution step. */
  def trace(pos: Int, pc: Int, backtrack: Boolean, capture: Int => Option[UString], cnts: Seq[Int]): Unit
}

/** Tracer instances. */
object Tracer {

  /** LimitException is an exception thrown when VM execution step exceeds a limit. */
  class LimitException(message: String) extends Exception(message)

  /** LimitTracer is a tracer implementation which can trace the execution on a limit.
    * When an execution step exceeds the limit, it throws [[LimitException]].
    */
  class LimitTracer(val ir: IR, val limit: Int = Int.MaxValue) extends Tracer {
    private[this] var counter = 0

    def steps(): Int = counter

    def trace(pos: Int, pc: Int, backtrack: Boolean, capture: Int => Option[UString], cnts: Seq[Int]): Unit = {
      if (ir.codes(pc).isConsumable && !backtrack) {
        counter += 1
        if (counter >= limit) throw new LimitException("limit is exceeded")
      }
    }
  }

  /** NoTracer is a tracer implementation which traces nothing in fact. */
  case object NoTracer extends Tracer {
    def trace(pos: Int, pc: Int, backtrack: Boolean, capture: Int => Option[UString], cnts: Seq[Int]): Unit = ()
  }
}
