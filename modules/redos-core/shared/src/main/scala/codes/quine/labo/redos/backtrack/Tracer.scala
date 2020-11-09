package codes.quine.labo.redos
package backtrack

import scala.collection.mutable

import util.Timeout

/** Tracer is a common interface of VM execution tracers. */
trait Tracer {

  /** An underlying timeout instance. */
  def timeout: Timeout

  /** Traces an execution. */
  def trace(pos: Int, pc: Int, backtrack: Boolean): Unit

  /** A tracer execution result. */
  def result(): Tracer.Result
}

/** Tracer instaances. */
object Tracer {

  /** Result is a tracer result.
    *
    * `traces` is a map from a position to a pair of pc and backtrack flag.
    */
  final case class Result(steps: Int, traces: Map[Int, Set[(Int, Boolean)]]) {

    /** A set of a pair of pc and backtrack flag. */
    lazy val coverage: Set[(Int, Boolean)] = traces.values.flatten.toSet
  }

  /** LimitTracer is a tracer implementation which can trace execution on a limit.
    * When an execution step exceeds the limit, it throws [[LimitException]].
    */
  final class LimitTracer(val limit: Int = Int.MaxValue, val timeout: Timeout = Timeout.NoTimeout) extends Tracer {
    private[this] var steps = 0
    private[this] val traces =
      mutable.Map.empty[Int, mutable.Builder[(Int, Boolean), Set[(Int, Boolean)]]]

    def trace(pos: Int, pc: Int, backtrack: Boolean): Unit = {
      steps += 1
      if (steps >= limit) throw new LimitException("limit is exceeded")
      if (!traces.contains(pos)) traces(pos) = Set.newBuilder[(Int, Boolean)]
      traces(pos).addOne((pc, backtrack))
    }

    def result(): Result = Result(steps, traces.toMap.view.mapValues(_.result()).toMap)
  }

  /** NoTracer is a tracer implementation which traces notthing in fact. */
  final case class NoTracer(timeout: Timeout = Timeout.NoTimeout) extends Tracer {
    def trace(pos: Int, pc: Int, backtrack: Boolean): Unit = ()
    def result(): Result = throw new UnsupportedOperationException
  }
}
