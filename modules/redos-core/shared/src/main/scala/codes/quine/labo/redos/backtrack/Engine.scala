package codes.quine.labo.redos
package backtrack

import scala.util.Try

import data.UString
import regexp.Pattern

/** RegExp pattern matching Engine (frontend). */
object Engine {

  /** Tests a matching of the pattern on the input. */
  def matches(pattern: Pattern, input: String, pos: Int = 0, tracer: Tracer = Tracer.NoTracer()): Try[Option[Match]] =
    IRCompiler.compile(pattern).map(VM.execute(_, UString.from(input, pattern.flagSet.unicode), pos, tracer = tracer))
}
