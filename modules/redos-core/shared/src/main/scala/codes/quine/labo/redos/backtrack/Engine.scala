package codes.quine.labo.redos
package backtrack

import scala.util.Try

import data.UString
import regexp.Pattern

/** RegExp pattern matching Engine (frontend). */
object Engine {

  /** Tests a matching of the pattern on the input. */
  def matches(pattern: Pattern, input: String, pos: Int = 0, tracer: Tracer = Tracer.NoTracer()): Try[Option[Match]] =
    tracer.checkTimeout("backtrack.Engine.matches") {
      val original = UString.from(input, pattern.flagSet.unicode)
      val canonical =
        if (pattern.flagSet.ignoreCase) UString.canonicalize(original, pattern.flagSet.unicode) else original
      IRCompiler.compile(pattern).map(VM.execute(_, canonical, pos, tracer = tracer).map(_.copy(input = original)))
    }
}
