package codes.quine.labo.recheck
package backtrack

import scala.util.Try

import common.Context
import data.UString
import regexp.Pattern

/** RegExp pattern matching Engine (frontend). */
object Engine {

  /** Tests a matching of the pattern on the input. */
  def matches(pattern: Pattern, input: String, pos: Int = 0)(implicit ctx: Context): Try[Option[Match]] = {
    val original = UString.from(input, pattern.flagSet.unicode)
    val canonical =
      if (pattern.flagSet.ignoreCase) UString.canonicalize(original, pattern.flagSet.unicode) else original
    IRCompiler.compile(pattern).map(VM.execute(_, canonical, pos).map(_.copy(input = original)))
  }
}
