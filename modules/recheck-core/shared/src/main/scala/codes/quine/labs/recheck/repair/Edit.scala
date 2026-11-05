package codes.quine.labs.recheck
package repair

/** Edit replaces the part of a source between `start` and `end` with `text`. */
final case class Edit(start: Int, end: Int, text: String):

  /** Applies this to the source. */
  def applyTo(source: String): String = source.substring(0, start) + text + source.substring(end)
