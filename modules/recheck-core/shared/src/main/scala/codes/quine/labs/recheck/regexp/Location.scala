package codes.quine.labs.recheck.regexp

import codes.quine.labs.resyntax.ast.SourceLocation

/** Location is a location of a node in a source. */
final case class Location(start: Int, end: Int)

object Location:

  /** Converts a location of resyntax, which may be invalid. */
  def from(loc: SourceLocation): Option[Location] =
    Option.when(loc != SourceLocation.Invalid)(Location(loc.start, loc.end))
