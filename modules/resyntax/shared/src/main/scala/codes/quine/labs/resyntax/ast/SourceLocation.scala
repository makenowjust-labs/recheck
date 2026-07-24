package codes.quine.labs.resyntax.ast

/** SourceLocation is a location position on source. */
final case class SourceLocation(start: Int, end: Int)

object SourceLocation:

  /** Invalid is an invalid source location. */
  val Invalid: SourceLocation = SourceLocation(-1, -1)
