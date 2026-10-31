package codes.quine.labs.resyntax.ast

final case class SourceLocation(start: Int, end: Int)

object SourceLocation:

  val Invalid: SourceLocation = SourceLocation(-1, -1)
