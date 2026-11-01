package codes.quine.labs.resyntax.ast

final case class SourceLocation(start: Int, end: Int):

  /** Returns the location, or `None` if it is [[SourceLocation.Invalid]]. */
  def toOption: Option[SourceLocation] = Option.when(this != SourceLocation.Invalid)(this)

object SourceLocation:

  val Invalid: SourceLocation = SourceLocation(-1, -1)
