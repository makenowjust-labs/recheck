package codes.quine.labs.resyntax.parser

/** ParsingException is an exception on parsing. */
final class ParsingException(val message: String, val pos: Option[Int]) extends Exception {
  override def getMessage: String =
    pos.fold(message)(pos => s"$message at $pos")
}
