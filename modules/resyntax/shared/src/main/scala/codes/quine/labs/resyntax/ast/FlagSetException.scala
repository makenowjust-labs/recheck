package codes.quine.labs.resyntax.ast

final class FlagSetException(val message: String, val pos: Option[Int]) extends Exception {
  override def getMessage: String =
    pos.fold(message)(pos => s"$message at $pos")
}
