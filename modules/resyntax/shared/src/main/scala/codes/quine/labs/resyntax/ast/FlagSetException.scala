package codes.quine.labs.resyntax.ast

/** FlagSetException is an exception on parsing flag set string. */
final class FlagSetException(val message: String, val pos: Option[Int]) extends Exception:
  override def getMessage: String =
    pos.fold(message)(pos => s"$message at $pos")
