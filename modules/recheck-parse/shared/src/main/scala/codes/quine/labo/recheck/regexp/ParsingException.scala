package codes.quine.labo.recheck.regexp

/** ParsingException is an exception class which is thrown on occurring parsing failure or some syntax errors can be
  * detected statically.
  */
final class ParsingException(message: String, loc: Option[(Int, Int)]) extends Exception(message) {
  override def getMessage: String = {
    val at = loc match {
      case Some((x, y)) if x == y => s" (at $x)"
      case Some((x, y))           => s" (at $x:$y)"
      case None                   => ""
    }
    s"$message$at"
  }
}
