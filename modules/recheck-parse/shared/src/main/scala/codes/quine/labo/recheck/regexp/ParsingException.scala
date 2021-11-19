package codes.quine.labo.recheck.regexp

import codes.quine.labo.recheck.regexp.Pattern.Location

/** ParsingException is an exception class which is thrown on occurring parsing failure or some syntax errors can be
  * detected statically.
  */
final class ParsingException(message: String, loc: Option[Location]) extends Exception(message) {
  override def getMessage: String = {
    val at = loc match {
      case Some(Location(x, y)) if x == y => s" (at $x)"
      case Some(Location(x, y))           => s" (at $x:$y)"
      case None                           => ""
    }
    s"$message$at"
  }
}
