package codes.quine.labs.resyntax.parser

/** NestingLimitException is thrown when a pattern nests deeper than [[Parser.MaxNestingDepth]].
  *
  * Unlike [[ParsingException]], the pattern may be valid on the engine.
  */
final class NestingLimitException(val limit: Int, val pos: Int) extends Exception:
  override def getMessage: String = s"Too deeply nested (limit $limit) at $pos"
