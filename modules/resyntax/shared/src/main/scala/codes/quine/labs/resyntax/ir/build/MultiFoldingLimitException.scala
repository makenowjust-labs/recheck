package codes.quine.labs.resyntax.ir
package build

/** MultiFoldingLimitException is thrown when a run of literals has more multi-character case foldings than
  * [[IRCharResolver.MaxMultiFoldings]].
  *
  * Unlike [[IRBuildingException]], the pattern may be valid on the engine.
  */
final class MultiFoldingLimitException(val limit: Int, val pos: Int) extends Exception:

  override def getMessage: String = s"Too many multi-character case foldings in a literal (limit $limit) at $pos"
