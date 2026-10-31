package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.SourceLocation

/** IRBuildingException is an exception on building IR, for an error that needs the whole pattern. */
final class IRBuildingException(val message: String, val loc: SourceLocation) extends Exception:
  override def getMessage: String =
    if loc == SourceLocation.Invalid then message else s"$message at ${loc.start}"
