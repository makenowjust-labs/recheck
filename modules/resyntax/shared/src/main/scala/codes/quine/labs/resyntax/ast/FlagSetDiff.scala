package codes.quine.labs.resyntax.ast

/** FlagSetDiff is a flag set character diff in inline flag. */
final case class FlagSetDiff(added: FlagSet, removed: Option[FlagSet])
