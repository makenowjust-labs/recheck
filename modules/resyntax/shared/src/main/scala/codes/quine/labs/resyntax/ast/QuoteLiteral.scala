package codes.quine.labs.resyntax.ast

/** QuoteLiteral is a literal in a `\q` escape. */
sealed abstract class QuoteLiteral extends Product with Serializable

object QuoteLiteral:

  final case class QuoteValue(value: Int) extends QuoteLiteral

  final case class QuoteBackslash(kind: BackslashKind.BackslashValue) extends QuoteLiteral
