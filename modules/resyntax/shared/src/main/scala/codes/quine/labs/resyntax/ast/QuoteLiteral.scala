package codes.quine.labs.resyntax.ast

/** QuoteLiteral is literal in `\q` escape. */
sealed abstract class QuoteLiteral extends Product with Serializable

object QuoteLiteral:

  /** QuoteValue is simple value literal. */
  final case class QuoteValue(value: Int) extends QuoteLiteral

  /** QuoteEscape is escape sequence literal. */
  final case class QuoteBackslash(kind: BackslashKind.BackslashValue) extends QuoteLiteral
