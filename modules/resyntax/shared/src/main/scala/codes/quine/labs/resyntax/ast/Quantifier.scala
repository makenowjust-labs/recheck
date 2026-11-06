package codes.quine.labs.resyntax.ast

/** Quantifier is a repetition quantifier node.
  *
  * {{{
  * Quantifier ::=
  *   "*" BacktrackStrategy
  *   "+" BacktrackStrategy
  *   "?" BacktrackStrategy
  *   "{" Int "}" BacktrackStrategy
  *   "{" Int "," Int "}" BacktrackStrategy
  *   "{" Int ",}" BacktrackStrategy
  *   "{," Int "}" BacktrackStrategy
  * }}}
  */
sealed abstract class Quantifier extends Product with Serializable:

  def strategy: BacktrackStrategy

  /** Returns the plain text of the quantifier (e.g. `{2,3}?`), which a repeat without a written text has. */
  def plainText: String =
    val text = this match
      case Quantifier.Star(_)              => "*"
      case Quantifier.Plus(_)              => "+"
      case Quantifier.Question(_)          => "?"
      case Quantifier.Exact(n, _)          => s"{$n}"
      case Quantifier.Bounded(min, max, _) => s"{$min,$max}"
      case Quantifier.MaxBounded(max, _)   => s"{,$max}"
      case Quantifier.Unbounded(min, _)    => s"{$min,}"
    strategy match
      case BacktrackStrategy.Greedy     => text
      case BacktrackStrategy.Lazy       => s"$text?"
      case BacktrackStrategy.Possessive => s"$text+"

object Quantifier:

  /** Star is a star quantifier (e.g. `*`). */
  final case class Star(strategy: BacktrackStrategy) extends Quantifier

  /** Plus is a plus quantifier (e.g. `+`). */
  final case class Plus(strategy: BacktrackStrategy) extends Quantifier

  /** Question is a question quantifier (e.g. `?`). */
  final case class Question(strategy: BacktrackStrategy) extends Quantifier

  /** Exact is an exact number bounded quantifier (e.g. `{2}`). */
  final case class Exact(n: Int, strategy: BacktrackStrategy) extends Quantifier

  /** Bounded is a bounded quantifier (e.g. `{2,3}`). */
  final case class Bounded(min: Int, max: Int, strategy: BacktrackStrategy) extends Quantifier

  /** MaxBounded is an alternative bounded quantifier (e.g. `{,3}`). */
  final case class MaxBounded(max: Int, strategy: BacktrackStrategy) extends Quantifier

  /** Unbounded is an unbounded quantifier (e.g. `{2,}`). */
  final case class Unbounded(min: Int, strategy: BacktrackStrategy) extends Quantifier
