package codes.quine.labs.resyntax.ast

/** Dialect is a regular expression dialect specifier. */
sealed abstract class Dialect extends Product with Serializable

object Dialect {

  /** DotNet specifies .NET dialect. */
  case object DotNet extends Dialect

  /** Java specifies Java dialect. */
  case object Java extends Dialect

  /** JavaScript specifies JavaScript (ECMA-262) dialect. */
  case object JavaScript extends Dialect

  /** PCRE specifies PCRE dialect. */
  case object PCRE extends Dialect

  /** Perl specifies Perl dialect. */
  case object Perl extends Dialect

  /** Python specifies Python dialect. */
  case object Python extends Dialect

  /** Ruby specifies Ruby dialect. */
  case object Ruby extends Dialect
}
