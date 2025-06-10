package codes.quine.labs.resyntax.ast

/** NameStyle is a named capture name style in a pattern. */
sealed abstract class NameStyle extends Product with Serializable

object NameStyle {

  /** Angle is a name wrapped around angles (e.g. `<foo>`). */
  case object Angle extends NameStyle

  /** Quote is a name wrapped around single quotes (e.g. `'foo'`). */
  case object Quote extends NameStyle

  /** Curly is a name wrapped around curly brackets (e.g. `{foo}`). */
  case object Curly extends NameStyle

  /** Bare is a name without any quote (e.g.`foo`). */
  case object Bare extends NameStyle
}
