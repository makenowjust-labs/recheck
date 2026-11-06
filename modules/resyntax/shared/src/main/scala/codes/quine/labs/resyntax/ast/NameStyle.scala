package codes.quine.labs.resyntax.ast

sealed abstract class NameStyle extends Product with Serializable

object NameStyle:

  /** Angle is a name in angle brackets (e.g. `<foo>`). */
  case object Angle extends NameStyle

  /** Quote is a name in single quotes (e.g. `'foo'`). */
  case object Quote extends NameStyle

  /** Curly is a name in curly brackets (e.g. `{foo}`). */
  case object Curly extends NameStyle

  /** Bare is a name without any quote (e.g. `foo`). */
  case object Bare extends NameStyle
