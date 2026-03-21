package codes.quine.labs.resyntax.ast

/** BackReferenceStyle is a style for back-reference escape. */
sealed abstract class BackReferenceStyle extends Product with Serializable

object BackReferenceStyle:

  /** BareBackReference is `\1` back-reference escape style. */
  case object BareBackReference extends BackReferenceStyle

  /** GBackReference is `\g` back-reference escape style. */
  final case class GBackReference(style: NameStyle) extends BackReferenceStyle

  /** KBackReference is `\k` back-reference escape style. */
  final case class KBackReference(style: NameStyle) extends BackReferenceStyle
