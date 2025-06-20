package codes.quine.labs.resyntax.ast

/** BoundaryModifier is a kind of boundary modifier. */
sealed abstract class BoundaryModifier extends Product with Serializable

object BoundaryModifier {

  /** GModifier is a modifier of `\b{g}`. */
  case object GModifier extends BoundaryModifier

  /** GcbModifier is a modifier of `\b{gcb}`. */
  case object GcbModifier extends BoundaryModifier

  /** LbModifier is a modifier of `\b{lb}`. */
  case object LbModifier extends BoundaryModifier

  /** SbModifier is a modifier of `\b{sb}`. */
  case object SbModifier extends BoundaryModifier

  /** WbModifier is a modifier of `\b{wb}`. */
  case object WbModifier extends BoundaryModifier
}
