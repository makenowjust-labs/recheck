package codes.quine.labs.resyntax.ast

/** AssertKind is a kind of backslash assertion. */
sealed abstract class AssertKind extends Product with Serializable

object AssertKind {

  /** Boundary is a boundary assertion (e.g. `\b`). */
  final case class Boundary(modifier: Option[BoundaryModifier]) extends AssertKind

  /** NonBoundary is a non-boundary assertion (e.g. `\B`). */
  final case class NonBoundary(modifier: Option[BoundaryModifier]) extends AssertKind

  /** Cut is a cut assertion (e.g. `\K`). */
  case object Cut extends AssertKind

  /** Sticky is a sticky assertion (e.g. `\G`). */
  case object Sticky extends AssertKind

  /** Begin is a begin assertion (e.g. `\A`). */
  case object Begin extends AssertKind

  /** LowerEnd is an end assertion (e.g. `\z`). */
  case object LowerEnd extends AssertKind

  /** UpperEnd is an end assertion (e.g. `\Z`). */
  case object UpperEnd extends AssertKind
}
