package codes.quine.labs.resyntax.ast

/** BackslashKind is a kind of backslash node. */
sealed abstract class BackslashKind extends Product with Serializable

object BackslashKind {

  /** Unknown is an unknown backslash. */
  final case class Unknown(value: Int) extends BackslashKind

  /** EscapeClass is a backslash escape class. */
  final case class EscapeClass(kind: EscapeClassKind) extends BackslashKind

  /** EscapeBackReference is a back-reference escape. */
  final case class EscapeBackReference(style: BackReferenceStyle, ref: Reference) extends BackslashKind

  /** EscapeCall is `\g<x>` escape. */
  final case class EscapeCall(style: NameStyle, ref: Reference.BaseReference) extends BackslashKind

  /** Escape is a backslash escape. */
  final case class Escape(style: EscapeStyle, value: Int) extends BackslashKind

  /** Assert is an assertion backslash. */
  final case class Assert(kind: AssertKind) extends BackslashKind

  /** CaseCommand is a case command backslash. */
  final case class CaseCommand(kind: CaseCommandKind) extends BackslashKind
}
