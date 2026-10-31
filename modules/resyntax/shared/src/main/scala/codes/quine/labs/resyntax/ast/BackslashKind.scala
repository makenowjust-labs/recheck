package codes.quine.labs.resyntax.ast

sealed abstract class BackslashKind extends Product with Serializable

object BackslashKind:

  sealed abstract class BackslashValue extends BackslashKind:
    def value: Int

  final case class Unknown(value: Int) extends BackslashValue

  final case class Escape(style: EscapeStyle, value: Int) extends BackslashValue

  final case class EscapeClass(kind: EscapeClassKind) extends BackslashKind

  final case class EscapeBackReference(style: BackReferenceStyle, ref: Reference) extends BackslashKind

  /** EscapeCall is `\g<x>` escape. */
  final case class EscapeCall(style: NameStyle, ref: Reference.BaseReference) extends BackslashKind

  final case class Assert(kind: AssertKind) extends BackslashKind

  /** CaseCommand is Java's and PCRE's quoting command (`\Q` or `\E`). */
  final case class CaseCommand(kind: CaseCommandKind) extends BackslashKind
