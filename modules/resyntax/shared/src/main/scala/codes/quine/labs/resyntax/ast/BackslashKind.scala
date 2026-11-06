package codes.quine.labs.resyntax.ast

sealed abstract class BackslashKind extends Product with Serializable

object BackslashKind:

  sealed abstract class BackslashValue extends BackslashKind:

    def value: Int

  final case class Unknown(value: Int) extends BackslashValue

  /** Escape is an escape of a value in the style.
    *
    * `written` is the text after its letters as written (e.g. `7f` of `\x7f`, `{0041}` of `\x{0041}`, `j` of Ruby's
    * `\C-j`, and the digits after `\` or Java's `\0` of `\012`), or `None` for the plain text of the style.
    */
  final case class Escape(style: EscapeStyle, value: Int, written: Option[String] = None) extends BackslashValue

  final case class EscapeClass(kind: EscapeClassKind) extends BackslashKind

  final case class EscapeBackReference(style: BackReferenceStyle, ref: Reference) extends BackslashKind

  /** EscapeCall is `\g<x>` escape. */
  final case class EscapeCall(style: NameStyle, ref: Reference.BaseReference) extends BackslashKind

  final case class Assert(kind: AssertKind) extends BackslashKind

  /** CaseCommand is Java's and PCRE's quoting command (`\Q` or `\E`). */
  final case class CaseCommand(kind: CaseCommandKind) extends BackslashKind
