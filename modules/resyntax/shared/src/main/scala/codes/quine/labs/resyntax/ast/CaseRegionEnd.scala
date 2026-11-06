package codes.quine.labs.resyntax.ast

/** CaseRegionEnd is how a case region ends. */
sealed abstract class CaseRegionEnd extends Product with Serializable

object CaseRegionEnd:

  /** Closed is the end with `\E`. */
  case object Closed extends CaseRegionEnd

  /** Unclosed is the end without `\E`, at the end of the pattern or at the `\E` of the region around it. */
  case object Unclosed extends CaseRegionEnd

  /** NextCharacter is the end of Perl's `\u` or `\l` without `\E`, whose region is the next character, since they
    * change only it.
    */
  case object NextCharacter extends CaseRegionEnd

  /** ClosedBeforeQuantifier is the end with `\E` before the quantifier of the last part (e.g. `\Qab\E*`, which is
    * `ab*`).
    *
    * The last part is a repeat, and its text is the one of the repeated node.
    */
  case object ClosedBeforeQuantifier extends CaseRegionEnd
