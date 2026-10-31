package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.charset.UnitMode

/** IRPattern is a whole pattern in the internal representation.
  *
  * @param node
  *   the root node
  * @param captureCount
  *   the largest capture index, or `0` without captures
  * @param captureNames
  *   the capture indices of each name, in the order of the groups
  * @param backReference
  *   how back-references behave in the dialect
  * @param anchored
  *   whether a match starts only at the starting position (JavaScript `y`, PCRE `A`)
  * @param mode
  *   the unit of the input, which the character sets of the nodes consist of
  * @param notEmpty
  *   where an empty match is rejected (PCRE's `(*NOTEMPTY)` and `(*NOTEMPTY_ATSTART)`)
  */
final case class IRPattern(
    node: IRNode,
    captureCount: Int,
    captureNames: Map[String, Seq[Int]],
    backReference: IRBackReferenceSemantics,
    anchored: Boolean,
    mode: UnitMode,
    notEmpty: IRNotEmpty = IRNotEmpty.Never
)

/** IRNotEmpty is where an empty match is rejected, which makes the engine backtrack further.
  *
  * The match is the reported one, which starts at the last `\K`.
  */
sealed abstract class IRNotEmpty extends Product with Serializable

object IRNotEmpty:

  /** Never rejects no match. */
  case object Never extends IRNotEmpty

  /** Anywhere rejects an empty match at any position (`(*NOTEMPTY)`). */
  case object Anywhere extends IRNotEmpty

  /** AtStart rejects an empty match at the start of the search (`(*NOTEMPTY_ATSTART)`). */
  case object AtStart extends IRNotEmpty

/** IRBackReferenceSemantics is how back-references behave, which differs among dialects.
  *
  * @param unsetMatchesEmpty
  *   whether a reference to a capture that is not set matches the empty string (JavaScript), or fails (the others)
  * @param choice
  *   how a reference with more than one index chooses the capture
  */
final case class IRBackReferenceSemantics(unsetMatchesEmpty: Boolean, choice: IRBackReferenceChoice)

/** IRBackReferenceChoice is how a back-reference with more than one index chooses the capture. */
sealed abstract class IRBackReferenceChoice extends Product with Serializable

object IRBackReferenceChoice:

  /** FirstSet uses the first index whose capture is set (JavaScript, PCRE, Perl). */
  case object FirstSet extends IRBackReferenceChoice

  /** LastMatching tries the indices from the last, and uses the first one whose capture is set and matches, without
    * backtracking to the others (Ruby).
    */
  case object LastMatching extends IRBackReferenceChoice
