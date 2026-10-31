package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.FullCaseFolding
import codes.quine.labs.resyntax.charset.UString
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
  * @param tries
  *   the runs of the alternatives (from and until the indices) that Perl compiles into a trie, by the location of the
  *   alternation, where `(*THEN)` goes to the alternative after the run (found only with `(*THEN)`)
  */
final case class IRPattern(
    node: IRNode,
    captureCount: Int,
    captureNames: Map[String, Seq[Int]],
    backReference: IRBackReferenceSemantics,
    anchored: Boolean,
    mode: UnitMode,
    notEmpty: IRNotEmpty = IRNotEmpty.Never,
    tries: Map[SourceLocation, Seq[(Int, Int)]] = Map.empty
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

/** IRBackReferenceFolding is how a case-insensitive back-reference compares the capture with the input. */
sealed abstract class IRBackReferenceFolding extends Product with Serializable

object IRBackReferenceFolding:

  /** Simple compares them character by character with the case equivalence. */
  case object Simple extends IRBackReferenceFolding

  /** Full compares the full case foldings of the capture and the input, which must end at the end of a character of
    * both (Perl and Ruby, e.g. `(ß)\1` matches `"ßss"`, but `(s)\1` does not match `"sß"`).
    *
    * @param below
    *   the smallest code value of a folding to several characters, which a character with another folding keeps instead
    *   (`0x80` under `aa`, and `0x100` under `l`)
    */
  final case class Full(below: Int) extends IRBackReferenceFolding:

    /** Returns the folding of a character to several characters, if any. */
    def fold(c: Int): Option[UString] =
      Some(FullCaseFolding(c)).filter(key => key.length >= 2 && key.iterator.forall(_ >= below))
