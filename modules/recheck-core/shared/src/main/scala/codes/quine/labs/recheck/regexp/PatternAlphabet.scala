package codes.quine.labs.recheck.regexp

import codes.quine.labs.recheck.regexp.PatternAlphabet.*
import codes.quine.labs.resyntax.charset.Alphabet
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode

/** PatternAlphabet is a set of pairs of a disjoint character set (an atom) and a character kind. */
final case class PatternAlphabet(pairs: IndexedSeq[(CharSet, CharKind)]):

  /** An alphabet whose atoms are the pairs' sets and the rest. */
  private lazy val alphabet: Alphabet = Alphabet.refine(UnitMode.CodePoint, pairs.map(_._1))

  /** A map from an atom index of [[alphabet]] to the index of its pair, or `-1` for the rest. */
  private lazy val pairIndices: Array[Int] =
    val indices = Array.fill(alphabet.size)(-1)
    for ((c, _), i) <- pairs.zipWithIndex do indices(alphabet.atomOf(c.head)) = i
    indices

  /** Splits the set into the pairs contained in it. */
  def refine(c: CharSet): Set[(CharSet, CharKind)] =
    alphabet
      .atomsOf(c)
      .iterator
      .map(pairIndices)
      .filter(i => i >= 0 && pairs(i)._1.subsetOf(c))
      .map(pairs)
      .toSet

  /** Splits the complement of the set into the pairs. */
  def refineInvert(c: CharSet): Set[(CharSet, CharKind)] = any.diff(refine(c))

  /** The pairs for a 'any' pattern. */
  def any: Set[(CharSet, CharKind)] = pairs.toSet

  /** The pairs for a 'dot' pattern. */
  def dot: Set[(CharSet, CharKind)] = any.diff(newline)

  /** The pairs of the line terminator characters. */
  def newline: Set[(CharSet, CharKind)] = refine(EcmaScriptChars.LineTerminator)

/** PatternAlphabet utilities. */
object PatternAlphabet:

  /** Builds the coarsest alphabet of all characters such that each set is a union of atoms.
    *
    * With `ignoreCase`, all characters means the canonical ones. An atom in `lineTerminator` (resp. `word`) has the
    * kind [[CharKind.LineTerminator]] (resp. [[CharKind.Word]]).
    *
    * The atoms are ordered as splitting all characters, `lineTerminator`, `word` and `sets` in turn: an atom of an
    * earlier set comes first, and a split atom puts the part in the set first.
    */
  def build(
      ignoreCase: Boolean,
      unicode: Boolean,
      sets: Seq[CharSet] = Seq.empty,
      lineTerminator: Option[CharSet] = None,
      word: Option[CharSet] = None
  ): PatternAlphabet =
    val all = ((EcmaScriptChars.dot(ignoreCase, true, unicode) +: lineTerminator.toSeq) ++ word ++ sets.distinct)
      .filter(_.nonEmpty)
      .toIndexedSeq
    def order(c: CharSet): Seq[Int] =
      val first = all.indexWhere(_.contains(c.head))
      first +: all.drop(first + 1).map(s => if s.contains(c.head) then 0 else 1)
    val pairs = Alphabet
      .refine(EcmaScriptChars.mode(unicode), all)
      .atoms
      .filter(c => all.exists(_.contains(c.head)))
      .map(c => (c, order(c)))
      .sortBy(_._2)(using Ordering.Implicits.seqOrdering)
      .map: (c, _) =>
        val kind =
          if lineTerminator.exists(_.contains(c.head)) then CharKind.LineTerminator
          else if word.exists(_.contains(c.head)) then CharKind.Word
          else CharKind.Normal
        (c, kind)
    PatternAlphabet(pairs)

  /** CharKind is a minimum character information for assertion check. */
  enum CharKind:

    /** A character kind for normal character. */
    case Normal

    /** A character kind for line terminator character. */
    case LineTerminator

    /** A character kind for word character. */
    case Word
