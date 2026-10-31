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

/** PatternAlphabet utilities. */
object PatternAlphabet:

  /** Builds the coarsest alphabet of all characters of the mode such that each set is a union of atoms.
    *
    * An atom in `lineTerminator` (resp. `word`) has the kind [[CharKind.LineTerminator]] (resp. [[CharKind.Word]]).
    *
    * An atom is narrowed to its characters in `preferred` if any, so that its first character, which stands for the
    * atom in generated strings, is a preferred one (e.g. a canonical character of JavaScript's ignore-case matching).
    * Each set is still a union of the narrowed atoms intersecting it.
    *
    * The atoms are ordered as splitting all characters, `lineTerminator`, `word` and `sets` in turn: an atom of an
    * earlier set comes first, and a split atom puts the part in the set first.
    */
  def build(
      mode: UnitMode,
      sets: Seq[CharSet] = Seq.empty,
      lineTerminator: Option[CharSet] = None,
      word: Option[CharSet] = None,
      preferred: Option[CharSet] = None
  ): PatternAlphabet =
    val all = ((mode.universe +: lineTerminator.toSeq) ++ word ++ sets.distinct)
      .map(_.restrict(mode))
      .filter(_.nonEmpty)
      .toIndexedSeq
    def narrow(c: CharSet): CharSet =
      preferred.map(c.intersect).filter(_.nonEmpty).getOrElse(c)
    def order(c: CharSet): Seq[Int] =
      val first = all.indexWhere(_.contains(c.head))
      first +: all.drop(first + 1).map(s => if s.contains(c.head) then 0 else 1)
    val pairs = Alphabet
      .refine(mode, all)
      .atoms
      .map(narrow)
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
