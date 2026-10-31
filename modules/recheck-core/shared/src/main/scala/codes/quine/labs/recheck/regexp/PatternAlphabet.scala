package codes.quine.labs.recheck.regexp

import scala.util.hashing.MurmurHash3

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
    * The kind of an atom tells which word sets (`word` first, then `moreWords`) and which line terminator sets
    * (`lineTerminator` first, then `moreLineTerminators`) have it, and, with `crlf`, whether it is `\r` or `\n`, which
    * are then atoms by themselves.
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
      preferred: Option[CharSet] = None,
      crlf: Boolean = false,
      moreLineTerminators: Seq[CharSet] = Seq.empty,
      moreWords: Seq[CharSet] = Seq.empty
  ): PatternAlphabet =
    val crlfSets = if crlf then Seq(CharSet.single(0x0d), CharSet.single(0x0a)) else Seq.empty
    val terminators = lineTerminator.toSeq ++ moreLineTerminators
    val words = word.toSeq ++ moreWords
    val all = ((mode.universe +: terminators) ++ words ++ (sets ++ crlfSets).distinct)
      .map(_.restrict(mode))
      .filter(_.nonEmpty)
      .toIndexedSeq
    def narrow(c: CharSet): CharSet =
      preferred.map(c.intersect).filter(_.nonEmpty).getOrElse(c)
    def order(c: CharSet): Seq[Int] =
      val first = all.indexWhere(_.contains(c.head))
      first +: all.drop(first + 1).map(s => if s.contains(c.head) then 0 else 1)
    // The bits are of the sets as given, even if `lineTerminator` (resp. `word`) is `None`.
    def mask(first: Option[CharSet], more: Seq[CharSet], c: Int): Int =
      (first +: more.map(Some(_))).zipWithIndex.collect { case (Some(t), i) if t.contains(c) => 1 << i }.sum
    val pairs = Alphabet
      .refine(mode, all)
      .atoms
      .map(narrow)
      .map(c => (c, order(c)))
      .sortBy(_._2)(using Ordering.Implicits.seqOrdering)
      .map: (c, _) =>
        val kind = CharKind(
          words = mask(word, moreWords, c.head),
          terminators = mask(lineTerminator, moreLineTerminators, c.head),
          cr = crlf && c.head == 0x0d,
          lf = crlf && c.head == 0x0a
        )
        (c, kind)
    PatternAlphabet(pairs)

  /** CharKind is a minimum character information for assertion check.
    *
    * The last three are guesses on the context, which the automaton checks with the next (resp. previous) kind.
    *
    * @param words
    *   the bits of the word sets having the character
    * @param terminators
    *   the bits of the line terminator sets having the character
    * @param cr
    *   whether it is `\r`, which is distinguished for `\r\n`
    * @param lf
    *   whether it is `\n`, which is distinguished for `\r\n`
    * @param boundary
    *   whether it is the begin or the end of the input instead of a character
    * @param last
    *   whether the character is the last of the input
    * @param afterCR
    *   whether `\r` precedes the `\n`
    * @param follow
    *   what follows the `\r`
    */
  final case class CharKind(
      words: Int = 0,
      terminators: Int = 0,
      cr: Boolean = false,
      lf: Boolean = false,
      boundary: Boolean = false,
      last: Option[Boolean] = None,
      afterCR: Option[Boolean] = None,
      follow: Option[CRFollow] = None
  ):

    /** Checks whether it is a character of the first word set. */
    def word: Boolean = isWordOf(0)

    /** Checks whether it is a character of the `i`-th word set. */
    def isWordOf(i: Int): Boolean = (words & (1 << i)) != 0

    /** Checks whether it is a character of a line terminator set. */
    def isLineTerminator: Boolean = terminators != 0

    /** Checks whether it is a character of the `i`-th line terminator set. */
    def isLineTerminatorOf(i: Int): Boolean = (terminators & (1 << i)) != 0

    // The named kinds hash by their names, as the enum values did, which keeps the orders of the automaton's states.
    override def hashCode: Int = this match
      case CharKind.Normal | CharKind.LineTerminator | CharKind.Word | CharKind.Boundary => toString.hashCode
      case _ => MurmurHash3.productHash(this)

    override def toString: String = this match
      case CharKind.Normal         => "Normal"
      case CharKind.LineTerminator => "LineTerminator"
      case CharKind.Word           => "Word"
      case CharKind.Boundary       => "Boundary"
      case _                       =>
        val flags = Seq(
          Option.when(words != 0)(s"words=$words"),
          Option.when(terminators != 0)(s"terminators=$terminators"),
          Option.when(cr)("cr"),
          Option.when(lf)("lf"),
          Option.when(boundary)("boundary"),
          last.map(l => s"last=$l"),
          afterCR.map(a => s"afterCR=$a"),
          follow.map(f => s"follow=$f")
        ).flatten
        flags.mkString("CharKind(", ",", ")")

  object CharKind:

    /** A character kind for normal character. */
    val Normal: CharKind = CharKind()

    /** A character kind for line terminator character (of the first set). */
    val LineTerminator: CharKind = CharKind(terminators = 1)

    /** A character kind for word character. */
    val Word: CharKind = CharKind(words = 1)

    /** The kind of the begin and the end of the input, where there is no character. */
    val Boundary: CharKind = CharKind(boundary = true)

    /** The basic kinds, which the original assertions tell about. */
    val basic: Seq[CharKind] = Seq(Normal, LineTerminator, Word)

  /** CRFollow is what follows `\r`. */
  enum CRFollow:

    /** Neither `\n` nor the end. */
    case Other

    /** `\n` that is not the last character. */
    case LF

    /** `\n` that is the last character. */
    case FinalLF
