package codes.quine.labs.resyntax.ir.alphabet

import codes.quine.labs.resyntax.charset.Alphabet
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRAssertKind
import codes.quine.labs.resyntax.ir.IRLineTerminatorSet
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData.*
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.IRSurrogateKind
import codes.quine.labs.resyntax.ir.IRWordMarkSet

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

  /** The pairs for a pattern matching any character. */
  def any: Set[(CharSet, CharKind)] = pairs.toSet

/** PatternAlphabet utilities. */
object PatternAlphabet:

  /** Builds the coarsest alphabet of the characters of the input such that each set is a union of atoms.
    *
    * The characters are those of the mode, without the surrogates if they are `Absent`.
    *
    * The kind of an atom tells which word sets (`word` first, then `moreWords`) and which line terminator sets
    * (`lineTerminator` first, then `moreLineTerminators`) have it, which sets of `wordMarks` have it, and, with `crlf`,
    * whether it is `\r` or `\n`, which are then atoms by themselves.
    *
    * With `Paired` surrogates, an atom of surrogates alone is split into the high and the low ones, and the kinds tell
    * an atom of high surrogates and one of low surrogates if both exist, since a high surrogate followed by a low one
    * is one character.
    *
    * The atoms are ordered as splitting all characters, `lineTerminator`, `word` and `sets` in turn: an atom of an
    * earlier set comes first, and a split atom puts the part in the set first.
    */
  def build(
      mode: UnitMode,
      sets: Seq[CharSet] = Seq.empty,
      lineTerminator: Option[CharSet] = None,
      word: Option[CharSet] = None,
      crlf: Boolean = false,
      moreLineTerminators: Seq[CharSet] = Seq.empty,
      moreWords: Seq[CharSet] = Seq.empty,
      wordMarks: Seq[IRWordMarkSet] = Seq.empty,
      surrogates: IRSurrogateKind = IRSurrogateKind.Separate
  ): PatternAlphabet =
    val crlfSets = if crlf then Seq(CharSet.single(0x0d), CharSet.single(0x0a)) else Seq.empty
    val terminators = lineTerminator.toSeq ++ moreLineTerminators
    val words = word.toSeq ++ moreWords
    val markSets = wordMarks.flatMap(m => Seq(m.marks, m.extenders, m.bases))
    val universe = surrogates.universe(mode)
    val all = ((universe +: terminators) ++ words ++ markSets ++ (sets ++ crlfSets).distinct)
      .map(_.restrict(mode))
      .filter(_.nonEmpty)
      .toIndexedSeq
    def order(c: CharSet): Seq[Int] =
      val first = all.indexWhere(_.contains(c.head))
      first +: all.drop(first + 1).map(s => if s.contains(c.head) then 0 else 1)

    // The flags of a dialect give only a few distinct sets, so these hold for every pattern.
    validateBitCount(1 + moreWords.size)
    validateBitCount(1 + moreLineTerminators.size)
    validateBitCount(wordMarks.size)

    // The bits are of the sets as given, even if `lineTerminator` (resp. `word`) is `None`.
    def mask(sets: Seq[Option[CharSet]], c: UChar): Int =
      sets.zipWithIndex.collect { case (Some(t), i) if t.contains(c) => 1 << i }.sum

    val atoms = Alphabet
      .refine(mode, all)
      .atoms
      .filter(c => universe.contains(c.head))
      .map(c => (c, order(c)))
      .sortBy(_._2)(using Ordering.Implicits.seqOrdering)
      .flatMap: (c, _) =>
        if surrogates == IRSurrogateKind.Paired && c.subsetOf(IRSurrogateKind.All) then
          Seq(c.intersect(IRSurrogateKind.High), c.intersect(IRSurrogateKind.Low)).filter(_.nonEmpty)
        else Seq(c)
    val tellsSurrogates = surrogates == IRSurrogateKind.Paired &&
      atoms.exists(_.subsetOf(IRSurrogateKind.High)) && atoms.exists(_.subsetOf(IRSurrogateKind.Low))

    val pairs = atoms
      .map: c =>
        val kind = CharKind(
          words = mask(word +: moreWords.map(Some(_)), c.head),
          terminators = mask(lineTerminator +: moreLineTerminators.map(Some(_)), c.head),
          marks = mask(wordMarks.map(m => Some(m.marks)), c.head),
          extenders = mask(wordMarks.map(m => Some(m.extenders)), c.head),
          bases = mask(wordMarks.map(m => Some(m.bases)), c.head),
          cr = crlf && c.head == 0x0d,
          lf = crlf && c.head == 0x0a,
          high = tellsSurrogates && c.subsetOf(IRSurrogateKind.High),
          low = tellsSurrogates && c.subsetOf(IRSurrogateKind.Low)
        )
        (c, kind)
    PatternAlphabet(pairs)

  /** Computes the alphabet of the pattern.
    *
    * Its kinds tell the word sets of [[wordSets]], the line terminators of [[kindLineTerminators]] and the marks of
    * [[wordMarks]] by their indices.
    */
  def of(pattern: IRPattern): PatternAlphabet =
    val universe = pattern.universe
    def loop(node: IRNode): Seq[CharSet] = node.data match
      case Chars(set) =>
        // A set like a negated class takes part in the order of atoms as its complement.
        Vector(if prefersComplement(set, universe) then set.complement(universe) else set)
      case Strings(set) =>
        set.strings.toVector.flatMap(_.iterator.map(CharSet.single)) :+ set.chars
      case _ => node.children.flatMap(loop)

    val terminatorSets = pattern.assertionLineTerminators.flatMap: t =>
      val chars = CharSet.of(t.chars.toSeq*)
      if t.crlf == IRLineTerminatorSet.CRLF.Ignored then Seq(chars)
      else Seq(chars, CharSet.of(0x0d), CharSet.of(0x0a))
    val words = wordSets(pattern)
    val kinds = kindLineTerminators(pattern).map(t => CharSet.of(t.chars.toSeq*))
    val lineTerminator = kinds.headOption.filter(_.nonEmpty)
    val extra = terminatorSets.filterNot(kinds.contains)

    build(
      pattern.mode,
      loop(pattern.node) ++ extra,
      lineTerminator,
      words.headOption,
      needsCRLFDistinction(pattern),
      kinds.drop(1),
      words.drop(1),
      wordMarks(pattern),
      pattern.surrogates
    )

  /** Checks whether the set is described more simply by its complement, as a negated class or `.` is. */
  def prefersComplement(set: CharSet, universe: CharSet): Boolean =
    set.complement(universe).intervalCount < set.intervalCount

  /** Validates that the sets fit in the bits of an `Int` of [[CharKind]]. */
  private def validateBitCount(count: Int): Unit =
    require(count <= Integer.SIZE, s"Too many sets for the bits of CharKind: $count")

  /** Returns the distinct word sets of the word boundary assertions, in the order of the bits of [[CharKind.words]]. */
  def wordSets(pattern: IRPattern): Seq[CharSet] =
    pattern.node
      .collect:
        case Assert(IRAssertKind.WordBoundary(w))           => w
        case Assert(IRAssertKind.NotWordBoundary(w))        => w
        case Assert(IRAssertKind.MarkWordBoundary(w, _))    => w
        case Assert(IRAssertKind.NotMarkWordBoundary(w, _)) => w
      .distinct

  /** Returns the distinct marks of the word boundary assertions taking marks after a base as word characters, in the
    * order of the bits of [[CharKind.marks]].
    */
  def wordMarks(pattern: IRPattern): Seq[IRWordMarkSet] =
    pattern.node
      .collect:
        case Assert(IRAssertKind.MarkWordBoundary(_, m))    => m
        case Assert(IRAssertKind.NotMarkWordBoundary(_, m)) => m
      .distinct

  /** Returns the distinct line terminators that the kinds tell, which are of the line assertions and the text end
    * assertions in the middle, in the order of the bits of [[CharKind.terminators]].
    */
  def kindLineTerminators(pattern: IRPattern): Seq[IRLineTerminatorSet] =
    val chomps = pattern.chompTextEndsInMiddle.collect { case IRNode(Assert(IRAssertKind.ChompTextEnd(t)), _) => t }
    (pattern.node.collect {
      case Assert(IRAssertKind.LineBegin(t, _)) => t
      case Assert(IRAssertKind.LineEnd(t))      => t
    } ++ chomps).distinct

  /** Checks whether the kinds tell `\r` and `\n`, for an assertion seeing `\r\n` as one line terminator. */
  def needsCRLFDistinction(pattern: IRPattern): Boolean =
    pattern.assertionLineTerminators.exists(_.crlf == IRLineTerminatorSet.CRLF.Atomic) ||
      pattern.node.exists(_ == Assert(IRAssertKind.NotBetweenCRLF)) ||
      kindLineTerminators(pattern).exists(t =>
        t.crlf != IRLineTerminatorSet.CRLF.Ignored && !(t.chars(0x0a) && t.chars(0x0d))
      ) ||
      pattern.chompTextEndsInMiddle.exists:
        case IRNode(Assert(IRAssertKind.ChompTextEnd(t)), _) => t.crlf != IRLineTerminatorSet.CRLF.Ignored
        case _                                               => false
