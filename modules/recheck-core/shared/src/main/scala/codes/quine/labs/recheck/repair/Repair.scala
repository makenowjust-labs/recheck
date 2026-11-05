package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.parser.FeatureSet
import codes.quine.labs.resyntax.parser.SourcePrescan

/** Repair searches for a safe pattern that extracts the same as a vulnerable one. */
private[recheck] object Repair:

  /** Result is a result of the search. */
  enum Result:

    /** Safe is the result for a pattern without vulnerabilities. */
    case Safe

    /** Repaired has a better pattern, the indices of the kept captures in it, its complexity, and how it compares with
      * the original.
      */
    case Repaired(pattern: Pattern, captures: Seq[Int], complexity: AttackComplexity, comparison: Comparison)

    /** NotFound is the result when no edit makes the pattern better. */
    case NotFound

  /** Comparison tells whether a repaired pattern extracts the same as the original. */
  enum Comparison:

    /** Equivalent tells that it extracts the same from every input. */
    case Equivalent

    /** Different has the shortest input on which it does not (see [[Equivalence.findCounterexample]]). */
    case Different(input: UString)

    /** Unknown tells that the comparison is not supported (e.g. for a back-reference). */
    case Unknown

  /** The rounds of edits that a search tries at most. */
  val MaxRounds = 8

  /** Searches for a safe pattern by edits around the hotspots, preferring the ones that keep the extraction, where
    * `captures` are the indices of the captures to keep and `0` is the whole match.
    *
    * The automaton checker checks the patterns with `params` (see [[ReDoS.computeAutomatonComplexities]]). Each round
    * takes the best edit by the complexity after it, and it stops when the pattern is safe, no edit makes it better, or
    * after [[MaxRounds]] rounds. An edit may remove a capture not kept, which renumbers the later ones.
    *
    * It throws [[UnsupportedException]] when the automaton checker gives up on the original pattern.
    */
  def search(original: Pattern, captures: Seq[Int], params: Parameters)(using ctx: Context): Result =
    def compare(a: Pattern, aIndices: Seq[Int], b: Pattern, bIndices: Seq[Int]): Comparison =
      try
        Equivalence
          .findCounterexample(a.ir, b.ir, aIndices.zip(bIndices), params.maxNFASize)
          .fold(Comparison.Equivalent)(Comparison.Different(_))
      catch case _: UnsupportedException => Comparison.Unknown

    def parseEdited(source: String): Option[Pattern] =
      try Some(Pattern.parse(source, original.flags, original.dialect))
      catch
        case _: UnsupportedException => None
        // An edit makes a valid pattern, though the parser may not support it.
        // $COVERAGE-OFF$
        case ex: InvalidRegExpException =>
          throw new UnexpectedException(s"an edit makes an invalid pattern /$source/: ${ex.getMessage}")
        // $COVERAGE-ON$

    // `indices` are the current indices of `captures`.
    def loop(current: Pattern, indices: Seq[Int], score: Score, round: Int): (Pattern, Seq[Int], Score) =
      if score.isSafe || round >= MaxRounds then (current, indices, score)
      else
        val improved = current
          .candidates(score.spots, indices.toSet)
          .flatMap: candidate =>
            for
              next <- parseEdited(candidate.edit.applyTo(current.source))
              nextScore <- Score.of(next.ir, params)
              if nextScore > score
            yield
              val nextIndices = indices.map(i => i - candidate.removedCaptures.count(_ < i))
              (next, nextIndices, nextScore, candidate.edit)
          .sortBy((_, _, nextScore, edit) =>
            (-nextScore.rank, nextScore.count, edit.text.length - (edit.end - edit.start))
          )
        val chosen = improved
          .find((next, nextIndices, _, _) => compare(current, indices, next, nextIndices) == Comparison.Equivalent)
          .orElse(improved.headOption)
        chosen match
          case Some((next, nextIndices, nextScore, _)) => loop(next, nextIndices, nextScore, round + 1)
          case None                                    => (current, indices, score)

    Score.of(original.ir, params) match
      case None => throw new UnsupportedException("the automaton checker cannot check the pattern")
      case Some(score) if score.isSafe => Result.Safe
      case Some(score)                 =>
        val (fixed, indices, fixedScore) = loop(original, captures, score, 0)
        if fixed eq original then Result.NotFound
        else Result.Repaired(fixed, indices, fixedScore.complexity, compare(original, captures, fixed, indices))

  /** Pattern is a pattern parsed in the dialect, with its syntax tree for the edits and its IR for the checks. */
  final case class Pattern(
      source: String,
      flags: String,
      dialect: Dialect,
      flagSet: FlagSet,
      node: Node,
      ir: IRPattern
  ):

    /** Lists the candidates around the spots, which keep the captures of `kept`. */
    def candidates(spots: Set[SourceLocation], kept: Set[Int]): Seq[Candidate] =
      val features = FeatureSet.from(SourcePrescan.from(source), dialect, flagSet, ir.mode)
      new CandidateFinder(source, node, ir, features, kept).find(spots)

  object Pattern:

    /** Parses the source.
      *
      * It throws [[InvalidRegExpException]] or [[UnsupportedException]] as [[ReDoS.parse]] does.
      */
    def parse(source: String, flags: String, dialect: Dialect)(using ctx: Context): Pattern =
      val (flagSet, node, ir) = ReDoS.parseTree(source, flags, dialect)
      Pattern(source, flags, dialect, flagSet, node, ir)

  /** Score is the vulnerabilities of a pattern, with the rank of the worst one (higher is better), their count, the
    * spots of the worst one, and the complexity.
    */
  private final case class Score(rank: Int, count: Int, spots: Set[SourceLocation], complexity: AttackComplexity):

    def isSafe: Boolean = count == 0

    def >(that: Score): Boolean = rank > that.rank || (rank == that.rank && count < that.count)

  private object Score:

    /** The vulnerabilities counted at most. */
    val maxCount = 16

    /** Checks the pattern by the automaton checker, or returns `None` if it gives up. */
    def of(ir: IRPattern, params: Parameters)(using ctx: Context): Option[Score] =
      try
        val (complexities, rest) =
          ReDoS.computeAutomatonComplexities(ir, params).span(_.isInstanceOf[Complexity.Vulnerable[?]])
        val vulnerable = complexities.collect { case v: Complexity.Vulnerable[UChar] => v }.take(maxCount).toSeq
        vulnerable.headOption match
          case None        => Some(Score(Int.MaxValue, 0, Set.empty, ReDoS.toSafeComplexity(rest)))
          case Some(worst) =>
            val rank = worst match
              case _: Complexity.Exponential[?] => Int.MinValue
              case p: Complexity.Polynomial[?]  => -p.degree
            val spots = worst.hotspot.spots.collect { case Hotspot.Spot(s, e, Hotspot.Hot) => SourceLocation(s, e) }
            Some(Score(rank, vulnerable.size, spots.toSet, worst.toAttackComplexity))
      catch case _: UnsupportedException => None
