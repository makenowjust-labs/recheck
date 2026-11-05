package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.common.UnsupportedException
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

    /** Repaired has a safe pattern, and how it compares with the original (see [[Equivalence.findCounterexample]]). */
    case Repaired(pattern: Pattern, comparison: Comparison)

    /** NotFound is the result when no edit makes the pattern safe. */
    case NotFound

  /** Comparison tells whether a repaired pattern extracts the same as the original. */
  enum Comparison:

    /** Equivalent tells that it extracts the same from every input. */
    case Equivalent

    /** Different has the shortest input on which it does not. */
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
    * after [[MaxRounds]] rounds.
    *
    * It throws [[UnsupportedException]] when the automaton checker gives up on the original pattern.
    */
  def search(original: Pattern, captures: Seq[Int], params: Parameters)(using ctx: Context): Result =
    val pairs = captures.map(i => (i, i))

    def compare(a: Pattern, b: Pattern): Comparison =
      try
        Equivalence
          .findCounterexample(a.ir, b.ir, pairs, params.maxNFASize)
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

    def loop(current: Pattern, score: Score, round: Int): (Pattern, Score) =
      if score.isSafe || round >= MaxRounds then (current, score)
      else
        val improved = current
          .candidates(score.spots)
          .flatMap: edit =>
            for
              next <- parseEdited(edit.applyTo(current.source))
              nextScore <- Score.of(next.ir, params)
              if nextScore > score
            yield (next, nextScore, edit)
          .sortBy((_, nextScore, edit) =>
            (-nextScore.rank, nextScore.count, edit.text.length - (edit.end - edit.start))
          )
        val chosen = improved
          .find((next, _, _) => compare(current, next) == Comparison.Equivalent)
          .orElse(improved.headOption)
        chosen match
          case Some((next, nextScore, _)) => loop(next, nextScore, round + 1)
          case None                       => (current, score)

    Score.of(original.ir, params) match
      case None => throw new UnsupportedException("the automaton checker cannot check the pattern")
      case Some(score) if score.isSafe => Result.Safe
      case Some(score)                 =>
        val (fixed, fixedScore) = loop(original, score, 0)
        if fixedScore.isSafe then Result.Repaired(fixed, compare(original, fixed)) else Result.NotFound

  /** Pattern is a pattern parsed in the dialect, with its syntax tree for the edits and its IR for the checks. */
  final case class Pattern(
      source: String,
      flags: String,
      dialect: Dialect,
      flagSet: FlagSet,
      node: Node,
      ir: IRPattern
  ):

    /** Lists the edits around the spots. */
    def candidates(spots: Set[SourceLocation]): Seq[Edit] =
      val features = FeatureSet.from(SourcePrescan.from(source), dialect, flagSet, ir.mode)
      new CandidateFinder(source, node, ir, features).find(spots)

  object Pattern:

    /** Parses the source.
      *
      * It throws [[InvalidRegExpException]] or [[UnsupportedException]] as [[ReDoS.parse]] does.
      */
    def parse(source: String, flags: String, dialect: Dialect)(using ctx: Context): Pattern =
      val (flagSet, node, ir) = ReDoS.parseTree(source, flags, dialect)
      Pattern(source, flags, dialect, flagSet, node, ir)

  /** Score is the vulnerabilities of a pattern, with the rank of the worst one (higher is better) and their count. */
  private final case class Score(rank: Int, count: Int, spots: Set[SourceLocation]):

    def isSafe: Boolean = count == 0

    def >(that: Score): Boolean = rank > that.rank || (rank == that.rank && count < that.count)

  private object Score:

    /** The vulnerabilities counted at most. */
    val maxCount = 16

    /** Checks the pattern by the automaton checker, or returns `None` if it gives up. */
    def of(ir: IRPattern, params: Parameters)(using ctx: Context): Option[Score] =
      try
        val vulnerable = ReDoS
          .computeAutomatonComplexities(ir, params)
          .takeWhile(_.isInstanceOf[Complexity.Vulnerable[?]])
          .collect { case v: Complexity.Vulnerable[UChar] => v }
          .take(maxCount)
          .toSeq
        vulnerable.headOption match
          case None        => Some(Score(Int.MaxValue, 0, Set.empty))
          case Some(worst) =>
            val rank = worst match
              case _: Complexity.Exponential[?] => Int.MinValue
              case p: Complexity.Polynomial[?]  => -p.degree
            val spots = worst.hotspot.spots.collect { case Hotspot.Spot(s, e, Hotspot.Hot) => SourceLocation(s, e) }
            Some(Score(rank, vulnerable.size, spots.toSet))
      catch case _: UnsupportedException => None
