package codes.quine.labs.recheck
package repair

import scala.collection.mutable

import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.fuzz.FuzzChecker
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.parser.FeatureSet
import codes.quine.labs.resyntax.parser.SourcePrescan
import codes.quine.labs.resyntax.printer.Printer

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

  /** The edits that a round compares at most by the inputs they differ on, without an equivalent one. */
  val MaxDifferent = 8

  /** Searches for a safe pattern by edits around the hotspots, preferring the ones that keep the extraction, where
    * `captures` are the indices of the captures to keep and `0` is the whole match.
    *
    * The automaton checker checks the patterns with `params` (see [[ReDoS.computeAutomatonComplexities]]). Each round
    * takes the best edit by the complexity after it, preferring an equivalent one, or else the one that differs on the
    * longest input among the first [[MaxDifferent]] best ones. It stops when the pattern is safe, no edit makes it
    * better, or after [[MaxRounds]] rounds. An edit may remove a capture not kept, which renumbers the later ones.
    *
    * When the automaton checker gives up on the exact complexities (e.g. with a back-reference), the upper bound of the
    * fuzz checker scores the pattern. A result scored only by an upper bound that is not linear is not reported, since
    * the bound does not show it better, but a linear one shows it safe. It throws [[UnsupportedException]] when there
    * is neither for the original pattern.
    *
    * When the time runs out after a round, the result is the pattern of the rounds so far, which is equivalent if every
    * edit is, and else compared only if the time allows.
    */
  def search(original: Pattern, captures: Seq[Int], params: Parameters)(using ctx: Context): Result =
    def compare(a: Pattern, aIndices: Seq[Int], b: Pattern, bIndices: Seq[Int]): Comparison =
      try
        Equivalence
          .findCounterexample(a.ir, b.ir, aIndices.zip(bIndices), params.maxNFASize, params.maxDFASize)
          .fold(Comparison.Equivalent)(Comparison.Different(_))
      catch case _: UnsupportedException => Comparison.Unknown

    // Tests whether the patterns match the same strings, which is false if the comparison does not support them.
    def matchesSame(a: Pattern, b: Pattern): Boolean =
      try Equivalence.findCounterexample(a.ir, b.ir, Seq.empty, params.maxNFASize, params.maxDFASize).isEmpty
      catch case _: UnsupportedException => false

    def parseEdited(source: String): Option[Pattern] =
      try Some(Pattern.parse(source, original.flags, original.dialect))
      catch
        case _: UnsupportedException => None
        // An edited tree is printed as a valid pattern, though the parser may not support it.
        // $COVERAGE-OFF$
        case ex: InvalidRegExpException =>
          throw new UnexpectedException(s"an edit makes an invalid pattern /$source/: ${ex.getMessage}")
        // $COVERAGE-ON$

    // Returns the best edit of the round, with its comparison.
    def step(current: Pattern, indices: Seq[Int], score: Score): Option[(Pattern, Seq[Int], Score, Comparison)] =
      var scoring = 0L
      var scored = 0
      var comparing = 0L
      val comparisons = mutable.Map.empty[String, Comparison]

      def growth(next: Pattern): Int = next.source.length - current.source.length

      def compareOnce(next: Pattern, nextIndices: Seq[Int]): Comparison =
        comparisons.getOrElseUpdate(
          next.source, {
            val started = System.nanoTime()
            try compare(current, indices, next, nextIndices)
            finally comparing += System.nanoTime() - started
          }
        )

      // The edits in the order that breaks the ties of the scores, so the first safe and equivalent one is the best.
      val edits = current
        .candidates(score.spots, indices.toSet)
        .flatMap: candidate =>
          parseEdited(current.print(candidate.node)).map: next =>
            (next, indices.map(i => i - candidate.removedCaptures.count(_ < i)), candidate)
        .sortBy((next, _, candidate) => (candidate.isFallback, growth(next)))
      val improved = LazyList
        .from(edits)
        .flatMap: (next, nextIndices, candidate) =>
          val started = System.nanoTime()
          val nextScore =
            try Score.of(next.ir, params)
            finally
              scoring += System.nanoTime() - started
              scored += 1
          nextScore.filter(_ > score).map((next, nextIndices, _, candidate))
      val chosen = improved
        .find((next, nextIndices, nextScore, _) =>
          nextScore.isSafe && !nextScore.isUpperBound && compareOnce(next, nextIndices) == Comparison.Equivalent
        )
        .map((next, nextIndices, nextScore, _) => (next, nextIndices, nextScore, Comparison.Equivalent))
        .orElse:
          val compared = LazyList
            .from(
              // The rank is negated as a `Long`, since `-Int.MinValue` (the rank of an exponential one) overflows.
              improved.sortBy: (next, _, nextScore, candidate) =>
                (-nextScore.rank.toLong, nextScore.count, nextScore.isUpperBound, candidate.isFallback, growth(next))
            )
            .map((next, nextIndices, nextScore, _) => (next, nextIndices, nextScore, compareOnce(next, nextIndices)))
            // A different edit is taken only if it matches the same strings, since it differs only in what it extracts,
            // and else it may match much fewer (e.g. `.*` to `.`, or `.*AspiegelBot` to `.*(?!.)AspiegelBot`, which
            // matches nothing), which no one would take as a repair. Without the comparison, an edit that bounds a
            // repetition is not taken for the same reason.
            .filter: (next, _, _, comparison) =>
              comparison match
                case Comparison.Equivalent   => true
                case Comparison.Different(_) => matchesSame(current, next)
                case Comparison.Unknown      => unboundedRepeats(next.node) >= unboundedRepeats(current.node)
          // Without an equivalent edit, the best ones differ on the longest input.
          compared
            .find(_._4 == Comparison.Equivalent)
            .orElse:
              compared.headOption.flatMap: (_, _, best, _) =>
                compared
                  .takeWhile((_, _, score, _) => score.rank == best.rank && score.count == best.count)
                  .take(MaxDifferent)
                  .maxByOption:
                    case (_, _, _, Comparison.Different(input)) => input.length
                    case _                                      => -1
      ctx.log:
        s"""|repair: ${edits.size} candidates, $scored scored in ${scoring / 1000000} ms,
            |  ${comparisons.size} compared in ${comparing / 1000000} ms""".stripMargin
      chosen

    // `indices` are the current indices of `captures`, and `isEquivalent` tells whether every edit so far is.
    def loop(
        current: Pattern,
        indices: Seq[Int],
        score: Score,
        round: Int,
        isEquivalent: Boolean
    ): (Pattern, Seq[Int], Score, Boolean) =
      if score.isSafe || round >= MaxRounds then (current, indices, score, isEquivalent)
      else
        ctx.log(s"repair: round $round from /${current.source}/")
        // A round without the time keeps the pattern of the rounds so far, if any.
        val chosen =
          try step(current, indices, score)
          catch case _: TimeoutException if round > 0 => None
        chosen match
          case Some((next, nextIndices, nextScore, comparison)) =>
            loop(next, nextIndices, nextScore, round + 1, isEquivalent && comparison == Comparison.Equivalent)
          case None => (current, indices, score, isEquivalent)

    Score.of(original.ir, params) match
      case None => throw new UnsupportedException("the automaton checker cannot check the pattern")
      case Some(score) if score.isSafe => Result.Safe
      case Some(score)                 =>
        val (fixed, indices, fixedScore, isEquivalent) = loop(original, captures, score, 0, true)
        // An upper bound that is not linear does not show that the result is better.
        if (fixed eq original) || (fixedScore.isUpperBound && !fixedScore.isSafe) then Result.NotFound
        else
          val comparison =
            if isEquivalent then Comparison.Equivalent
            else
              ctx.log("repair: comparison with the original")
              try compare(original, captures, fixed, indices)
              catch case _: TimeoutException => Comparison.Unknown
          Result.Repaired(fixed, indices, fixedScore.complexity, comparison)

  /** Counts the repetitions without a maximum in the syntax tree. */
  private def unboundedRepeats(node: Node): Int =
    val own = node.data match
      case NodeData.Repeat(_, Quantifier.Star(_) | Quantifier.Plus(_) | Quantifier.Unbounded(_, _), _) => 1
      case _                                                                                           => 0
    val children = node.data match
      case NodeData.Disjunction(ns)      => ns
      case NodeData.Sequence(ns)         => ns
      case NodeData.Repeat(n, _, _)      => Seq(n)
      case NodeData.Group(_, n)          => Seq(n)
      case NodeData.UnicodeList(ns, _)   => ns
      case NodeData.CaseRegion(_, ps, _) => ps.map(_.value)
      case _                             => Seq.empty
    own + children.map(unboundedRepeats).sum

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
      new CandidateFinder(node, ir, features, kept).find(spots)

    /** Prints an edited tree of this pattern as a source with the same flags. */
    def print(edited: Node): String = Printer.print(edited, flagSet, dialect, ir.mode)

  object Pattern:

    /** Parses the source.
      *
      * It throws [[InvalidRegExpException]] or [[UnsupportedException]] as [[ReDoS.parse]] does.
      */
    def parse(source: String, flags: String, dialect: Dialect)(using ctx: Context): Pattern =
      val (flagSet, node, ir) = ReDoS.parseTree(source, flags, dialect)
      Pattern(source, flags, dialect, flagSet, node, ir)

  /** Score is the vulnerabilities of a pattern, with the rank of the worst one (higher is better), their count, the
    * spots of the worst one, the complexity, and whether they are of the upper bound instead of the exact ones.
    */
  private final case class Score(
      rank: Int,
      count: Int,
      spots: Set[SourceLocation],
      complexity: AttackComplexity,
      isUpperBound: Boolean
  ):

    def isSafe: Boolean = count == 0

    def >(that: Score): Boolean = rank > that.rank || (rank == that.rank && count < that.count)

  private object Score:

    /** The vulnerabilities counted at most. */
    val maxCount = 16

    /** Checks the pattern by the automaton checker, by the upper bound (see [[FuzzChecker.upperBound]]) if it gives up
      * on the exact complexities, or returns `None` if there is no bound either.
      */
    def of(ir: IRPattern, params: Parameters)(using ctx: Context): Option[Score] =
      try
        val (complexities, rest) =
          ReDoS.computeAutomatonComplexities(ir, params).span(_.isInstanceOf[Complexity.Vulnerable[?]])
        val vulnerable = complexities.collect { case v: Complexity.Vulnerable[UChar] => v }.take(maxCount).toSeq
        if vulnerable.isEmpty then Some(Score(Int.MaxValue, 0, Set.empty, ReDoS.toSafeComplexity(rest), false))
        else Some(of(vulnerable, false))
      catch
        case _: UnsupportedException =>
          FuzzChecker
            .upperBound(ir, params.maxRepeatCount, params.maxNFASize, params.maxInitialGenerationSize)
            .map: vulnerable =>
              // A cost bounded from above by a linear one is safe, even when the exact one is unknown.
              if vulnerable.isEmpty then Score(Int.MaxValue, 0, Set.empty, AttackComplexity.Safe(false), false)
              else of(vulnerable.take(maxCount), true)

    /** Returns the score of the vulnerable complexities from the worst. */
    private def of(vulnerable: Seq[Complexity.Vulnerable[UChar]], isUpperBound: Boolean): Score =
      val worst = vulnerable.head
      val rank = worst match
        case _: Complexity.Exponential[?] => Int.MinValue
        case p: Complexity.Polynomial[?]  => -p.degree
      val spots = worst.hotspot.spots.collect { case Hotspot.Spot(s, e, Hotspot.Hot) => SourceLocation(s, e) }
      Score(rank, vulnerable.size, spots.toSet, worst.toAttackComplexity, isUpperBound)
