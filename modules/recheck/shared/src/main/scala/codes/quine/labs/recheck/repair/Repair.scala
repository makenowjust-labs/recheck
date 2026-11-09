package codes.quine.labs.recheck
package repair

import scala.collection.mutable

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.SourceLocation
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

  /** The equivalent edits keeping the score that a round tries another edit after at most, without a better edit. */
  val MaxNeutral = 4

  /** The equivalent edits keeping the score that a round takes in turn at most before a better edit. */
  val MaxNeutralDepth = 2

  /** How many times smaller the generations of the fuzz checker are while the repair scores the edits. */
  val FuzzScale = 4

  /** Searches for a safe pattern by edits around the hotspots, preferring the ones that keep the extraction, where
    * `captures` are the indices of the captures to keep and `0` is the whole match.
    *
    * The checker of `params` checks the original and the result as [[ReDoS.check]] does, so a safe result is what it
    * finds safe, and it scores the edits with the generations of the fuzz checker [[FuzzScale]] times smaller. Each
    * round takes the best edit by the complexity after it, preferring an equivalent one, or else the one that differs
    * on the longest input among the first [[MaxDifferent]] best ones. Without a better edit, it tries the next edits
    * after each of the first [[MaxNeutral]] equivalent edits keeping the score, up to [[MaxNeutralDepth]] of them in
    * turn, and takes them if they end better and equivalent. It stops when the pattern is safe, no edit makes it
    * better, or after [[MaxRounds]] rounds. An edit may remove a capture not kept, which renumbers the later ones. It
    * throws [[UnsupportedException]] when the checker does not support the original pattern.
    *
    * When the time runs out after a round, the result is the pattern of the rounds so far, which is equivalent if every
    * edit is, and else compared only if the time allows, and its complexity is the score of the edit if the time does
    * not allow checking it.
    */
  def search(original: Pattern, captures: Seq[Int], params: Parameters)(using ctx: Context): Result =
    val smallerFuzz = params.copy(
      maxIteration = (params.maxIteration / FuzzScale).max(1),
      maxInitialGenerationSize = (params.maxInitialGenerationSize / FuzzScale).max(1),
      maxGenerationSize = (params.maxGenerationSize / FuzzScale).max(1)
    )

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

    // Returns the best edit of the round, with its comparison, which may follow up to `neutral` equivalent edits.
    def step(
        current: Pattern,
        indices: Seq[Int],
        score: Score,
        neutral: Int = MaxNeutralDepth
    ): Option[(Pattern, Seq[Int], Score, Comparison)] =
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
      val scoredEdits = LazyList
        .from(edits)
        .flatMap: (next, nextIndices, candidate) =>
          val started = System.nanoTime()
          val nextScore =
            try Score.of(next.ir, smallerFuzz)
            finally
              scoring += System.nanoTime() - started
              scored += 1
          nextScore.map((next, nextIndices, _, candidate))
      val improved = scoredEdits.filter(_._3 > score)
      val chosen = improved
        .find((next, nextIndices, nextScore, _) =>
          nextScore.isSafe && compareOnce(next, nextIndices) == Comparison.Equivalent
        )
        .map((next, nextIndices, nextScore, _) => (next, nextIndices, nextScore, Comparison.Equivalent))
        .orElse:
          val compared = LazyList
            .from(
              // The rank is negated as a `Long`, since `-Int.MinValue` (the rank of an exponential one) overflows.
              improved.sortBy: (next, _, nextScore, candidate) =>
                (
                  -nextScore.rank.toLong,
                  nextScore.count,
                  nextScore.complexity.isFuzz,
                  candidate.isFallback,
                  growth(next)
                )
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

      // An equivalent edit keeping the score may make way for another, as the two together repair
      // `; (SAMSUNG-)([A-Za-z0-9\-]+).* Build/` though neither does alone. The time running out there ends the search.
      def afterNeutral: Option[(Pattern, Seq[Int], Score, Comparison)] =
        try
          scoredEdits
            .filter: (next, nextIndices, nextScore, _) =>
              !(nextScore > score) && !(score > nextScore) && compareOnce(next, nextIndices) == Comparison.Equivalent
            .take(MaxNeutral)
            .flatMap: (next, nextIndices, nextScore, _) =>
              ctx.log(s"repair: edits after /${next.source}/")
              step(next, nextIndices, nextScore, neutral - 1).filter: (_, _, laterScore, comparison) =>
                laterScore > score && comparison == Comparison.Equivalent
            .headOption
        catch case _: TimeoutException => None

      chosen.orElse(if neutral == 0 then None else afterNeutral)

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
      case None                        => throw new UnsupportedException("the checker cannot check the pattern")
      case Some(score) if score.isSafe => Result.Safe
      case Some(score)                 =>
        val (fixed, indices, fixedScore, isEquivalent) = loop(original, captures, score, 0, true)
        if fixed eq original then Result.NotFound
        else
          val comparison =
            if isEquivalent then Comparison.Equivalent
            else
              ctx.log("repair: comparison with the original")
              try compare(original, captures, fixed, indices)
              catch
                case _: TimeoutException => Comparison.Unknown
          // The fuzz checker of the scoring runs smaller generations, so the result is checked as `ReDoS.check` does.
          val checked =
            if !fixedScore.complexity.isFuzz then fixedScore
            else
              ctx.log("repair: check of the result")
              try Score.of(fixed.ir, params).getOrElse(fixedScore)
              catch case _: TimeoutException => fixedScore
          Result.Repaired(fixed, indices, checked.complexity, comparison)

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

  /** Score is the vulnerabilities of a pattern that the checker finds, with the rank of the worst one (higher is
    * better), their count, the spots of the worst one, and the complexity.
    */
  private final case class Score(rank: Int, count: Int, spots: Set[SourceLocation], complexity: AttackComplexity):

    def isSafe: Boolean = count == 0

    /** Tests whether this is better, where the count of the fuzz checker, which finds one in a run, compares with none.
      */
    def >(that: Score): Boolean =
      rank > that.rank || (rank == that.rank && !complexity.isFuzz && !that.complexity.isFuzz && count < that.count)

  private object Score:

    /** The vulnerabilities counted at most. */
    val maxCount = 16

    /** Checks the pattern by the checker of the parameters as [[ReDoS.check]] does, without runtime attacks, or returns
      * `None` if the checker does not support it.
      */
    def of(ir: IRPattern, params: Parameters)(using ctx: Context): Option[Score] =
      val avoidsAttack = (_: AttackComplexity.Vulnerable, _: Hotspot) => false
      val (diagnostics, safe, _) = params.checker match
        case Checker.Automaton => ReDoS.checkAutomaton("", "", ir, params)
        case Checker.Fuzz      => ReDoS.checkFuzz("", "", ir, params, avoidsAttack)
        case Checker.Auto      => ReDoS.checkAuto("", "", ir, params, avoidsAttack)
      try
        diagnostics.nextOption() match
          case None        => Some(Score(Int.MaxValue, 0, Set.empty, safe().complexity))
          case Some(worst) =>
            // The fuzz checker would run again for another one.
            val count = if worst.checker == Checker.Fuzz then 1 else 1 + diagnostics.take(maxCount - 1).size
            val rank = worst.complexity match
              case _: AttackComplexity.Exponential => Int.MinValue
              case p: AttackComplexity.Polynomial  => -p.degree
            val spots = worst.hotspot.spots.collect { case Hotspot.Spot(s, e, Hotspot.Hot) => SourceLocation(s, e) }
            Some(Score(rank, count, spots.toSet, worst.complexity))
      catch case _: UnsupportedException => None
