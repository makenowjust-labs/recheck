package codes.quine.labs.recheck

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.Random

import codes.quine.labs.recheck.automaton.AutomatonChecker
import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.common.CancellationToken
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.ReDoSException
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.DiagnosticsIteratorUtil.*
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.RefutedAttack
import codes.quine.labs.recheck.diagnostics.UpperBound
import codes.quine.labs.recheck.fuzz.FuzzChecker
import codes.quine.labs.recheck.fuzz.FuzzProgram
import codes.quine.labs.recheck.repair.Keep
import codes.quine.labs.recheck.repair.Repair
import codes.quine.labs.recheck.repair.Rx
import codes.quine.labs.recheck.verification.RuntimeAttackVerifier
import codes.quine.labs.recheck.vm.Interpreter
import codes.quine.labs.recheck.vm.ProgramBuilder
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetException
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.build.IRBuilder
import codes.quine.labs.resyntax.ir.build.IRBuildingException
import codes.quine.labs.resyntax.ir.build.MultiFoldingLimitException
import codes.quine.labs.resyntax.parser.NestingLimitException
import codes.quine.labs.resyntax.parser.Parser
import codes.quine.labs.resyntax.parser.ParsingException

/** ReDoS is a ReDoS checker frontend. */
object ReDoS:

  /** Returns the diagnostics of the given RegExp pattern.
    *
    * The pattern and the flags are of `params.dialect`. The result is the first vulnerable diagnostics whose runtime
    * attack is not run or confirms the attack, or else the safe or unknown diagnostics, with the attacks refuted before
    * them.
    *
    * An attack with the same complexity and hotspot as a refuted one is skipped. When runtime-attack verification
    * refutes `maxRefutedAttacks` attacks, the result is unknown with [[Diagnostics.ErrorKind.RuntimeAttacksRefuted]].
    */
  def check(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None
  ): Diagnostics =
    given ctx: Context = Context(params.timeout, token, params.logger)

    def diagnose(ir: IRPattern): Diagnostics =
      val refutedAttacks = mutable.ArrayBuffer.empty[RefutedAttack]
      val avoidsAttack = (c: AttackComplexity.Vulnerable, h: Hotspot) => isRefuted(refutedAttacks, c, h)
      // The checkers run lazily on iterating the candidates, so that the errors of the checking are caught below.
      val (candidates, safe, upperBound) = params.checker match
        case Checker.Automaton => checkAutomaton(source, flags, ir, params)
        case Checker.Fuzz      => checkFuzz(source, flags, ir, params, avoidsAttack)
        case Checker.Auto      => checkAuto(source, flags, ir, params, avoidsAttack)
      val diagnostics =
        try
          val verify = runRuntimeAttack(source, flags, params.dialect, ir.mode, params.runtimeAttackTimeout)
          verifyCandidates(candidates, safe, verify, refutedAttacks, params.maxRefutedAttacks)
        catch
          case ex: ReDoSException =>
            Diagnostics.Unknown.from(source, flags, params.dialect, Some(ir.mode), refutedAttacks.toSeq, ex)
      diagnostics.withUpperBound(upperBound())

    try diagnose(parse(source, flags, params.dialect))
    catch case ex: ReDoSException => Diagnostics.Unknown.from(source, flags, params.dialect, None, Seq.empty, ex)

  /** Returns a better RegExp for the given one, keeping what `keep` tells of its matches.
    *
    * The pattern and the flags are of `params.dialect`. The repair edits the pattern around the hotspots that the
    * checker of `params.checker` finds as [[check]] does, preferring the edits that keep the matches, until it is safe
    * or no edit makes it better. The result has the complexity of the repaired RegExp, and the shortest input on which
    * the matches differ if they do. Without a safe repair keeping what `keep` tells, a safe repair keeping less (the
    * range of the match, and then only whether it matches) is the result, whose `kept` tells what it keeps. The time is
    * limited by `params.repairTimeout`.
    */
  def repair(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      keep: Keep = Keep.All,
      token: Option[CancellationToken] = None
  ): Rx =
    given ctx: Context = Context(params.repairTimeout, token, params.logger)
    val dialect = params.dialect
    try
      val original = Repair.Pattern.parse(source, flags, dialect)
      def isSafe(result: Repair.Result): Boolean = result match
        case Repair.Result.Safe                                        => true
        case Repair.Result.Repaired(_, _, _: AttackComplexity.Safe, _) => true
        case _                                                         => false

      val requested = keep.captureIndices(original.ir)
      val fewer = Seq(Keep.MatchRange, Keep.Match).map(_.captureIndices(original.ir)).filter(_.size < requested.size)
      // A search keeping fewer captures is the result only if it is safe, and the time it runs out keeps the earlier.
      // The searches share the scores of the patterns.
      val scores = Repair.Scores()
      val (kept, found) = fewer.distinct.foldLeft((requested, Repair.search(original, requested, params, scores))):
        case (best @ (_, found), _) if isSafe(found) => best
        case (best, indices)                         =>
          ctx.log(s"repair: keep ${indices.mkString("[", ", ", "]")}")
          try
            val found = Repair.search(original, indices, params, scores)
            if isSafe(found) then (indices, found) else best
          catch case _: TimeoutException => best
      found match
        case Repair.Result.Safe     => Rx.Safe(source, flags, dialect)
        case Repair.Result.NotFound => Rx.Unknown(source, flags, dialect, None)
        case Repair.Result.Repaired(repaired, captures, complexity, comparison) =>
          val result = comparison match
            case Repair.Comparison.Equivalent       => Rx.Comparison.Equivalent
            case Repair.Comparison.Unknown          => Rx.Comparison.Unknown
            case Repair.Comparison.Different(input) =>
              // The captures are of the indices in each pattern, in the order of `kept`.
              def run(pattern: IRPattern, indices: Seq[Int]): Option[Seq[Option[(Int, Int)]]] =
                val result = Interpreter.run(ProgramBuilder.build(pattern, false), input, Interpreter.Options())
                result.captures.map(cs => indices.map(i => Option.when(cs(2 * i) >= 0)((cs(2 * i), cs(2 * i + 1)))))

              val counterexample = Rx.Counterexample(input, run(original.ir, kept), run(repaired.ir, captures))
              Rx.Comparison.Different(counterexample)
          Rx.Repaired(source, flags, dialect, original.ir.mode, repaired.source, complexity, kept, result)
    catch case ex: ReDoSException => Rx.Unknown(source, flags, dialect, Some(Diagnostics.ErrorKind.from(ex)))

  /** Runs `verify` on the candidates in order, and returns the final diagnostics of [[check]].
    *
    * `safe` gives the final diagnostics after the candidates run out. `refutedAttacks` collects the attacks that
    * runtime attacks refute.
    */
  private[recheck] def verifyCandidates(
      candidates: Iterator[Diagnostics.Vulnerable],
      safe: () => Diagnostics.Safe,
      verify: Diagnostics.Vulnerable => Diagnostics.Vulnerable,
      refutedAttacks: mutable.Buffer[RefutedAttack],
      maxRefutedAttacks: Int
  )(using ctx: Context): Diagnostics =
    val untried = candidates.filter: d =>
      val skipped = isRefuted(refutedAttacks, d.complexity, d.hotspot)
      if skipped then ctx.log("runtime attack: skip the same attack as a refuted one")
      !skipped

    @tailrec
    def verifyNext(): Diagnostics =
      if !untried.hasNext then safe().copy(refutedAttacks = refutedAttacks.toSeq)
      else
        val d = verify(untried.next())
        d.runtimeAttack match
          case Some(r) if !r.isConfirmed =>
            refutedAttacks += RefutedAttack(d.complexity, d.attack, d.hotspot, r)
            if refutedAttacks.size < maxRefutedAttacks then verifyNext()
            else
              Diagnostics.Unknown(
                d.source,
                d.flags,
                d.dialect,
                Some(d.unitMode),
                Diagnostics.ErrorKind.RuntimeAttacksRefuted,
                refutedAttacks.toSeq,
                Some(d.checker)
              )
          case _ => d.copy(refutedAttacks = refutedAttacks.toSeq)

    verifyNext()

  /** Whether an attack of the complexity and the hotspot is the same as a refuted one. */
  private[recheck] def isRefuted(
      refutedAttacks: collection.Seq[RefutedAttack],
      complexity: AttackComplexity.Vulnerable,
      hotspot: Hotspot
  ): Boolean =
    val key = AttackKey(complexity, hotspot)
    refutedAttacks.exists(r => AttackKey(r.complexity, r.hotspot) == key)

  /** Parses the pattern and the flags in the dialect, and builds its IR.
    *
    * It throws [[InvalidRegExpException]] on an invalid pattern, and [[UnsupportedException]] on a pattern nested
    * deeper than [[Parser.MaxNestingDepth]] or with too many multi-character case foldings in a literal.
    */
  private[recheck] def parse(source: String, flags: String, dialect: Dialect)(using ctx: Context): IRPattern =
    parseTree(source, flags, dialect)._3

  /** Parses the source into its flags, its syntax tree and its IR. */
  private[recheck] def parseTree(source: String, flags: String, dialect: Dialect)(using
      ctx: Context
  ): (FlagSet, Node, IRPattern) =
    ctx.log("parse: start")
    val flagSet = ctx.interrupt(parseFlags(flags, dialect))
    val mode = ctx.interrupt(Parser.determineUnitMode(source, flagSet, dialect))
    val node = ctx.interrupt(parseSource(source, flagSet, dialect, mode))
    val ir = ctx.interrupt(buildIR(node, flagSet, dialect, mode))
    ctx.log(s"parse: finish\n  pattern: /$source/$flags")
    (flagSet, node, ir)

  private def parseFlags(flags: String, dialect: Dialect): FlagSet =
    try FlagSet.parse(flags, dialect)
    catch case ex: FlagSetException => throw new InvalidRegExpException(ex.getMessage)

  private def parseSource(source: String, flagSet: FlagSet, dialect: Dialect, mode: UnitMode): Node =
    try Parser.parse(source, flagSet, dialect, mode)
    catch
      case ex: ParsingException      => throw new InvalidRegExpException(ex.getMessage)
      case ex: NestingLimitException => throw new UnsupportedException(ex.getMessage)

  private def buildIR(node: Node, flagSet: FlagSet, dialect: Dialect, mode: UnitMode): IRPattern =
    try IRBuilder.build(node, flagSet, dialect, mode)
    catch
      case ex: IRBuildingException        => throw new InvalidRegExpException(ex.getMessage)
      case ex: MultiFoldingLimitException => throw new UnsupportedException(ex.getMessage)

  /** AttackKey identifies an attack by the degree of the polynomial complexity (or `None` for the exponential one) and
    * the hotspot.
    */
  private[recheck] final case class AttackKey(degree: Option[Int], spots: Seq[(Int, Int)])

  private[recheck] object AttackKey:

    def apply(complexity: AttackComplexity.Vulnerable, hotspot: Hotspot): AttackKey =
      val degree = complexity match
        case AttackComplexity.Polynomial(degree, _) => Some(degree)
        case AttackComplexity.Exponential(_)        => None
      AttackKey(degree, hotspot.spots.collect { case Hotspot.Spot(s, e, Hotspot.Hot) => (s, e) })

  /** Runs the automaton checker, and returns the vulnerable diagnostics it finds, a function giving the safe
    * diagnostics once they run out, and a function giving the upper bound, which is always `None`.
    */
  private[recheck] def checkAutomaton(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters
  )(using
      ctx: Context
  ): (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe, () => Option[UpperBound]) =
    import params.*

    // The safe complexities follow the vulnerable ones.
    lazy val (vulnerable, rest) =
      computeAutomatonComplexities(ir, params).span(_.isInstanceOf[Complexity.Vulnerable[?]])

    val diagnostics = vulnerable
      .collect:
        case vul: Complexity.Vulnerable[UChar] =>
          val attack = vul.buildAttackPattern(runtimeAttackLimit, maxRuntimeAttackStringSize, ir.mode)
          Diagnostics.Vulnerable(
            source,
            flags,
            params.dialect,
            ir.mode,
            vul.toAttackComplexity,
            attack,
            vul.hotspot,
            None,
            Seq.empty,
            Checker.Automaton
          )
      .withChecker(Checker.Automaton)

    def safe(): Diagnostics.Safe =
      Diagnostics.Safe(source, flags, params.dialect, ir.mode, toSafeComplexity(rest), Seq.empty, Checker.Automaton)

    (diagnostics, safe, () => None)

  /** Returns the complexity of a safe pattern from the complexities of [[computeAutomatonComplexities]] after the
    * vulnerable ones.
    */
  private[recheck] def toSafeComplexity(rest: Iterator[Complexity[UChar]]): AttackComplexity.Safe =
    rest.nextOption() match
      case Some(safe: Complexity.Safe) => safe.toAttackComplexity
      case _                           => AttackComplexity.Safe(false)

  /** Runs the automaton checker, and returns the complexities, the vulnerable ones from the worst followed by the safe
    * one if any.
    *
    * It throws [[UnsupportedException]] when it gives up.
    */
  private[recheck] def computeAutomatonComplexities(ir: IRPattern, params: Parameters)(using
      ctx: Context
  ): Iterator[Complexity[UChar]] =
    import params.*
    val maxNFASize = if checker == Checker.Auto then params.maxNFASize else Int.MaxValue
    val maxDFASize = if checker == Checker.Auto then params.maxDFASize else Int.MaxValue

    if checker == Checker.Auto && ir.repeatCount >= maxRepeatCount then
      ctx.log("auto: exceed maxRepeatCount")
      throw UnsupportedException("The pattern contains too many repeat")
    // When the pattern has no infinite repetition, then it is safe.
    else if ir.isBounded then
      ctx.log("automaton: constant pattern")
      Iterator.empty
    else if checker == Checker.Auto && ir.size >= maxPatternSize then
      ctx.log("auto: exceed maxPatternSize")
      throw UnsupportedException("The pattern is too large")
    else
      val epsNFA = EpsNFABuilder.build(ir, Set.empty, maxNFASize, maxBackReferenceStrings)
      val orderedNFA = epsNFA.toOrderedNFA(false, maxNFASize, maxDFASize).rename
      AutomatonChecker.check(
        epsNFA.restrictInputs(orderedNFA).mapAlphabet(_.representative),
        false,
        maxNFASize,
        maxDFASize
      )

  /** Runs the fuzz checker, and returns the vulnerable diagnostics it finds, a function giving the safe diagnostics
    * once they run out, and a function giving the upper bound found before fuzzing.
    *
    * `avoidsAttack` tells whether the fuzz checker avoids an attack of the complexity and the hotspot, which a runtime
    * attack refuted.
    */
  private[recheck] def checkFuzz(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters,
      avoidsAttack: (AttackComplexity.Vulnerable, Hotspot) => Boolean
  )(using
      ctx: Context
  ): (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe, () => Option[UpperBound]) =
    import params.*

    var upperBound = Option.empty[UpperBound]

    // It runs on the first iteration in `withChecker`, which marks its errors as of the fuzz checker.
    lazy val (results, safeComplexity) =
      val fuzz = FuzzProgram.from(ir)
      val (results, safeComplexity, checkedUpperBound) = FuzzChecker.check(
        ir,
        fuzz,
        new Random(randomSeed),
        seeder,
        maxSimpleRepeatCount,
        maxRepeatCount,
        maxNFASize,
        seedingLimit,
        seedingTimeout,
        incubationLimit,
        incubationTimeout,
        maxGeneStringSize,
        attackLimit,
        attackTimeout,
        crossoverSize,
        mutationSize,
        maxAttackStringSize,
        maxInitialGenerationSize,
        maxGenerationSize,
        maxAttackCandidates,
        maxIteration,
        maxDegree,
        heatRatio,
        accelerationMode,
        avoidsAttack
      )
      upperBound = checkedUpperBound
      (results, safeComplexity)

    def diagnose: Iterator[Diagnostics.Vulnerable] =
      results.map:
        case (complexity, attack, hotspot) =>
          val adjusted = attack.adjust(complexity, runtimeAttackLimit, maxRuntimeAttackStringSize, ir.mode)
          Diagnostics.Vulnerable(
            source,
            flags,
            params.dialect,
            ir.mode,
            complexity,
            adjusted,
            hotspot,
            None,
            Seq.empty,
            Checker.Fuzz
          )

    val diagnostics = diagnose.withChecker(Checker.Fuzz)
    val safe = () => Diagnostics.Safe(source, flags, params.dialect, ir.mode, safeComplexity, Seq.empty, Checker.Fuzz)
    (diagnostics, safe, () => upperBound)

  /** Runs the automaton checker, or the fuzz checker if the automaton checker gives up, and returns the vulnerable
    * diagnostics they find, and functions giving the safe diagnostics and the upper bound of the checker that ran last.
    */
  private[recheck] def checkAuto(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters,
      avoidsAttack: (AttackComplexity.Vulnerable, Hotspot) => Boolean
  )(using
      ctx: Context
  ): (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe, () => Option[UpperBound]) =
    val (automaton, automatonSafe, automatonUpperBound) = checkAutomaton(source, flags, ir, params)
    var safe = automatonSafe
    var upperBound = automatonUpperBound
    val diagnostics = automaton.recoverWith:
      case _: UnsupportedException =>
        val (fuzz, fuzzSafe, fuzzUpperBound) = checkFuzz(source, flags, ir, params, avoidsAttack)
        safe = fuzzSafe
        upperBound = fuzzUpperBound
        fuzz
    (diagnostics, () => safe(), () => upperBound())

  /** Runs the attack in the vulnerable diagnostics on the runtime of the dialect, and records the result in them. */
  private[recheck] def runRuntimeAttack(
      source: String,
      flags: String,
      dialect: Dialect,
      mode: UnitMode,
      timeout: Duration
  )(
      d: Diagnostics.Vulnerable
  )(using ctx: Context): Diagnostics.Vulnerable =
    try d.copy(runtimeAttack = RuntimeAttackVerifier.verify(source, flags, dialect, mode, d.attack, timeout, sys.env))
    catch
      case ex: ReDoSException =>
        ex.checker = Some(d.checker)
        throw ex
