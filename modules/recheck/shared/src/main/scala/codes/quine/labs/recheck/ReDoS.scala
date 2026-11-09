package codes.quine.labs.recheck

import scala.collection.mutable
import scala.util.Random

import codes.quine.labs.recheck.automaton.AutomatonChecker
import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.context.CancellationToken
import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.DiagnosticsState
import codes.quine.labs.recheck.exception.InvalidRegExpException
import codes.quine.labs.recheck.exception.ReDoSException
import codes.quine.labs.recheck.exception.TimeoutException
import codes.quine.labs.recheck.exception.UnsupportedException
import codes.quine.labs.recheck.fuzz.FuzzChecker
import codes.quine.labs.recheck.fuzz.FuzzProgram
import codes.quine.labs.recheck.parameters.Checker
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.recheck.repair.Keep
import codes.quine.labs.recheck.repair.Repair
import codes.quine.labs.recheck.repair.Rx
import codes.quine.labs.recheck.verification.RuntimeAttackExecutor
import codes.quine.labs.recheck.verification.RuntimeAttacker
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
    * An attack with the same complexity, spots of its hotspot, and characteristics of its string as a refuted one is
    * skipped (see [[RuntimeAttacker]]). When runtime-attack verification refutes `maxRefutedAttacks` attacks, the
    * result is unknown with [[Diagnostics.ErrorKind.RuntimeAttacksRefuted]].
    */
  def check(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None
  ): Diagnostics =
    given ctx: Context = Context(params.timeout, token, params.logger)
    val state = new DiagnosticsState(source, flags, params.dialect)

    try
      val ir = parse(source, flags, params.dialect)
      state.unitMode = Some(ir.mode)
      val runtimeAttacker = runtimeAttackerOf(source, flags, ir, params, state)
      params.checker match
        case Checker.Automaton => checkAutomaton(ir, params, state, runtimeAttacker, mutable.ArrayBuffer.empty)
        case Checker.Fuzz      => checkFuzz(ir, params, state, runtimeAttacker, Seq.empty)
        case Checker.Auto      => checkAuto(ir, params, state, runtimeAttacker)
    catch case ex: ReDoSException => state.unknown(ex)

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
      val (kept, found) = fewer.distinct.foldLeft((requested, Repair.search(original, requested, params))):
        case (best @ (_, found), _) if isSafe(found) => best
        case (best, indices)                         =>
          ctx.log(s"repair: keep ${indices.mkString("[", ", ", "]")}")
          try
            val found = Repair.search(original, indices, params)
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

  /** Returns the runtime attacker of the checking of the pattern, which runs the attack strings on the runtime of the
    * dialect.
    */
  private def runtimeAttackerOf(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters,
      state: DiagnosticsState
  )(using ctx: Context): RuntimeAttacker =
    val execute = (attack: AttackPattern) =>
      RuntimeAttackExecutor.execute(
        source,
        flags,
        params.dialect,
        ir.mode,
        attack,
        runtimeAttackCommand = params.runtimeAttackCommand,
        runtimeAttackCompileTimeout = params.runtimeAttackCompileTimeout,
        runtimeAttackTimeout = params.runtimeAttackTimeout
      )
    new RuntimeAttacker(
      state,
      execute,
      ir.readStartLocations(params.maxRefutedAttackPartSize),
      RuntimeAttacker.characteristicSets(ir, params.attackStringCharacteristics),
      ir.mode,
      limit = params.runtimeAttackLimit,
      maxSize = params.maxRuntimeAttackStringSize,
      maxRefutedAttacks = params.maxRefutedAttacks
    )

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

  /** Runs the automaton checker, and returns the diagnostics of its first attack that the runtime does not refute, or
    * the safe ones.
    *
    * `attacks` collects the vulnerable complexities that it finds.
    */
  private def checkAutomaton(
      ir: IRPattern,
      params: Parameters,
      state: DiagnosticsState,
      runtimeAttacker: RuntimeAttacker,
      attacks: mutable.Buffer[Complexity.Vulnerable[UChar]]
  )(using ctx: Context): Diagnostics =
    import params.*
    state.checker = Some(Diagnostics.Checker.Automaton)

    // The safe complexities follow the vulnerable ones.
    val (vulnerable, rest) = computeAutomatonComplexities(ir, params).span(_.isInstanceOf[Complexity.Vulnerable[?]])
    val diagnostics = vulnerable
      .collect:
        case vul: Complexity.Vulnerable[UChar] => vul
      .flatMap: vul =>
        attacks += vul
        val attack =
          vul.buildAttackPattern(ir.mode, limit = runtimeAttackLimit, maxSize = maxRuntimeAttackStringSize)
        runtimeAttacker.attack(vul.toAttackComplexity, attack, vul.hotspot)

    diagnostics
      .nextOption()
      .getOrElse(state.safe(toSafeComplexity(rest)))

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
      val epsNFA =
        EpsNFABuilder.build(ir, Set.empty, maxBackReferenceStrings = maxBackReferenceStrings, maxNFASize = maxNFASize)
      val orderedNFA = epsNFA.toOrderedNFA(false, maxDFASize = maxDFASize, maxNFASize = maxNFASize).rename
      AutomatonChecker.check(
        epsNFA.restrictInputs(orderedNFA).mapAlphabet(alphabetRepresentative.of),
        false,
        maxDFASize = maxDFASize,
        maxNFASize = maxNFASize
      )

  /** Runs the fuzz checker, and returns the diagnostics of its first attack that the runtime does not refute, or the
    * safe ones.
    *
    * `automatonAttacks` are the attacks that the automaton checker found, which are the upper bound if there is any
    * (see [[FuzzChecker.check]]).
    */
  private def checkFuzz(
      ir: IRPattern,
      params: Parameters,
      state: DiagnosticsState,
      runtimeAttacker: RuntimeAttacker,
      automatonAttacks: Seq[Complexity.Vulnerable[UChar]]
  )(using ctx: Context): Diagnostics =
    import params.*
    state.checker = Some(Diagnostics.Checker.Fuzz)
    val (results, safeComplexity, upperBound) = FuzzChecker.check(
      ir,
      FuzzProgram.from(ir),
      new Random(randomSeed),
      runtimeAttacker,
      automatonAttacks,
      accelerationHitRatio = accelerationHitRatio,
      accelerationMode = accelerationMode,
      alphabetRepresentative = alphabetRepresentative,
      attackLimit = attackLimit,
      attackTimeout = attackTimeout,
      boundedWidthRatio = boundedWidthRatio,
      crossoverSize = crossoverSize,
      incubationLimit = incubationLimit,
      incubationTimeout = incubationTimeout,
      maxAttackCandidates = maxAttackCandidates,
      maxAttackStringSize = maxAttackStringSize,
      maxBoostIteration = maxBoostIteration,
      maxDegree = maxDegree,
      maxExtraDegree = maxExtraDegree,
      maxGeneStringSize = maxGeneStringSize,
      maxGenerationSize = maxGenerationSize,
      maxIncubationBoundedWidth = maxIncubationBoundedWidth,
      maxInitialGenerationSize = maxInitialGenerationSize,
      maxIteration = maxIteration,
      maxNFASize = maxNFASize,
      maxRepeatCount = maxRepeatCount,
      maxSimpleRepeatCount = maxSimpleRepeatCount,
      mutationSize = mutationSize,
      seeder = seeder,
      seedingLimit = seedingLimit,
      seedingTimeout = seedingTimeout
    )
    // The fuzzing starts on the first iteration, so the diagnostics of an error in it have the upper bound.
    state.upperBound = upperBound
    results.nextOption().getOrElse(state.safe(safeComplexity))

  /** Runs the automaton checker, or the fuzz checker if the automaton checker does not support the pattern, and returns
    * its diagnostics.
    *
    * When every attack of the automaton checker is refuted, the fuzz checker looks for another attack. Those attacks
    * are its upper bound and its seeds (see [[FuzzChecker.check]]). If it does not finish, for the time or for the
    * limit of the refuted attacks, the result is the safe diagnostics of the automaton checker, with the attacks
    * refuted by then.
    */
  private def checkAuto(
      ir: IRPattern,
      params: Parameters,
      state: DiagnosticsState,
      runtimeAttacker: RuntimeAttacker
  )(using ctx: Context): Diagnostics =
    def isUnfinished(d: Diagnostics): Boolean = d match
      case d: Diagnostics.Unknown =>
        d.error == Diagnostics.ErrorKind.Timeout || d.error == Diagnostics.ErrorKind.RuntimeAttacksRefuted
      case _ => false

    val attacks = mutable.ArrayBuffer.empty[Complexity.Vulnerable[UChar]]
    val diagnostics =
      try checkAutomaton(ir, params, state, runtimeAttacker, attacks)
      catch case _: UnsupportedException => checkFuzz(ir, params, state, runtimeAttacker, Seq.empty)
    diagnostics match
      case safe: Diagnostics.Safe if safe.checker == Diagnostics.Checker.Automaton && safe.refutedAttacks.nonEmpty =>
        ctx.log("auto: retry with the fuzz checker")
        val retried =
          try checkFuzz(ir, params, state, runtimeAttacker, attacks.toSeq)
          catch case ex: ReDoSException => state.unknown(ex)
        if isUnfinished(retried) then
          ctx.log("auto: the fuzz checker did not finish")
          safe.copy(refutedAttacks = state.refutedAttacks.toSeq)
        else retried
      case _ => diagnostics
