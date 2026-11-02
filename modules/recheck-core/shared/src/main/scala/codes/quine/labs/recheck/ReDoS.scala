package codes.quine.labs.recheck

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
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.DiagnosticsIteratorUtil.*
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.fuzz.FuzzChecker
import codes.quine.labs.recheck.fuzz.FuzzProgram
import codes.quine.labs.recheck.recall.RecallValidator
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetException
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.build.IRBuilder
import codes.quine.labs.resyntax.ir.build.IRBuildingException
import codes.quine.labs.resyntax.parser.NestingLimitException
import codes.quine.labs.resyntax.parser.Parser
import codes.quine.labs.resyntax.parser.ParsingException

/** ReDoS is a ReDoS checker frontend. */
object ReDoS:

  /** Tests the given RegExp pattern causes ReDoS.
    *
    * The pattern and the flags are of `params.dialect`. It returns the final diagnostics of [[checkAll]], and computes
    * no more than it.
    */
  def check(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None
  ): Diagnostics =
    checkAll(source, flags, params, token).reduce((_, d) => d)

  /** Tests the given RegExp pattern causes ReDoS, and iterates the diagnostics until the final one.
    *
    * The diagnostics are the vulnerable ones whose recall validation failed, followed by the final one: the first
    * vulnerable diagnostics whose recall validation is not run or succeeded, or else the safe or unknown one. The
    * iterator computes no more than the next diagnostics.
    *
    * A vulnerable diagnostics with the same complexity and hotspot as a failed one is skipped. When the recall
    * validation fails `maxRecallFailures` times, the final one is unknown with
    * [[Diagnostics.ErrorKind.RecallFailureLimit]].
    */
  def checkAll(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None
  ): Iterator[Diagnostics] =
    given ctx: Context = Context(params.timeout, token, params.logger)

    def diagnose: Iterator[Diagnostics] =
      val ir = parse(source, flags, params.dialect)
      val failures = mutable.Set.empty[RecallKey]
      val avoidsAttack = (c: AttackComplexity.Vulnerable, h: Hotspot) => failures.contains(RecallKey(c, h))
      val (candidates, safe) = params.checker match
        case Checker.Automaton => checkAutomaton(source, flags, ir, params)
        case Checker.Fuzz      => checkFuzz(source, flags, ir, params, avoidsAttack)
        case Checker.Auto      => checkAuto(source, flags, ir, params, avoidsAttack)
      val validate = recallValidation(source, flags, params.dialect, ir.mode, params.recallTimeout)
      validateCandidates(candidates, safe, validate, failures, params.maxRecallFailures)

    diagnose.recoverWith:
      case ex: ReDoSException => Iterator(Diagnostics.Unknown.from(source, flags, params.dialect, ex))

  /** Runs `validate` on the candidates in order, and iterates the diagnostics of [[checkAll]] until the final one.
    *
    * `safe` gives the final diagnostics after the candidates run out. `failures` holds the attacks whose recall
    * validation failed.
    */
  private[recheck] def validateCandidates(
      candidates: Iterator[Diagnostics.Vulnerable],
      safe: () => Diagnostics.Safe,
      validate: Diagnostics.Vulnerable => Diagnostics.Vulnerable,
      failures: mutable.Set[RecallKey],
      maxRecallFailures: Int
  )(using ctx: Context): Iterator[Diagnostics] =
    val untried = candidates.filter: d =>
      val skipped = failures.contains(RecallKey(d.complexity, d.hotspot))
      if skipped then ctx.log("recall: skip the same attack as a failed one")
      !skipped

    def validateNext(): Iterator[Diagnostics] =
      if !untried.hasNext then Iterator(safe())
      else
        val d = validate(untried.next())
        if d.recallValidation.forall(_.isSucceeded) then Iterator(d)
        else
          failures.add(RecallKey(d.complexity, d.hotspot))
          if failures.size < maxRecallFailures then Iterator(d) ++ validateNext()
          else
            val limit =
              Diagnostics.Unknown(
                d.source,
                d.flags,
                d.dialect,
                Diagnostics.ErrorKind.RecallFailureLimit,
                Some(d.checker)
              )
            Iterator(d, limit)

    validateNext()

  /** Parses the pattern and the flags in the dialect, and builds its IR.
    *
    * It throws [[InvalidRegExpException]] on an invalid pattern, and [[UnsupportedException]] on a pattern nested
    * deeper than [[Parser.MaxNestingDepth]].
    */
  private[recheck] def parse(source: String, flags: String, dialect: Dialect)(using ctx: Context): IRPattern =
    ctx.log("parse: start")
    val flagSet = ctx.interrupt(parseFlags(flags, dialect))
    val node = ctx.interrupt(parseSource(source, flagSet, dialect))
    val mode = ctx.interrupt(Parser.determineUnitMode(source, flagSet, dialect))
    val ir = ctx.interrupt(buildIR(node, flagSet, dialect, mode))
    ctx.log(s"parse: finish\n  pattern: /$source/$flags")
    ir

  private def parseFlags(flags: String, dialect: Dialect): FlagSet =
    try FlagSet.parse(flags, dialect)
    catch case ex: FlagSetException => throw new InvalidRegExpException(ex.getMessage)

  private def parseSource(source: String, flagSet: FlagSet, dialect: Dialect): Node =
    try Parser.parse(source, flagSet, dialect)
    catch
      case ex: ParsingException      => throw new InvalidRegExpException(ex.getMessage)
      case ex: NestingLimitException => throw new UnsupportedException(ex.getMessage)

  private def buildIR(node: Node, flagSet: FlagSet, dialect: Dialect, mode: UnitMode): IRPattern =
    try IRBuilder.build(node, flagSet, dialect, mode)
    catch case ex: IRBuildingException => throw new InvalidRegExpException(ex.getMessage)

  /** RecallKey identifies an attack on recall validation by the degree of the polynomial complexity (or `None` for the
    * exponential one) and the hotspot.
    */
  private[recheck] final case class RecallKey(degree: Option[Int], spots: Seq[(Int, Int)])

  private[recheck] object RecallKey:
    def apply(complexity: AttackComplexity.Vulnerable, hotspot: Hotspot): RecallKey =
      val degree = complexity match
        case AttackComplexity.Polynomial(degree, _) => Some(degree)
        case AttackComplexity.Exponential(_)        => None
      RecallKey(degree, hotspot.spots.collect { case Hotspot.Spot(s, e, Hotspot.Heat) => (s, e) })

  /** Runs the automaton checker, and returns the vulnerable diagnostics it finds and a function giving the safe
    * diagnostics once they run out.
    */
  private[recheck] def checkAutomaton(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters
  )(using ctx: Context): (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe) =
    import params.*
    val maxNFASize = if checker == Checker.Auto then params.maxNFASize else Int.MaxValue

    def complexities: Iterator[Complexity[UChar]] =
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
        val epsNFA = EpsNFABuilder.build(ir).get
        val orderedNFA = epsNFA.toOrderedNFA(maxNFASize).rename.mapAlphabet(_.representative)
        AutomatonChecker.check(orderedNFA, maxNFASize)

    // The safe complexities follow the vulnerable ones.
    lazy val (vulnerable, rest) = complexities.span(_.isInstanceOf[Complexity.Vulnerable[?]])

    val diagnostics = vulnerable
      .collect:
        case vul: Complexity.Vulnerable[UChar] =>
          val attack = vul.buildAttackPattern(recallLimit, maxRecallStringSize)
          Diagnostics.Vulnerable(
            source,
            flags,
            params.dialect,
            ir.mode,
            vul.toAttackComplexity,
            attack,
            vul.hotspot,
            Checker.Automaton
          )
      .withChecker(Checker.Automaton)

    def safe(): Diagnostics.Safe =
      val complexity = rest.nextOption() match
        case Some(safe: Complexity.Safe) => safe.toAttackComplexity
        case _                           => AttackComplexity.Safe(false)
      Diagnostics.Safe(source, flags, params.dialect, complexity, Checker.Automaton)

    (diagnostics, safe)

  /** Runs the fuzz checker, and returns the vulnerable diagnostics it finds and a function giving the safe diagnostics
    * once they run out.
    *
    * `avoidsAttack` tells whether the fuzz checker avoids an attack of the complexity and the hotspot, which failed
    * recall validation.
    */
  private[recheck] def checkFuzz(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters,
      avoidsAttack: (AttackComplexity.Vulnerable, Hotspot) => Boolean = (_, _) => false
  )(using ctx: Context): (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe) =
    import params._

    def diagnose: Iterator[Diagnostics.Vulnerable] =
      val random = new Random(randomSeed)
      val fuzz = FuzzProgram.from(ir).get
      val results = FuzzChecker.check(
        ir,
        fuzz,
        random,
        seeder,
        maxSimpleRepeatCount,
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
        maxIteration,
        maxDegree,
        heatRatio,
        accelerationMode,
        avoidsAttack
      )

      results.map:
        case (complexity, attack, hotspot) =>
          val recallAttack = attack.adjust(complexity, recallLimit, maxRecallStringSize)
          Diagnostics.Vulnerable(
            source,
            flags,
            params.dialect,
            ir.mode,
            complexity,
            recallAttack,
            hotspot,
            Checker.Fuzz
          )

    val diagnostics = diagnose.withChecker(Checker.Fuzz)
    (diagnostics, () => Diagnostics.Safe(source, flags, params.dialect, AttackComplexity.Safe(true), Checker.Fuzz))

  /** Runs the automaton checker, or the fuzz checker if the automaton checker gives up, and returns the vulnerable
    * diagnostics they find and a function giving the safe diagnostics of the checker that ran last.
    */
  private[recheck] def checkAuto(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters,
      avoidsAttack: (AttackComplexity.Vulnerable, Hotspot) => Boolean = (_, _) => false
  )(using ctx: Context): (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe) =
    val (automaton, automatonSafe) = checkAutomaton(source, flags, ir, params)
    var safe = automatonSafe
    val diagnostics = automaton.recoverWith:
      case _: UnsupportedException =>
        val (fuzz, fuzzSafe) = checkFuzz(source, flags, ir, params, avoidsAttack)
        safe = fuzzSafe
        fuzz
    (diagnostics, () => safe())

  /** Runs recall validation against a vulnerable diagnostics, and records the result in it.
    *
    * It runs on the runtime of the dialect when this platform can run it.
    */
  private[recheck] def recallValidation(
      source: String,
      flags: String,
      dialect: Dialect,
      mode: UnitMode,
      timeout: Duration
  )(
      d: Diagnostics.Vulnerable
  )(using ctx: Context): Diagnostics.Vulnerable =
    RecallValidator.defaultExec match
      case Some(exec) =>
        try
          d.copy(recallValidation =
            RecallValidator.validate(source, flags, dialect, mode, d.attack, timeout, sys.env)(exec)
          )
        catch
          case ex: ReDoSException =>
            ex.checker = Some(if d.checker == Checker.Fuzz then Checker.Fuzz else Checker.Automaton)
            throw ex
      case None => d
