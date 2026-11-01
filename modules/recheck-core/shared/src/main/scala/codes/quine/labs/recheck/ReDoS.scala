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
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.fuzz.FuzzChecker
import codes.quine.labs.recheck.fuzz.FuzzProgram
import codes.quine.labs.recheck.recall.RecallValidator
import codes.quine.labs.recheck.util.DiagnosticsIteratorUtil.recovering
import codes.quine.labs.recheck.util.DiagnosticsIteratorUtil.tagging
import codes.quine.labs.recheck.util.DiagnosticsIteratorUtil.takeUntilAccepted
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetException
import codes.quine.labs.resyntax.ast.Node
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
    * The pattern and the flags are of `params.dialect`. It returns the last diagnostics of [[checkAll]], and computes
    * no more than it.
    */
  def check(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None
  ): Diagnostics =
    iterator(source, flags, params, token).reduce((_, d) => d)

  /** Tests the given RegExp pattern causes ReDoS, and returns all the diagnostics until the accepted one.
    *
    * The list consists of the vulnerable diagnostics whose recall validation failed, followed by the accepted one: a
    * vulnerable diagnostics whose recall validation is not run or succeeded, or the safe or unknown one at the end.
    *
    * A vulnerable diagnostics with the same complexity and hotspot as a failed one is skipped. When the recall
    * validation fails `maxRecallFailures` times, the last one is unknown with
    * [[Diagnostics.ErrorKind.RecallFailureLimit]].
    */
  def checkAll(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None
  ): List[Diagnostics] =
    iterator(source, flags, params, token).toList

  /** Iterates the diagnostics of [[checkAll]].
    *
    * Recall validation runs on `exec`, or does not run if it is `None`. The environment variables `env` may replace the
    * commands of the runtimes.
    */
  private[recheck] def iterator(
      source: String,
      flags: String,
      params: Parameters,
      token: Option[CancellationToken],
      exec: Context ?=> Option[RecallValidator.Exec] = RecallValidator.defaultExec,
      env: Map[String, String] = sys.env
  ): Iterator[Diagnostics] =
    given ctx: Context = Context(params.timeout, token, params.logger)

    def ds: Iterator[Diagnostics] =
      val ir = parse(source, flags, params.dialect)
      val style = AttackPattern.Style.of(params.dialect, ir.mode)
      val failures = mutable.Set.empty[RecallKey]
      val failed = (c: AttackComplexity.Vulnerable, h: Hotspot) => failures.contains(RecallKey(c, h))
      val candidates = params.checker match
        case Checker.Automaton => checkAutomaton(source, flags, ir, style, params)
        case Checker.Fuzz      => checkFuzz(source, flags, ir, style, params, failed)
        case Checker.Auto      => checkAuto(source, flags, ir, style, params, failed)
      val validate = recallValidation(source, flags, params.dialect, ir.mode, params.recallTimeout, exec, env)
      candidates.flatMap:
        case d: Diagnostics.Vulnerable =>
          val key = RecallKey(d.complexity, d.hotspot)
          if failures.contains(key) then
            ctx.log("recall: skip the same attack as a failed one")
            Iterator.empty
          else
            val v = validate(d)
            if v.isAccepted then Iterator(v)
            else
              failures.add(key)
              if failures.size < params.maxRecallFailures then Iterator(v)
              else
                val limit =
                  Diagnostics.Unknown(
                    source,
                    flags,
                    params.dialect,
                    Diagnostics.ErrorKind.RecallFailureLimit,
                    Some(d.checker)
                  )
                Iterator(v, limit)
        case d => Iterator(d)

    val all = recovering(ds):
      case ex: ReDoSException => Iterator(Diagnostics.Unknown.from(source, flags, params.dialect, ex))
    takeUntilAccepted(all)

  /** Parses the pattern and the flags in the dialect, and builds its IR.
    *
    * It throws [[InvalidRegExpException]] on an invalid pattern, and [[UnsupportedException]] on a pattern nested
    * deeper than [[Parser.MaxNestingDepth]].
    */
  private[recheck] def parse(source: String, flags: String, dialect: Dialect)(using ctx: Context): IRPattern =
    ctx.log("parse: start")
    val flagSet = ctx.interrupt(parseFlags(flags, dialect))
    val node = ctx.interrupt(parseSource(source, flagSet, dialect))
    val ir = ctx.interrupt(buildIR(node, flagSet, dialect))
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

  private def buildIR(node: Node, flagSet: FlagSet, dialect: Dialect): IRPattern =
    try IRBuilder.build(node, flagSet, dialect)
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

  /** Iterates the diagnostics of the automaton checker, ending with the safe one. */
  private[recheck] def checkAutomaton(
      source: String,
      flags: String,
      ir: IRPattern,
      style: AttackPattern.Style,
      params: Parameters
  )(using ctx: Context): Iterator[Diagnostics] = tagging(Checker.Automaton):
    import params.*
    val maxNFASize = if checker == Checker.Auto then params.maxNFASize else Int.MaxValue

    val complexities =
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
        val orderedNFA = epsNFA.toOrderedNFA(maxNFASize).rename.mapAlphabet(_.head)
        AutomatonChecker.check(orderedNFA, maxNFASize)

    complexities.map:
      case vul: Complexity.Vulnerable[Int] =>
        val attack = vul.buildAttackPattern(recallLimit, maxRecallStringSize).copy(style = style)
        Diagnostics.Vulnerable(
          source,
          flags,
          params.dialect,
          vul.toAttackComplexity,
          attack,
          vul.hotspot,
          Checker.Automaton
        )
      case safe: Complexity.Safe =>
        Diagnostics.Safe(source, flags, params.dialect, safe.toAttackComplexity, Checker.Automaton)
    ++ Iterator(Diagnostics.Safe(source, flags, params.dialect, AttackComplexity.Safe(false), Checker.Automaton))

  /** Iterates the diagnostics of the fuzz checker, ending with the safe one.
    *
    * `failed` tells whether an attack of the complexity and the hotspot failed recall validation, which the fuzz
    * checker avoids.
    */
  private[recheck] def checkFuzz(
      source: String,
      flags: String,
      ir: IRPattern,
      style: AttackPattern.Style,
      params: Parameters,
      failed: (AttackComplexity.Vulnerable, Hotspot) => Boolean = (_, _) => false
  )(using ctx: Context): Iterator[Diagnostics] = tagging(Checker.Fuzz):
    import params._

    val random = new Random(randomSeed)
    val fuzz = FuzzProgram.from(ir, dialect).get
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
      failed
    )

    results.map:
      case (complexity, attack, hotspot) =>
        val recallAttack =
          attack.adjust(complexity, recallLimit, maxRecallStringSize).copy(style = style)
        Diagnostics.Vulnerable(source, flags, params.dialect, complexity, recallAttack, hotspot, Checker.Fuzz)
    ++ Iterator(Diagnostics.Safe(source, flags, params.dialect, AttackComplexity.Safe(true), Checker.Fuzz))

  /** Iterates the diagnostics of the automaton checker, or of the fuzz checker if the automaton checker gives up. */
  private[recheck] def checkAuto(
      source: String,
      flags: String,
      ir: IRPattern,
      style: AttackPattern.Style,
      params: Parameters,
      failed: (AttackComplexity.Vulnerable, Hotspot) => Boolean = (_, _) => false
  )(using ctx: Context): Iterator[Diagnostics] =
    recovering(checkAutomaton(source, flags, ir, style, params)):
      case _: UnsupportedException => checkFuzz(source, flags, ir, style, params, failed)

  /** Runs recall validation against a vulnerable diagnostics, and records the result in it.
    *
    * It runs on the runtime of the dialect when this platform can run it.
    */
  private[recheck] def recallValidation(
      source: String,
      flags: String,
      dialect: Dialect,
      mode: UnitMode,
      timeout: Duration,
      exec: Option[RecallValidator.Exec],
      env: Map[String, String]
  )(
      d: Diagnostics.Vulnerable
  )(using ctx: Context): Diagnostics.Vulnerable =
    exec match
      case Some(exec) =>
        try
          d.copy(recallValidation =
            RecallValidator.validate(source, flags, dialect, mode, d.attack, timeout, env)(exec)
          )
        catch
          case ex: ReDoSException =>
            ex.checker = Some(if d.checker == Checker.Fuzz then Checker.Fuzz else Checker.Automaton)
            throw ex
      case None => d
