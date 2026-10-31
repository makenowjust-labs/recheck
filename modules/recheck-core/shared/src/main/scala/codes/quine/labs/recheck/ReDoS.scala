package codes.quine.labs.recheck

import scala.concurrent.duration.Duration
import scala.util.Failure
import scala.util.Random
import scala.util.Success
import scala.util.Try

import codes.quine.labs.recheck.automaton.AutomatonChecker
import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.common.CancellationToken
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.ReDoSException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.exec.NodeExecutor
import codes.quine.labs.recheck.fuzz.FuzzChecker
import codes.quine.labs.recheck.fuzz.FuzzProgram
import codes.quine.labs.recheck.recall.RecallValidator
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.regexp.PatternExtensions.*
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ir.IREncoding

/** ReDoS is a ReDoS checker frontend. */
object ReDoS:

  /** Tests the given RegExp pattern causes ReDoS.
    *
    * @param dialect
    *   the dialect of the pattern, which is not a public parameter yet
    * @param encoding
    *   the kind of strings of the dialect (see [[IREncoding]]), or `None` for the dialect's default
    */
  def check(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None,
      dialect: Dialect = Dialect.JavaScript,
      encoding: Option[IREncoding] = None
  ): Diagnostics =
    import params._

    given ctx: Context = Context(params.timeout, token, logger)

    val result = for
      _ <- Try(()) // Ensures `Try` context.
      _ = ctx.log("parse: start")
      pattern <- ctx.interrupt(ParsedPattern.parse(source, flags, dialect, encoding).toTry)
      _ = ctx.log(s"parse: finish\n  pattern: $pattern")
      diagnostics <- checker match
        case Checker.Automaton => checkAutomaton(source, flags, pattern, params)
        case Checker.Fuzz      => checkFuzz(source, flags, pattern, params)
        case Checker.Auto      => checkAuto(source, flags, pattern, params)
    yield diagnostics
    result
      .recover:
        case ex: ReDoSException => Diagnostics.Unknown.from(source, flags, ex)
      .get

  private[recheck] def checkAutomaton(
      source: String,
      flags: String,
      pattern: ParsedPattern,
      params: Parameters
  )(using ctx: Context): Try[Diagnostics] =
    import params.*
    val maxNFASize = if checker == Checker.Auto then params.maxNFASize else Int.MaxValue

    val result = for
      _ <- Try(()) // Ensures `Try` context.
      _ <-
        if checker == Checker.Auto && pattern.repeatCount >= maxRepeatCount then
          ctx.log("auto: exceed maxRepeatCount")
          Failure(UnsupportedException("The pattern contains too many repeat"))
        else Success(())
      complexity <-
        // When the pattern has no infinite repetition, then it is safe.
        if pattern.isConstant then
          ctx.log("automaton: constant pattern")
          Success(Iterator.empty)
        else
          for
            _ <-
              if checker == Checker.Auto && pattern.size >= maxPatternSize then
                ctx.log("auto: exceed maxPatternSize")
                Failure(UnsupportedException("The pattern is too large"))
              else Success(())
            epsNFA <- EpsNFABuilder.build(pattern)
            orderedNFA <- Try(epsNFA.toOrderedNFA(maxNFASize).rename.mapAlphabet(_.head))
          yield AutomatonChecker.check(orderedNFA, maxNFASize)
    yield complexity

    result
      .map: cs =>
        cs.map:
          case vul: Complexity.Vulnerable[Int] =>
            val attack = vul.buildAttackPattern(recallLimit, maxRecallStringSize)
            Diagnostics.Vulnerable(source, flags, vul.toAttackComplexity, attack, vul.hotspot, Checker.Automaton)
          case safe: Complexity.Safe => Diagnostics.Safe(source, flags, safe.toAttackComplexity, Checker.Automaton)
        ++ Iterator(Diagnostics.Safe(source, flags, AttackComplexity.Safe(false), Checker.Automaton))
      .map(recallValidation(pattern, recallTimeout))
      .recoverWith:
        case ex: ReDoSException =>
          ex.checker = Some(Checker.Automaton)
          Failure(ex)

  private[recheck] def checkFuzz(source: String, flags: String, pattern: ParsedPattern, params: Parameters)(using
      ctx: Context
  ): Try[Diagnostics] =
    import params._

    val random = new Random(randomSeed)

    val result = FuzzProgram
      .from(pattern)
      .map: fuzz =>
        FuzzChecker.check(
          pattern,
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
          accelerationMode
        )

    result
      .map: rs =>
        rs.map:
          case (complexity, attack, hotspot) =>
            val recallAttack = attack.adjust(complexity, recallLimit, maxRecallStringSize)
            Diagnostics.Vulnerable(source, flags, complexity, recallAttack, hotspot, Checker.Fuzz)
        ++ Iterator(Diagnostics.Safe(source, flags, AttackComplexity.Safe(true), Checker.Fuzz))
      .map(recallValidation(pattern, recallTimeout))
      .recoverWith:
        case ex: ReDoSException =>
          ex.checker = Some(Checker.Fuzz)
          Failure(ex)

  private[recheck] def checkAuto(source: String, flags: String, pattern: ParsedPattern, params: Parameters)(using
      ctx: Context
  ): Try[Diagnostics] =
    checkAutomaton(source, flags, pattern, params)
      .recoverWith:
        case _: UnsupportedException => checkFuzz(source, flags, pattern, params)

  /** Runs recall validation against diagnostics, then returns validated diagnostics.
    *
    * It runs only for JavaScript, by Node.js.
    */
  private[recheck] def recallValidation(pattern: ParsedPattern, timeout: Duration)(
      ds: Iterator[Diagnostics]
  )(using ctx: Context): Diagnostics =
    ds.filter:
      case d: Diagnostics.Vulnerable if pattern.dialect == Dialect.JavaScript =>
        RecallValidator.checks(pattern.source, pattern.flags, d.attack, timeout)(NodeExecutor.exec)
      case _ => true
    .next()
