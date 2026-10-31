package codes.quine.labs.recheck

import scala.collection.AbstractIterator
import scala.concurrent.duration.Duration
import scala.util.Random

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
import codes.quine.labs.recheck.fuzz.FuzzChecker
import codes.quine.labs.recheck.fuzz.FuzzProgram
import codes.quine.labs.recheck.recall.RecallValidator
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.regexp.PatternExtensions.*
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.DialectOptions
import codes.quine.labs.resyntax.ast.PerlSource

/** ReDoS is a ReDoS checker frontend. */
object ReDoS:

  /** Tests the given RegExp pattern causes ReDoS.
    *
    * It returns the last diagnostics of [[checkAll]], and computes no more than it.
    *
    * @param dialect
    *   the dialect of the pattern, which is not a public parameter yet
    * @param options
    *   the dialect's options (see [[DialectOptions]])
    */
  def check(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None,
      dialect: Dialect = Dialect.JavaScript,
      options: DialectOptions = DialectOptions.Default
  ): Diagnostics =
    iterator(source, flags, params, token, dialect, options).reduce((_, d) => d)

  /** Tests the given RegExp pattern causes ReDoS, and returns all the diagnostics until the accepted one.
    *
    * The list consists of the vulnerable diagnostics whose recall validation failed, followed by the accepted one: a
    * vulnerable diagnostics whose recall validation is not run or succeeded, or the safe or unknown one at the end.
    */
  def checkAll(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None,
      dialect: Dialect = Dialect.JavaScript,
      options: DialectOptions = DialectOptions.Default
  ): List[Diagnostics] =
    iterator(source, flags, params, token, dialect, options).toList

  /** Iterates the diagnostics of [[checkAll]].
    *
    * @param exec
    *   the executor for recall validation, or `None` if commands cannot run
    */
  private[recheck] def iterator(
      source: String,
      flags: String,
      params: Parameters,
      token: Option[CancellationToken],
      dialect: Dialect,
      options: DialectOptions,
      exec: Context ?=> Option[RecallValidator.Exec] = RecallValidator.defaultExec
  ): Iterator[Diagnostics] =
    given ctx: Context = Context(params.timeout, token, params.logger)

    def ds: Iterator[Diagnostics] =
      ctx.log("parse: start")
      val pattern = ctx.interrupt(ParsedPattern.parse(source, flags, dialect, options).toTry.get)
      ctx.log(s"parse: finish\n  pattern: $pattern")
      val candidates = params.checker match
        case Checker.Automaton => checkAutomaton(source, flags, pattern, params)
        case Checker.Fuzz      => checkFuzz(source, flags, pattern, params)
        case Checker.Auto      => checkAuto(source, flags, pattern, params)
      candidates.map(recallValidation(pattern, options, params.recallTimeout, exec))

    val all = recovering(ds):
      case ex: ReDoSException => Iterator(Diagnostics.Unknown.from(source, flags, ex))
    takeUntilAccepted(all)

  /** Iterates the diagnostics of the automaton checker, ending with the safe one. */
  private[recheck] def checkAutomaton(
      source: String,
      flags: String,
      pattern: ParsedPattern,
      params: Parameters
  )(using ctx: Context): Iterator[Diagnostics] = tagging(Checker.Automaton):
    import params.*
    val maxNFASize = if checker == Checker.Auto then params.maxNFASize else Int.MaxValue

    val complexities =
      if checker == Checker.Auto && pattern.repeatCount >= maxRepeatCount then
        ctx.log("auto: exceed maxRepeatCount")
        throw UnsupportedException("The pattern contains too many repeat")
      // When the pattern has no infinite repetition, then it is safe.
      else if pattern.isConstant then
        ctx.log("automaton: constant pattern")
        Iterator.empty
      else if checker == Checker.Auto && pattern.size >= maxPatternSize then
        ctx.log("auto: exceed maxPatternSize")
        throw UnsupportedException("The pattern is too large")
      else
        val epsNFA = EpsNFABuilder.build(pattern).get
        val orderedNFA = epsNFA.toOrderedNFA(maxNFASize).rename.mapAlphabet(_.head)
        AutomatonChecker.check(orderedNFA, maxNFASize)

    complexities.map:
      case vul: Complexity.Vulnerable[Int] =>
        val attack = vul.buildAttackPattern(recallLimit, maxRecallStringSize)
        Diagnostics.Vulnerable(source, flags, vul.toAttackComplexity, attack, vul.hotspot, Checker.Automaton)
      case safe: Complexity.Safe => Diagnostics.Safe(source, flags, safe.toAttackComplexity, Checker.Automaton)
    ++ Iterator(Diagnostics.Safe(source, flags, AttackComplexity.Safe(false), Checker.Automaton))

  /** Iterates the diagnostics of the fuzz checker, ending with the safe one. */
  private[recheck] def checkFuzz(source: String, flags: String, pattern: ParsedPattern, params: Parameters)(using
      ctx: Context
  ): Iterator[Diagnostics] = tagging(Checker.Fuzz):
    import params._

    val random = new Random(randomSeed)
    val fuzz = FuzzProgram.from(pattern).get
    val results = FuzzChecker.check(
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

    results.map:
      case (complexity, attack, hotspot) =>
        val recallAttack = attack.adjust(complexity, recallLimit, maxRecallStringSize)
        Diagnostics.Vulnerable(source, flags, complexity, recallAttack, hotspot, Checker.Fuzz)
    ++ Iterator(Diagnostics.Safe(source, flags, AttackComplexity.Safe(true), Checker.Fuzz))

  /** Iterates the diagnostics of the automaton checker, or of the fuzz checker if the automaton checker gives up. */
  private[recheck] def checkAuto(source: String, flags: String, pattern: ParsedPattern, params: Parameters)(using
      ctx: Context
  ): Iterator[Diagnostics] =
    recovering(checkAutomaton(source, flags, pattern, params)):
      case _: UnsupportedException => checkFuzz(source, flags, pattern, params)

  /** Runs recall validation against a vulnerable diagnostics, and records the result in it.
    *
    * It runs on the runtime of the dialect when this platform can run it.
    */
  private[recheck] def recallValidation(
      pattern: ParsedPattern,
      options: DialectOptions,
      timeout: Duration,
      exec: Option[RecallValidator.Exec]
  )(
      d: Diagnostics
  )(using ctx: Context): Diagnostics = d match
    case d: Diagnostics.Vulnerable =>
      exec match
        case Some(exec) =>
          try
            d.copy(recallValidation =
              RecallValidator.validate(pattern, d.attack, timeout, options.perlSource == PerlSource.Literal)(exec)
            )
          catch
            case ex: ReDoSException =>
              ex.checker = Some(if d.checker == Checker.Fuzz then Checker.Fuzz else Checker.Automaton)
              throw ex
        case None => d
    case d => d

  /** Iterates the diagnostics until the accepted one, without computing the next one. */
  private def takeUntilAccepted(ds: Iterator[Diagnostics]): Iterator[Diagnostics] = new AbstractIterator[Diagnostics]:
    private var accepted = false

    def hasNext: Boolean = !accepted && ds.hasNext

    def next(): Diagnostics =
      if !hasNext then throw new NoSuchElementException
      val d = ds.next()
      accepted = d match
        case v: Diagnostics.Vulnerable => v.isAccepted
        case _                         => true
      d

  /** Sets the checker to an exception thrown on building or iterating the diagnostics. */
  private def tagging(checker: Checker.Used)(ds: => Iterator[Diagnostics]): Iterator[Diagnostics] =
    recovering(ds):
      case ex: ReDoSException if ex.checker.isEmpty =>
        ex.checker = Some(checker)
        throw ex

  /** Iterates the diagnostics, and continues with the recovery when an exception is thrown on the iteration.
    *
    * It computes the next element on `hasNext` so that an exception is thrown there.
    */
  private def recovering(ds: => Iterator[Diagnostics])(
      recover: PartialFunction[Throwable, Iterator[Diagnostics]]
  ): Iterator[Diagnostics] = new AbstractIterator[Diagnostics]:
    private var current: Iterator[Diagnostics] = null
    private var recovered = false
    private var buffer: Option[Diagnostics] = None

    def hasNext: Boolean =
      if buffer.isEmpty then buffer = fetch()
      buffer.isDefined

    def next(): Diagnostics =
      if !hasNext then throw new NoSuchElementException
      val d = buffer.get
      buffer = None
      d

    private def fetch(): Option[Diagnostics] =
      try
        if current eq null then current = ds
        Option.when(current.hasNext)(current.next())
      catch
        case ex if !recovered && recover.isDefinedAt(ex) =>
          recovered = true
          current = recover(ex)
          Option.when(current.hasNext)(current.next())
