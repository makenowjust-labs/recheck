package codes.quine.labs.recheck

import scala.collection.AbstractIterator
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
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.regexp.PatternExtensions.*
import codes.quine.labs.resyntax.ast.DialectOptions
import codes.quine.labs.resyntax.ast.Encoding
import codes.quine.labs.resyntax.ast.PerlSource

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

  /** Returns the dialect's options of the parameters.
    *
    * It throws [[InvalidRegExpException]] if the dialect does not have the encoding.
    */
  private[recheck] def dialectOptions(params: Parameters): DialectOptions =
    val encoding = params.encoding.map: name =>
      Encoding
        .of(params.dialect, name)
        .getOrElse:
          throw new InvalidRegExpException(s"Unknown encoding for ${params.dialect}: $name")
    DialectOptions(params.perlSource, encoding, params.perlUtf8)

  /** Iterates the diagnostics of [[checkAll]].
    *
    * @param exec
    *   the executor for recall validation, or `None` if commands cannot run
    * @param env
    *   the environment variables giving the runtime command lines of recall validation
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
      ctx.log("parse: start")
      val options = dialectOptions(params)
      val pattern = ctx.interrupt(ParsedPattern.parse(source, flags, params.dialect, options).toTry.get)
      ctx.log(s"parse: finish\n  pattern: $pattern")
      val failures = mutable.Set.empty[RecallKey]
      val failed = (c: AttackComplexity.Vulnerable, h: Hotspot) => failures.contains(RecallKey(c, h))
      val candidates = params.checker match
        case Checker.Automaton => checkAutomaton(source, flags, pattern, params)
        case Checker.Fuzz      => checkFuzz(source, flags, pattern, params, failed)
        case Checker.Auto      => checkAuto(source, flags, pattern, params, failed)
      val validate = recallValidation(pattern, options, params.recallTimeout, exec, env)
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
                  Diagnostics.Unknown(source, flags, Diagnostics.ErrorKind.RecallFailureLimit, Some(d.checker))
                Iterator(v, limit)
        case d => Iterator(d)

    val all = recovering(ds):
      case ex: ReDoSException => Iterator(Diagnostics.Unknown.from(source, flags, ex))
    takeUntilAccepted(all)

  /** RecallKey identifies an attack on recall validation by the complexity and the hotspot.
    *
    * @param degree
    *   the degree of the polynomial complexity, or `None` for the exponential one
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
        val attack = vul.buildAttackPattern(recallLimit, maxRecallStringSize).copy(style = attackStyle(pattern))
        Diagnostics.Vulnerable(source, flags, vul.toAttackComplexity, attack, vul.hotspot, Checker.Automaton)
      case safe: Complexity.Safe => Diagnostics.Safe(source, flags, safe.toAttackComplexity, Checker.Automaton)
    ++ Iterator(Diagnostics.Safe(source, flags, AttackComplexity.Safe(false), Checker.Automaton))

  /** Iterates the diagnostics of the fuzz checker, ending with the safe one.
    *
    * @param failed
    *   whether an attack of the complexity and the hotspot failed recall validation, which the fuzz checker avoids
    */
  private[recheck] def checkFuzz(
      source: String,
      flags: String,
      pattern: ParsedPattern,
      params: Parameters,
      failed: (AttackComplexity.Vulnerable, Hotspot) => Boolean = (_, _) => false
  )(using ctx: Context): Iterator[Diagnostics] = tagging(Checker.Fuzz):
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
      accelerationMode,
      failed
    )

    results.map:
      case (complexity, attack, hotspot) =>
        val recallAttack =
          attack.adjust(complexity, recallLimit, maxRecallStringSize).copy(style = attackStyle(pattern))
        Diagnostics.Vulnerable(source, flags, complexity, recallAttack, hotspot, Checker.Fuzz)
    ++ Iterator(Diagnostics.Safe(source, flags, AttackComplexity.Safe(true), Checker.Fuzz))

  /** Iterates the diagnostics of the automaton checker, or of the fuzz checker if the automaton checker gives up. */
  private[recheck] def checkAuto(
      source: String,
      flags: String,
      pattern: ParsedPattern,
      params: Parameters,
      failed: (AttackComplexity.Vulnerable, Hotspot) => Boolean = (_, _) => false
  )(using ctx: Context): Iterator[Diagnostics] =
    recovering(checkAutomaton(source, flags, pattern, params)):
      case _: UnsupportedException => checkFuzz(source, flags, pattern, params, failed)

  /** Returns the style of the attack strings of the pattern, which is the host language of its dialect. */
  private[recheck] def attackStyle(pattern: ParsedPattern): AttackPattern.Style =
    AttackPattern.Style.of(pattern.dialect, pattern.ir.mode)

  /** Runs recall validation against a vulnerable diagnostics, and records the result in it.
    *
    * It runs on the runtime of the dialect when this platform can run it.
    */
  private[recheck] def recallValidation(
      pattern: ParsedPattern,
      options: DialectOptions,
      timeout: Duration,
      exec: Option[RecallValidator.Exec],
      env: Map[String, String]
  )(
      d: Diagnostics.Vulnerable
  )(using ctx: Context): Diagnostics.Vulnerable =
    exec match
      case Some(exec) =>
        val perlLiteral = options.perlSource == PerlSource.Literal
        try d.copy(recallValidation = RecallValidator.validate(pattern, d.attack, timeout, perlLiteral, env)(exec))
        catch
          case ex: ReDoSException =>
            ex.checker = Some(if d.checker == Checker.Fuzz then Checker.Fuzz else Checker.Automaton)
            throw ex
      case None => d

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
