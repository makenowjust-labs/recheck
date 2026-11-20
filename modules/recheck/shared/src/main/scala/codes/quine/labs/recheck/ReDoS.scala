package codes.quine.labs.recheck

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.Random

import codes.quine.labs.recheck.automaton.AutomatonChecker
import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.context.CancellationToken
import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.DiagnosticsIteratorUtil.*
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.RefutedAttack
import codes.quine.labs.recheck.diagnostics.UpperBound
import codes.quine.labs.recheck.exception.InvalidRegExpException
import codes.quine.labs.recheck.exception.ReDoSException
import codes.quine.labs.recheck.exception.UnsupportedException
import codes.quine.labs.recheck.fuzz.FuzzChecker
import codes.quine.labs.recheck.fuzz.FuzzProgram
import codes.quine.labs.recheck.parameters.AttackStringCharacteristics
import codes.quine.labs.recheck.parameters.Checker
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.recheck.verification.RuntimeAttackVerifier
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetException
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet
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
    * skipped (see [[AttackKey]]). When runtime-attack verification refutes `maxRefutedAttacks` attacks, the result is
    * unknown with [[Diagnostics.ErrorKind.RuntimeAttacksRefuted]].
    */
  def check(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None
  ): Diagnostics =
    given ctx: Context = Context(params.timeout, token, params.logger)

    try
      val ir = parse(source, flags, params.dialect)
      val verify = runRuntimeAttack(
        source,
        flags,
        params.dialect,
        ir.mode,
        params.runtimeAttackTimeout,
        params.runtimeAttackCompileTimeout
      )
      diagnose(source, flags, ir, params, verify)
    catch case ex: ReDoSException => Diagnostics.Unknown.from(source, flags, params.dialect, None, Seq.empty, ex, None)

  /** Runs the checker of the parameters on the pattern with `verify` as the runtime-attack verification, and returns
    * the diagnostics of [[check]].
    *
    * With the auto checker, when every attack of the automaton checker is refuted, the fuzz checker looks for another
    * attack. Those attacks are its upper bound and its seeds (see [[FuzzChecker.check]]). If it does not finish, for
    * the time, for the limit of the refuted attacks or for a pattern that it does not support, the result is the safe
    * diagnostics of the automaton checker, with the attacks refuted by then.
    */
  private[recheck] def diagnose(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters,
      verify: Diagnostics.Vulnerable => Diagnostics.Vulnerable
  )(using ctx: Context): Diagnostics =
    val refutedAttacks = mutable.ArrayBuffer.empty[RefutedAttack]
    val readStarts = ir.readStartLocations(params.maxRefutedAttackPartSize)
    val sets = characteristicSets(ir, params.attackStringCharacteristics)
    val avoidsAttack = (c: AttackComplexity.Vulnerable, a: AttackPattern, h: Hotspot) =>
      isRefuted(refutedAttacks, readStarts, sets, c, a, h)

    // A checker runs lazily on iterating the candidates, so that the errors of the checking are caught here.
    def run(
        checking: (
            Iterator[Diagnostics.Vulnerable],
            () => Diagnostics.Safe,
            () => Option[UpperBound],
            () => Diagnostics.Checker
        )
    ): Diagnostics =
      val (candidates, safe, upperBound, checker) = checking
      val diagnostics =
        try verifyCandidates(candidates, safe, verify, refutedAttacks, params.maxRefutedAttacks, readStarts, sets)
        catch
          case ex: ReDoSException =>
            val refuted = refutedAttacks.toSeq
            Diagnostics.Unknown.from(source, flags, params.dialect, Some(ir.mode), refuted, ex, Some(checker()))
      diagnostics.withUpperBound(upperBound())

    def isUnfinished(d: Diagnostics): Boolean = d match
      case d: Diagnostics.Unknown =>
        d.error match
          case Diagnostics.ErrorKind.Timeout | Diagnostics.ErrorKind.RuntimeAttacksRefuted => true
          case _: Diagnostics.ErrorKind.Unsupported                                        => true
          case _                                                                           => false
      case _ => false

    params.checker match
      case Checker.Automaton => run(checkAutomaton(source, flags, ir, params, mutable.ArrayBuffer.empty))
      case Checker.Fuzz      => run(checkFuzz(source, flags, ir, params, avoidsAttack, Seq.empty))
      case Checker.Auto      =>
        val attacks = mutable.ArrayBuffer.empty[Complexity.Vulnerable[UChar]]
        run(checkAuto(source, flags, ir, params, avoidsAttack, attacks)) match
          case safe: Diagnostics.Safe if safe.checker == Diagnostics.Checker.Automaton && refutedAttacks.nonEmpty =>
            ctx.log("auto: retry with the fuzz checker")
            val retried = run(checkFuzz(source, flags, ir, params, avoidsAttack, attacks.toSeq))
            if isUnfinished(retried) then
              ctx.log("auto: the fuzz checker did not finish")
              safe.copy(refutedAttacks = refutedAttacks.toSeq)
            else retried
          case diagnostics => diagnostics

  /** Runs `verify` on the candidates in order, and returns the final diagnostics of [[check]].
    *
    * `safe` gives the final diagnostics after the candidates run out. `refutedAttacks` collects the attacks that
    * runtime attacks refute. `readStarts` and `sets` are the ones of [[AttackKey]], by which a candidate is the same as
    * a refuted one.
    */
  private[recheck] def verifyCandidates(
      candidates: Iterator[Diagnostics.Vulnerable],
      safe: () => Diagnostics.Safe,
      verify: Diagnostics.Vulnerable => Diagnostics.Vulnerable,
      refutedAttacks: mutable.Buffer[RefutedAttack],
      maxRefutedAttacks: Int,
      readStarts: Set[SourceLocation],
      sets: Seq[CharSet]
  )(using ctx: Context): Diagnostics =
    val untried = candidates.filter: d =>
      val skipped = isRefuted(refutedAttacks, readStarts, sets, d.complexity, d.attack, d.hotspot)
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

  /** Whether an attack of the complexity, the string and the hotspot is the same as a refuted one (see [[AttackKey]]).
    */
  private[recheck] def isRefuted(
      refutedAttacks: collection.Seq[RefutedAttack],
      readStarts: Set[SourceLocation],
      sets: Seq[CharSet],
      complexity: AttackComplexity.Vulnerable,
      attack: AttackPattern,
      hotspot: Hotspot
  ): Boolean =
    val key = AttackKey(complexity, attack, hotspot, readStarts, sets)
    refutedAttacks.exists(r => AttackKey(r.complexity, r.attack, r.hotspot, readStarts, sets) == key)

  /** Returns the sets of characters that tell an attack string from another in [[AttackKey]]. */
  private[recheck] def characteristicSets(ir: IRPattern, characteristics: AttackStringCharacteristics): Seq[CharSet] =
    def atoms: Seq[CharSet] = PatternAlphabet.of(ir).pairs.map(_._1)

    characteristics match
      case AttackStringCharacteristics.None      => Seq.empty
      case AttackStringCharacteristics.Latin1    => Seq(ir.universe.diff(CharSet.range(0x00, 0xff)))
      case AttackStringCharacteristics.CharClass =>
        val literals = ir.literalSets
        atoms.filterNot(atom => literals.exists(atom.subsetOf))
      case AttackStringCharacteristics.Alphabet => atoms

  /** Parses the pattern and the flags in the dialect, and builds its IR.
    *
    * It throws [[InvalidRegExpException]] on an invalid pattern, and [[UnsupportedException]] on a pattern nested
    * deeper than [[Parser.MaxNestingDepth]] or with too many multi-character case foldings in a literal.
    */
  private[recheck] def parse(source: String, flags: String, dialect: Dialect)(using ctx: Context): IRPattern =
    ctx.log("parse: start")
    val flagSet = ctx.interrupt(parseFlags(flags, dialect))
    val mode = ctx.interrupt(Parser.determineUnitMode(source, flagSet, dialect))
    val node = ctx.interrupt(parseSource(source, flagSet, dialect, mode))
    val ir = ctx.interrupt(buildIR(node, flagSet, dialect, mode))
    ctx.log(s"parse: finish\n  pattern: /$source/$flags")
    ir

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

  /** AttackKey identifies an attack by the degree of the polynomial complexity (or `None` for the exponential one), the
    * spots of the hotspot and the characteristics of the attack string.
    *
    * The warm and cold spots tell apart the attacks that grow at the same hot spots through different parts of the
    * pattern, one of which an engine may refute while it confirms another (e.g. `x[^∆]*∆y` on `'x'.repeat(n)`, which V8
    * refutes as a Latin-1 string, and on a string with `∆`).
    *
    * The spots are the ones at `readStarts`, the locations where the engine starts to read the pattern (see
    * [[IRPattern.readStartLocations]]), which have a part of the pattern as its characters read first up to
    * `maxRefutedAttackPartSize`. The attacks that differ only in how many characters of a part they read after them are
    * the same one (e.g. `form.*edit_profile` on the repetitions of `formedit` and of `formedit_p`).
    *
    * The characteristics are the indices of `sets` (see [[characteristicSets]]) that have a character of the attack
    * string.
    */
  private[recheck] final case class AttackKey(
      degree: Option[Int],
      spots: Seq[Hotspot.Spot],
      characteristics: Set[Int]
  )

  private[recheck] object AttackKey:

    def apply(
        complexity: AttackComplexity.Vulnerable,
        attack: AttackPattern,
        hotspot: Hotspot,
        readStarts: Set[SourceLocation],
        sets: Seq[CharSet]
    ): AttackKey =
      val chars = (attack.pumps.iterator.flatMap((s, t, _) => s.iterator ++ t.iterator) ++ attack.suffix.iterator).toSet
      val characteristics = sets.indices.filter(i => chars.exists(sets(i).contains)).toSet
      val degree = complexity match
        case AttackComplexity.Polynomial(degree, _) => Some(degree)
        case AttackComplexity.Exponential(_)        => None
      val spots =
        for
          loc <- readStarts.toSeq.sortBy(loc => (loc.start, loc.end))
          spot <- hotspot.spots.find(spot => spot.start <= loc.start && loc.end <= spot.end)
        yield Hotspot.Spot(loc.start, loc.end, spot.temperature)
      AttackKey(degree, spots, characteristics)

  /** Runs the automaton checker, and returns the vulnerable diagnostics it finds, a function giving the safe
    * diagnostics once they run out, a function giving the upper bound, which is always `None`, and a function giving
    * the checker.
    *
    * `attacks` collects the vulnerable complexities of the diagnostics.
    */
  private[recheck] def checkAutomaton(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters,
      attacks: mutable.Buffer[Complexity.Vulnerable[UChar]]
  )(using
      ctx: Context
  ): (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe, () => Option[UpperBound], () => Diagnostics.Checker) =
    import params.*
    val maxNFASize = if checker == Checker.Auto then params.maxNFASize else Int.MaxValue
    val maxDFASize = if checker == Checker.Auto then params.maxDFASize else Int.MaxValue

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
        val epsNFA = EpsNFABuilder.build(ir, maxNFASize, maxBackReferenceStrings)
        val orderedNFA = epsNFA.toOrderedNFA(false, maxNFASize, maxDFASize).rename
        AutomatonChecker.check(
          epsNFA.restrictInputs(orderedNFA).mapAlphabet(alphabetRepresentative.of),
          false,
          maxNFASize,
          maxDFASize
        )

    // The safe complexities follow the vulnerable ones.
    lazy val (vulnerable, rest) = complexities.span(_.isInstanceOf[Complexity.Vulnerable[?]])

    val diagnostics = Iterator(()).flatMap: _ =>
      vulnerable.collect:
        case vul: Complexity.Vulnerable[UChar] =>
          attacks += vul
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
            Diagnostics.Checker.Automaton
          )

    def safe(): Diagnostics.Safe =
      val complexity = rest.nextOption() match
        case Some(safe: Complexity.Safe) => safe.toAttackComplexity
        case _                           => AttackComplexity.Safe(false)
      Diagnostics.Safe(source, flags, params.dialect, ir.mode, complexity, Seq.empty, Diagnostics.Checker.Automaton)

    (diagnostics, safe, () => None, () => Diagnostics.Checker.Automaton)

  /** Runs the fuzz checker, and returns the vulnerable diagnostics it finds, a function giving the safe diagnostics
    * once they run out, a function giving the upper bound found before fuzzing, and a function giving the checker.
    *
    * `avoidsAttack` tells whether the fuzz checker avoids an attack of the complexity, the string and the hotspot,
    * which a runtime attack refuted. `automatonAttacks` are the attacks that the automaton checker found, which are the
    * upper bound if there is any (see [[FuzzChecker.check]]).
    */
  private[recheck] def checkFuzz(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters,
      avoidsAttack: (AttackComplexity.Vulnerable, AttackPattern, Hotspot) => Boolean,
      automatonAttacks: Seq[Complexity.Vulnerable[UChar]]
  )(using
      ctx: Context
  ): (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe, () => Option[UpperBound], () => Diagnostics.Checker) =
    import params.*

    var upperBound = Option.empty[UpperBound]

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
        maxExtraDegree,
        maxIncubationBoundedWidth,
        maxBoostIteration,
        boundedWidthRatio,
        accelerationMode,
        accelerationHitRatio,
        alphabetRepresentative,
        avoidsAttack,
        automatonAttacks
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
            Diagnostics.Checker.Fuzz
          )

    val diagnostics = Iterator(()).flatMap(_ => diagnose)
    val safe = () =>
      Diagnostics.Safe(source, flags, params.dialect, ir.mode, safeComplexity, Seq.empty, Diagnostics.Checker.Fuzz)
    (diagnostics, safe, () => upperBound, () => Diagnostics.Checker.Fuzz)

  /** Runs the automaton checker, or the fuzz checker if the automaton checker gives up, and returns the vulnerable
    * diagnostics they find, and functions giving the safe diagnostics and the upper bound of the checker that ran last,
    * and that checker.
    *
    * `attacks` collects the vulnerable complexities of the diagnostics of the automaton checker.
    */
  private[recheck] def checkAuto(
      source: String,
      flags: String,
      ir: IRPattern,
      params: Parameters,
      avoidsAttack: (AttackComplexity.Vulnerable, AttackPattern, Hotspot) => Boolean,
      attacks: mutable.Buffer[Complexity.Vulnerable[UChar]]
  )(using
      ctx: Context
  ): (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe, () => Option[UpperBound], () => Diagnostics.Checker) =
    val (automaton, automatonSafe, automatonUpperBound, automatonChecker) =
      checkAutomaton(source, flags, ir, params, attacks)
    var safe = automatonSafe
    var upperBound = automatonUpperBound
    var checker = automatonChecker
    val diagnostics = automaton.recoverWith:
      case _: UnsupportedException =>
        val (fuzz, fuzzSafe, fuzzUpperBound, fuzzChecker) =
          checkFuzz(source, flags, ir, params, avoidsAttack, Seq.empty)
        safe = fuzzSafe
        upperBound = fuzzUpperBound
        checker = fuzzChecker
        fuzz
    (diagnostics, () => safe(), () => upperBound(), () => checker())

  /** Runs the attack in the vulnerable diagnostics on the runtime of the dialect, and records the result in them. */
  private[recheck] def runRuntimeAttack(
      source: String,
      flags: String,
      dialect: Dialect,
      mode: UnitMode,
      timeout: Duration,
      compileTimeout: Duration
  )(
      d: Diagnostics.Vulnerable
  )(using ctx: Context): Diagnostics.Vulnerable =
    val runtimeAttack =
      RuntimeAttackVerifier.verify(source, flags, dialect, mode, d.attack, timeout, compileTimeout, sys.env)
    d.copy(runtimeAttack = runtimeAttack)
