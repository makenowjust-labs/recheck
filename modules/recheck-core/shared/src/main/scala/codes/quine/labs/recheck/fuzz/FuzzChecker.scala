package codes.quine.labs.recheck
package fuzz

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.Random

import codes.quine.labs.recheck.automaton.AutomatonChecker
import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.Seeder
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.UpperBound
import codes.quine.labs.recheck.fuzz.FuzzChecker.*
import codes.quine.labs.recheck.util.RepeatUtil
import codes.quine.labs.recheck.vm.Interpreter
import codes.quine.labs.recheck.vm.Interpreter.CoverageItem
import codes.quine.labs.recheck.vm.Interpreter.Options
import codes.quine.labs.recheck.vm.Interpreter.Result
import codes.quine.labs.recheck.vm.Interpreter.Status
import codes.quine.labs.recheck.vm.Program
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** ReDoS vulnerable RegExp checker based on fuzzing. */
object FuzzChecker:

  /** Checks whether RegExp is ReDoS vulnerable or not, and returns the attacks it finds, the complexity of the safety
    * once they run out, and the upper bound, if any.
    *
    * It checks the upper bound first (see [[upperBound]]), and it fuzzes only if the bound is not linear, from the
    * witnesses of the bound and with no attack above it.
    *
    * `avoidsAttack` tells whether to avoid an attack of the complexity and the hotspot, which runtime-attack
    * verification refuted, and may change while iterating the results. Strings with an avoided attack rank below the
    * others in the next generation.
    */
  def check(
      pattern: IRPattern,
      fuzz: FuzzProgram,
      random: Random,
      seeder: Seeder = Parameters.DefaultSeeder,
      maxSimpleRepeatCount: Int = Parameters.DefaultMaxSimpleRepeatCount,
      maxRepeatCount: Int = Parameters.DefaultMaxRepeatCount,
      maxNFASize: Int = Parameters.DefaultMaxNFASize,
      seedingLimit: Int = Parameters.DefaultSeedingLimit,
      seedingTimeout: Duration = Parameters.DefaultSeedingTimeout,
      incubationLimit: Int,
      incubationTimeout: Duration = Parameters.DefaultIncubationTimeout,
      maxGeneStringSize: Int = Parameters.DefaultMaxGeneStringSize,
      attackLimit: Int,
      attackTimeout: Duration = Parameters.DefaultAttackTimeout,
      crossoverSize: Int = Parameters.DefaultCrossoverSize,
      mutationSize: Int = Parameters.DefaultMutationSize,
      maxAttackStringSize: Int = Parameters.DefaultMaxAttackStringSize,
      maxInitialGenerationSize: Int = Parameters.DefaultMaxInitialGenerationSize,
      maxGenerationSize: Int = Parameters.DefaultMaxGenerationSize,
      maxAttackCandidates: Int = Parameters.DefaultMaxAttackCandidates,
      maxIteration: Int = Parameters.DefaultMaxIteration,
      maxDegree: Int = Parameters.DefaultMaxDegree,
      heatRatio: Double = Parameters.DefaultHeatRatio,
      accelerationMode: AccelerationMode = Parameters.DefaultAccelerationMode,
      avoidsAttack: (AttackComplexity.Vulnerable, Hotspot) => Boolean
  )(using
      ctx: Context
  ): (Iterator[(AttackComplexity.Vulnerable, AttackPattern, Hotspot)], AttackComplexity.Safe, Option[UpperBound]) =
    ctx.interrupt:
      val bound = upperBound(pattern, maxRepeatCount, maxNFASize, maxInitialGenerationSize)
      // The first complexity is the worst.
      val reported = bound.map:
        _.headOption.fold(UpperBound(AttackComplexity.Linear, Hotspot.empty))(v =>
          UpperBound(v.toAttackComplexity, v.hotspot)
        )
      if bound.exists(_.isEmpty) then
        ctx.log("fuzz: linear upper bound")
        (Iterator.empty, AttackComplexity.Safe(false), reported)
      else
        lazy val checker = new FuzzChecker(
          pattern,
          fuzz,
          random,
          seeder,
          maxSimpleRepeatCount,
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
          avoidsAttack,
          bound
        )
        // The fuzzing starts on the first iteration, after the bound returns, so an error in it can report the bound.
        (Iterator(()).flatMap(_ => checker.check()), AttackComplexity.Safe(true), reported)

  /** Checks the upper bound of the complexity of the pattern by the automaton checker on its simplified automata (see
    * [[EpsNFABuilder.buildSimplified]] and [[IRPattern.approximateRepeats]]).
    *
    * It returns the vulnerable complexities of the bound from the worst, at most `maxInitialGenerationSize` but at
    * least one if any, which are empty if the bound is linear, or `None` if there is no bound.
    */
  private[fuzz] def upperBound(
      pattern: IRPattern,
      maxRepeatCount: Int,
      maxNFASize: Int,
      maxInitialGenerationSize: Int
  )(using ctx: Context): Option[Seq[Complexity.Vulnerable[UChar]]] =
    val approximated = pattern.approximateRepeats(maxRepeatCount)
    // The repetitions left may hide a large constant in a linear bound (e.g. the `2^50` paths of `(a?){50}`).
    if approximated.repeatCount >= maxRepeatCount then
      ctx.log("fuzz: no upper bound for too many repeats")
      None
    else
      try
        val simplified = EpsNFABuilder.buildSimplified(approximated, false, maxNFASize)
        Option.when(simplified.isUpperBound):
          val orderedNFA = simplified.epsNFA.restrictInputs(simplified.orderedNFA)
          // The look-ahead DFA of the upper bound has the single state.
          AutomatonChecker
            .check(orderedNFA.mapAlphabet(_.representative), true, maxNFASize, Int.MaxValue)
            .collect { case v: Complexity.Vulnerable[UChar] => v }
            .take(maxInitialGenerationSize.max(1))
            .toVector
      catch case _: UnsupportedException => None

  /** Builds seeds from a witness of the upper bound, for its complexity and for each lower degree. */
  private[fuzz] def buildSeeds(
      complexity: Complexity.Vulnerable[UChar],
      limit: Int,
      maxSize: Int,
      maxDegree: Int
  ): Seq[FString] =
    val witness = complexity.witness
    val pumps = witness.pumps.map: (prefix, pump) =>
      prefix.map(FString.Wrap(_)) ++ (FString.Repeat(0, pump.size) +: pump.map(FString.Wrap(_)))
    val seqs =
      (pumps.flatten ++ witness.suffix.map(FString.Wrap(_))) +: (pumps.size - 1 to 1 by -1).map(pumps.take(_).flatten)
    val lowerDegrees = complexity match
      case _: Complexity.Exponential[UChar]    => maxDegree to 2 by -1
      case Complexity.Polynomial(degree, _, _) => degree - 1 to 2 by -1
    val counts =
      complexity.bestRepeatCount(limit, maxSize) +:
        lowerDegrees.map(Complexity.Polynomial(_, witness).bestRepeatCount(limit, maxSize))
    val seeds = for seq <- seqs; n <- counts.map(_.max(1)).distinct yield FString(n, seq.toIndexedSeq)
    seeds.distinct

  /** Trace is a summary of IR execution. */
  private[fuzz] final case class Trace(
      str: FString,
      rate: Double,
      steps: Int,
      coverage: Set[CoverageItem]
  )

  /** Generation is an immutable generation. */
  private[fuzz] final case class Generation(
      minRate: Double,
      traces: IndexedSeq[Trace],
      covered: Set[CoverageItem]
  )

/** FuzzChecker is a ReDoS vulnerable RegExp checker based on fuzzing. */
private[fuzz] final class FuzzChecker(
    val pattern: IRPattern,
    val fuzz: FuzzProgram,
    val random: Random,
    val seeder: Seeder,
    val maxSimpleRepeatCount: Int,
    val maxNFASize: Int,
    val seedingLimit: Int,
    val seedingTimeout: Duration,
    val incubationLimit: Int,
    val incubationTimeout: Duration,
    val maxGeneStringSize: Int,
    val attackLimit: Int,
    val attackTimeout: Duration,
    val crossoverSize: Int,
    val mutationSize: Int,
    val maxAttackStringSize: Int,
    val maxInitialGenerationSize: Int,
    val maxGenerationSize: Int,
    val maxAttackCandidates: Int,
    val maxIteration: Int,
    val maxDegree: Int,
    val heatRatio: Double,
    val accelerationMode: AccelerationMode,
    val avoidsAttack: (AttackComplexity.Vulnerable, Hotspot) => Boolean,
    val upperBound: Option[Seq[Complexity.Vulnerable[UChar]]]
)(using val ctx: Context):

  import ctx.*

  /** Whether to try an exponential attack, which the upper bound may rule out. */
  val triesExponential: Boolean = upperBound.forall(_.exists(_.isInstanceOf[Complexity.Exponential[?]]))

  /** The maximum degree of the polynomial attacks to try, which the upper bound may lower. */
  val maxTriedDegree: Int = upperBound match
    case Some(vulnerable) if !triesExponential =>
      vulnerable.collect { case p: Complexity.Polynomial[?] => p.degree }.max.min(maxDegree)
    case _ => maxDegree

  /** A set that holds strings once executed. */
  val inputs: mutable.Set[UString] = mutable.Set.empty

  /** The complexities and the hotspots of the attacks from each string. */
  val attacks: mutable.Map[FString, List[(AttackComplexity.Vulnerable, Hotspot)]] = mutable.Map.empty

  /** Whether an attack from the string is avoided. */
  def isAvoided(str: FString): Boolean = attacks.get(str).exists(_.exists(avoidsAttack.tupled))

  /** An alias to `fuzz.program`. */
  def program: Program = fuzz.program

  /** An alias to `fuzz.alphabet`. */
  def alphabet: PatternAlphabet = fuzz.alphabet

  /** A sequence of `fuzz.parts` */
  val parts: Seq[UString] = fuzz.parts.toSeq

  /** A type alias for result of attack. */
  type AttackResult = (AttackComplexity.Vulnerable, AttackPattern, Hotspot)

  /** Runs this fuzzer. */
  def check(): Iterator[AttackResult] = interrupt:
    log(s"fuzz: start (usesAcceleration: ${fuzz.usesAcceleration(accelerationMode)})")

    val (initResults, initPop) = init()
    var pop = initPop
    initResults ++ (1 to maxIteration).iterator.flatMap: i =>
      // `Population` is mutable and `Iterator` is lazy, so `pop.toGeneration` runs after the previous iteration.
      val (iterResults, iterPop) = iterate(i, pop.toGeneration)
      pop = iterPop
      iterResults

  /** Creates the initial generation from the seed set. */
  def init(): (Iterator[AttackResult], Population) = interrupt:
    val pop = new Population(0.0, mutable.Set.empty, mutable.Set.empty)
    val results = Iterator
      .single(pop)
      .flatMap: pop =>
        seed().foreach(pop.execute)
        attack(pop.candidates.valuesIterator)
    (results, pop)

  /** Computes the seed set, where the witnesses of the upper bound come first. */
  def seed(): Seq[FString] = interrupt:
    log(s"fuzz: seeding start (seeder: $seeder)")
    // The first seeds of the witnesses come before the others, so that every witness has a seed.
    val boundSeeds = upperBound.toSeq.flatten.map(buildSeeds(_, incubationLimit, maxGeneStringSize, maxDegree))
    val upperBoundSeed = (boundSeeds.map(_.head) ++ boundSeeds.flatMap(_.tail)).distinct
      .take(maxInitialGenerationSize)
    val seederSeed: Set[FString] = seeder match
      case Seeder.Static =>
        val staticSeed =
          StaticSeeder.seed(
            pattern,
            maxSimpleRepeatCount,
            maxNFASize,
            maxInitialGenerationSize,
            incubationLimit,
            maxGeneStringSize
          )
        if staticSeed.size < maxInitialGenerationSize then
          val dynamicSeed = DynamicSeeder.seed(
            fuzz,
            seedingLimit,
            seedingTimeout,
            maxInitialGenerationSize - staticSeed.size,
            accelerationMode
          )
          staticSeed ++ dynamicSeed
        else staticSeed
      case Seeder.Dynamic =>
        DynamicSeeder.seed(fuzz, seedingLimit, seedingTimeout, maxInitialGenerationSize, accelerationMode)
    val seed = upperBoundSeed ++ (seederSeed -- upperBoundSeed).take(maxInitialGenerationSize - upperBoundSeed.size)

    log(s"""|fuzz: seeding finish
            |  size: ${seed.size}""".stripMargin)
    seed

  /** Iterates a generation. */
  def iterate(i: Int, gen: Generation): (Iterator[AttackResult], Population) = interrupt:
    val next = Population.from(gen)
    if gen.traces.isEmpty then return (Iterator.empty, next)

    log:
      val max = gen.traces.maxBy(_.rate)
      s"""|fuzz: iteration $i
          |  traces: ${gen.traces.size}
          |     max: ${max.str} (steps: ${max.steps}, rate: ${max.rate})""".stripMargin

    for _ <- 1 to crossoverSize do cross(gen, next)
    for _ <- 1 to mutationSize do mutate(gen, next)

    (attack(next.candidates.valuesIterator), next)

  /** Simulates a crossover. */
  def cross(gen: Generation, next: Population): Unit = interrupt:
    val i1 = random.between(0, gen.traces.size)
    val i2 = random.between(0, gen.traces.size)

    val t1 = gen.traces(i1).str
    val t2 = gen.traces(i2).str

    val pos1 = random.between(0, t1.size + 1)
    val pos2 = random.between(0, t2.size + 1)

    val (s1, s2) = FString.cross(t1, t2, pos1, pos2)
    next.execute(s1)
    next.execute(s2)

  /** Simulates a mutation. */
  def mutate(gen: Generation, next: Population): Unit = interrupt:
    val i = random.nextInt(mutators.size)
    mutators(i)(gen, next)

  /** Mutators list defined in this fuzzer. */
  val mutators: IndexedSeq[(Generation, Population) => Unit] = IndexedSeq(
    mutateRepeat,
    mutateInsert,
    mutateInsertPart,
    mutateUpdate,
    mutateCopy,
    mutateDelete
  )

  /** A mutator to update a base repeat number. */
  def mutateRepeat(gen: Generation, next: Population): Unit = interrupt:
    val i = random.between(0, gen.traces.size)
    val t = gen.traces(i).str
    if t.isConstant then return

    val s = random.between(0, 2) match
      case 0 =>
        val d = random.between(-10, 11)
        t.mapN(_ + d)
      case 1 =>
        val d = random.between(2, 8)
        t.mapN(_ * d)
    next.execute(s)

  /** A mutator to insert a character or a repeat specifier. */
  def mutateInsert(gen: Generation, next: Population): Unit = interrupt:
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str

    val fc = random.between(0, 2) match
      case 0 =>
        val idx = random.between(0, alphabet.pairs.size)
        val c = alphabet.pairs(idx)._1.representative
        FString.Wrap(c)
      case 1 =>
        if t.isEmpty then return
        val m = random.between(0, 10)
        val size = random.between(0, t.size)
        FString.Repeat(m, size)

    val pos = random.between(0, t.size + 1)
    val s = t.insertAt(pos, fc)
    next.execute(s)

  /** A mutator to insert a part of the pattern (with/without a repeat specifier). */
  def mutateInsertPart(gen: Generation, next: Population): Unit = interrupt:
    // Falls back when there is no part in the pattern.
    if parts.isEmpty then return mutateInsert(gen, next)

    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str

    val idx = random.between(0, parts.size)
    val part = parts(idx).iterator.map(FString.Wrap(_)).toIndexedSeq
    val fcs = random.between(0, 2) match
      case 0 => part
      case 1 =>
        val m = random.between(0, 10)
        IndexedSeq(FString.Repeat(m, part.size)) ++ part

    val pos = random.between(0, t.size + 1)
    val s = t.insert(pos, fcs)
    next.execute(s)

  /** A mutator to update a character or a repeat specifier. */
  def mutateUpdate(gen: Generation, next: Population): Unit = interrupt:
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str
    if t.isEmpty then return

    val pos = random.between(0, t.size)
    val fc = t(pos) match
      case FString.Wrap(_) =>
        val k = random.nextInt(alphabet.pairs.size)
        val c = alphabet.pairs(k)._1.representative
        FString.Wrap(c)
      case FString.Repeat(m0, size0) =>
        val m = random.between(0, 2) match
          case 0 =>
            val d = random.between(-10, 11)
            m0 + d
          case 1 => m0 * 2
        val size = random.between(0, 2) match
          case 0 => random.between(1, t.size - pos + 1)
          case 1 => size0 + random.between(-10, 11)
        FString.Repeat(m, size)

    val s = t.replaceAt(pos, fc)
    next.execute(s)

  /** A mutator to copy a part of characters of a string. */
  def mutateCopy(gen: Generation, next: Population): Unit = interrupt:
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str
    if t.size < 2 then return

    val pos1 = random.between(0, t.size - 1)
    val size = random.between(1, t.size - pos1 + 1)
    val part = t.seq.slice(pos1, pos1 + size)

    val pos2 = random.between(0, t.size + 1)
    val s = t.insert(pos2, part)
    next.execute(s)

  /** A mutator to delete a part of characters of a string. */
  def mutateDelete(gen: Generation, next: Population): Unit = interrupt:
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str

    if t.size < 2 then return

    val pos = random.between(0, t.size)
    val size = random.between(1, t.size - pos + 1)
    val s = t.delete(pos, size)
    next.execute(s)

  /** Runs the attacks from the candidates, the steepest estimate first. */
  def attack(candidates: Iterator[FString]): Iterator[AttackResult] = interrupt:
    val failed = mutable.ArrayBuffer.empty[FString]
    attackInOrder(candidates, failed) ++ failed.iterator.flatMap(tryAttackPolynomialOnce(_, maxTriedDegree))

  /** The skeletons of the candidates whose attacks fail or whose steps grow less than quadratically. */
  val failedSkeletons: mutable.Set[UString] = mutable.Set.empty

  /** Runs the attacks of [[attack]], adding the candidates whose exponential attacks fail to `failed`. */
  def attackInOrder(candidates: Iterator[FString], failed: mutable.Buffer[FString]): Iterator[AttackResult] =
    interrupt:
      val estimates = mutable.ArrayBuffer.empty[(FString, Double)]
      while candidates.hasNext && !estimates.exists((_, slope) => isSteepest(slope)) do
        val str = candidates.next()
        if !failedSkeletons.contains(str.skeleton) then
          estimate(str) match
            case Some(slope) => estimates.addOne((str, slope))
            case None        => failedSkeletons.add(str.skeleton)
      if estimates.isEmpty then Iterator.empty
      else
        val results = estimates
          .sortBy(-_._2)
          .iterator
          .filterNot((str, _) => failedSkeletons.contains(str.skeleton))
          .flatMap: (str, slope) =>
            degreeOf(slope) match
              case None         => tryAttackExponentialOnce(str, failed)
              case Some(degree) => tryAttackPolynomialOnce(str, degree)
        results ++ attackInOrder(candidates, failed)

  /** Runs the exponential attack from the candidate, and adds it to `failed` if it fails. */
  def tryAttackExponentialOnce(str: FString, failed: mutable.Buffer[FString]): Iterator[AttackResult] =
    val result = tryAttackExponential(str)
    if result.isEmpty then failed.addOne(str)
    record(str, result)

  /** Runs the polynomial attack from the candidate, and adds its skeleton to [[failedSkeletons]] if it fails. */
  def tryAttackPolynomialOnce(str: FString, degree: Int): Iterator[AttackResult] =
    val result = tryAttackPolynomial(str, degree)
    if result.isEmpty then failedSkeletons.add(str.skeleton)
    record(str, result)

  /** The step limit of the runs on estimation. */
  val estimationLimit: Int = Math.min(attackLimit.toDouble, incubationLimit * Math.pow(2, maxDegree + 1)).toInt

  /** Estimates the growth of the steps of the string as their log-log slope, or `None` if it is less than quadratic. */
  def estimate(str: FString): Option[Double] = interrupt:
    if str.isConstant then return Some(Double.PositiveInfinity)

    val opts = Options(estimationLimit, usesAcceleration = fuzz.usesAcceleration(accelerationMode))
    // The steps on the repeat count, and whether the run stops before the match ends.
    def run(n: Int): (Double, Boolean) =
      val result = Interpreter.runWithTimeout(program, str.copy(n = n).toUString, opts, incubationTimeout)
      (result.steps.toDouble, result.status == Status.Limit || result.status == Status.Timeout)
    def repeatedSize(n: Int): Double = str.seq.collect { case FString.Repeat(m, size) => (n + m).max(0) * size }.sum
    def slope(n1: Int, steps1: Double, n2: Int, steps2: Double): Double =
      Math.log(steps2 / steps1) / Math.log(repeatedSize(n2) / repeatedSize(n1))

    var n = str.n
    var (s, stops) = run(n)
    var above = Option.empty[(Int, Double)]
    while stops && n > 1 do
      above = Some((n, s))
      n /= 2
      val (s1, stops1) = run(n)
      s = s1
      stops = stops1
    if stops then return Option.when(s >= estimationLimit)(Double.PositiveInfinity)
    if repeatedSize(n) == 0 || s == 0 then return None

    // The slope above the count, and whether it is exact, which it is not on a lower bound of the steps.
    val upper = above match
      case Some((n2, s2))                                                     => Some((slope(n, s, n2, s2), false))
      case None if str.fixedSize + repeatedSize(2 * n) <= maxAttackStringSize =>
        val (s2, stops2) = run(2 * n)
        Some((slope(n, s, 2 * n, s2), !stops2))
      case None => None
    val lower =
      if repeatedSize(n / 2) == 0 then None
      else
        val (s0, stops0) = run(n / 2)
        Option.unless(stops0 || s0 == 0)(slope(n / 2, s0, n, s))
    val estimate = upper match
      case Some((slope, true)) if slope < 1.5 => Some(slope)
      case _                                  => (upper.map(_._1) ++ lower).maxOption
    log(s"""|fuzz: estimate (slope: ${estimate.getOrElse("none")})
            |  string: $str""".stripMargin)
    estimate.filter(_ >= 1.5)

  /** The degree of the polynomial attack for the slope, or `None` for the exponential one. */
  def degreeOf(slope: Double): Option[Int] =
    if triesExponential && slope > maxDegree + 0.5 then None
    else Some(Math.round(slope.min(maxTriedDegree.toDouble)).toInt.max(2))

  /** Whether no attack can be steeper than the one of the slope. */
  def isSteepest(slope: Double): Boolean =
    degreeOf(slope) == (if triesExponential then None else Some(maxTriedDegree))

  /** Records the attack from the string. */
  def record(str: FString, result: Option[AttackResult]): Iterator[AttackResult] =
    result.iterator.map: r =>
      val (complexity, _, hotspot) = r
      attacks(str) = (complexity, hotspot) :: attacks.getOrElse(str, Nil)
      r

  /** Constructs an attack string on assuming the pattern is exponential. */
  def tryAttackExponential(str: FString): Option[AttackResult] = interrupt:
    log("fuzz: attack (exponential)")
    val n = RepeatUtil.exponential(attackLimit, str.fixedSize, str.repeatSize, maxAttackStringSize)
    val attack = str.copy(n = n)
    tryAttackExecute(attack).map:
      case (pattern, hotspot) => (AttackComplexity.Exponential(true), pattern, hotspot)

  /** Constructs an attack string on assuming the pattern is polynomial. */
  def tryAttackPolynomial(str: FString, degree: Int): Option[AttackResult] = interrupt:
    log(s"fuzz: attack (polynomial: $degree)")
    val n = RepeatUtil.polynomial(degree, attackLimit, str.fixedSize, str.repeatSize, maxAttackStringSize)
    val attack = str.copy(n = n)
    tryAttackExecute(attack).map:
      case (pattern, hotspot) => (AttackComplexity.Polynomial(degree, true), pattern, hotspot)

  /** Executes the string to construct attack string. */
  def tryAttackExecute(str: FString): Option[(AttackPattern, Hotspot)] = interrupt:
    val input = str.toUString
    if input.length <= maxAttackStringSize then
      val usesAcceleration = fuzz.usesAcceleration(accelerationMode)
      val opts = Options(attackLimit, usesAcceleration = usesAcceleration, needsHeatmap = true)
      val result = Interpreter.runWithTimeout(program, input, opts, attackTimeout)
      if result.status == Status.Limit || result.status == Status.Timeout then
        log:
          s"""|fuzz: attack succeeded (status: ${result.status})
              |  string: ${str}""".stripMargin
        return Some((str.toAttackPattern(pattern.mode), Hotspot.build(result.heatmap, heatRatio)))
    None

  /** Population is a mutable generation on fuzzing. */
  final class Population(
      var minRate: Double,
      val set: mutable.Set[Trace],
      val visited: mutable.Set[CoverageItem]
  ):

    /** The strings that reach `incubationLimit`, one for each sequence. */
    val candidates: mutable.LinkedHashMap[IndexedSeq[FString.FChar], FString] = mutable.LinkedHashMap.empty

    /** Executes the string and adds its result, and a candidate of an attack if it reaches `incubationLimit`. */
    def execute(str: FString): Unit =
      if candidates.size >= maxAttackCandidates then return
      if str.countRepeat >= maxDegree || !str.admitsAnyRepeatCount(pattern.mode) then return

      val input = str.toUString
      if inputs.contains(input) || input.length >= maxGeneStringSize then return

      val opts =
        Options(incubationLimit, usesAcceleration = fuzz.usesAcceleration(accelerationMode), needsCoverage = true)
      val result = Interpreter.runWithTimeout(program, input, opts, incubationTimeout)
      add(str, input, result)
      if result.status == Status.Limit || result.status == Status.Timeout then
        log:
          s"""|fuzz: candidate (status: ${result.status})
              |  string: $str""".stripMargin
        if !candidates.contains(str.seq) then candidates(str.seq) = str

    /** Records the execution result. */
    def add(str: FString, input: UString, result: Result): Unit =
      inputs.add(input)

      val rate = if input.isEmpty then 0 else result.steps.toDouble / input.length
      val coverage = result.coverage
      val trace = Trace(str, rate, result.steps, coverage)

      if set.contains(trace) then return

      if rate >= minRate || !coverage.subsetOf(visited) then
        minRate = Math.min(rate, minRate)
        set.add(trace)
        visited.addAll(coverage)

    /** Converts this to [[Generation]], in which strings with an avoided attack rank last. */
    def toGeneration: Generation =
      val traces = set.toIndexedSeq.sortBy(t => (isAvoided(t.str), -t.rate)).slice(0, maxGenerationSize)
      val newMinRate = traces.map(_.rate).minOption.getOrElse(0.0)
      val newCovered = traces.iterator.flatMap(_.coverage).toSet
      Generation(newMinRate, traces, newCovered)

  /** Population utilities. */
  object Population:

    /** Creates a population from the generation. */
    def from(gen: Generation): Population =
      new Population(
        gen.minRate,
        mutable.Set.from(gen.traces),
        mutable.Set.from(gen.covered)
      )
