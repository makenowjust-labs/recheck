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
import codes.quine.labs.recheck.fuzz.Attacker.AttackResult
import codes.quine.labs.recheck.fuzz.Attacker.Candidate
import codes.quine.labs.recheck.fuzz.FuzzChecker.*
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
        lazy val attacker = new Attacker(
          pattern,
          fuzz,
          incubationLimit,
          incubationTimeout,
          attackLimit,
          attackTimeout,
          maxAttackStringSize,
          maxDegree,
          heatRatio,
          accelerationMode,
          avoidsAttack,
          bound
        )
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
          crossoverSize,
          mutationSize,
          maxInitialGenerationSize,
          maxGenerationSize,
          maxAttackCandidates,
          maxIteration,
          maxDegree,
          accelerationMode,
          bound,
          attacker
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
    val crossoverSize: Int,
    val mutationSize: Int,
    val maxInitialGenerationSize: Int,
    val maxGenerationSize: Int,
    val maxAttackCandidates: Int,
    val maxIteration: Int,
    val maxDegree: Int,
    val accelerationMode: AccelerationMode,
    val upperBound: Option[Seq[Complexity.Vulnerable[UChar]]],
    val attacker: Attacker
)(using val ctx: Context):

  import ctx.*

  /** A set that holds strings once executed. */
  val inputs: mutable.Set[UString] = mutable.Set.empty

  /** An alias to `fuzz.program`. */
  def program: Program = fuzz.program

  /** An alias to `fuzz.alphabet`. */
  def alphabet: PatternAlphabet = fuzz.alphabet

  /** A sequence of `fuzz.parts` */
  val parts: Seq[UString] = fuzz.parts.toSeq

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
        attacker.attack(pop.candidates.valuesIterator)
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

    (attacker.attack(next.candidates.valuesIterator), next)

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

  /** Population is a mutable generation on fuzzing. */
  final class Population(
      var minRate: Double,
      val set: mutable.Set[Trace],
      val visited: mutable.Set[CoverageItem]
  ):

    /** The strings whose steps grow more than linearly, one for each fingerprint whose attacks have not failed. */
    val candidates: mutable.LinkedHashMap[FString.Fingerprint, Candidate] = mutable.LinkedHashMap.empty

    /** Executes the string and adds its result, and a candidate of an attack if its steps grow more than linearly. */
    def execute(raw: FString): Unit =
      if candidates.size >= maxAttackCandidates then return
      val str = raw.normalize
      if str.countRepeat >= maxDegree || !str.admitsAnyRepeatCount(pattern.mode) then return

      val input = str.toUString
      if input.length >= maxGeneStringSize then return

      var stops = false
      var reachesNew = false
      if !inputs.contains(input) then
        val usesAcceleration = fuzz.usesAcceleration(accelerationMode)
        val opts = Options(incubationLimit, usesAcceleration = usesAcceleration, needsCoverage = true)
        val result = Interpreter.runWithTimeout(program, input, opts, incubationTimeout)
        stops = result.status == Status.Limit || result.status == Status.Timeout
        reachesNew = !result.coverage.subsetOf(visited)
        add(str, input, result)

      if str.isConstant && !stops then
        if reachesNew && !str.isEmpty then execute(FString(1, FString.Repeat(0, str.size) +: str.seq))
      else attacker.candidateOf(str, stops).foreach(addCandidate)

    /** Adds the candidate, unless its fingerprint has one. */
    private def addCandidate(candidate: Candidate): Unit =
      val fingerprint = candidate.str.fingerprint
      if !candidates.contains(fingerprint) then
        log:
          s"""|fuzz: candidate (growth: ${candidate.measurement.growth.show})
              |  string: ${candidate.str}""".stripMargin
        candidates(fingerprint) = candidate

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
      val traces = set.toIndexedSeq.sortBy(t => (attacker.isAvoided(t.str), -t.rate)).slice(0, maxGenerationSize)
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
