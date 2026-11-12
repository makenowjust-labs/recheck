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
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.IRQuantifier
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

  /** Returns the scale of the pattern of the node: the largest width of what it matches when each repetition without
    * the largest count (e.g. `*` and `{2,}`) iterates the least times, or once for no times.
    *
    * It is the length of the characters that the pattern reads without looping, as far as the node tells. A
    * back-reference adds none.
    */
  private[fuzz] def scaleOf(node: IRNode): Int =
    def scale(node: IRNode): Long = node.data match
      case IRNodeData.Chars(_)           => 1
      case IRNodeData.Strings(set)       => set.strings.iterator.map(_.length.toLong).maxOption.getOrElse(0L).max(1)
      case IRNodeData.Disjunction(nodes) => nodes.map(scale).maxOption.getOrElse(0)
      case IRNodeData.Repeat(child, quantifier) =>
        val count = quantifier match
          case IRQuantifier.Exact(n)         => n
          case IRQuantifier.Unbounded(n, _)  => n.max(1)
          case IRQuantifier.Bounded(_, m, _) => m
        (count * scale(child)).min(Int.MaxValue / 4)
      case _ => node.children.map(scale).sum.min(Int.MaxValue / 4)

    scale(node).toInt

  /** Trace is a summary of IR execution. */
  private[fuzz] final case class Trace(
      str: FString,
      rate: Double,
      steps: Int,
      coverage: Set[CoverageItem]
  )

  /** Measurement is the growth of the steps of a string, measured on its repeat counts from `first` to `last`.
    *
    * `stops` tells that a run stopped on the limit or the timeout on the count after `last`, which the exponential
    * growth is for.
    */
  private[fuzz] final case class Measurement(growth: Growth, first: Int, last: Int, stops: Boolean)

  /** Candidate is a string whose steps grow more than linearly, with the measurement telling it. */
  private[fuzz] final case class Candidate(str: FString, measurement: Measurement)

  /** The degrees above `maxDegree` that a measurement tells from the exponential growth. */
  private[fuzz] val ExtraDegrees: Int = 3

  /** The largest scale of a pattern that the first measurement of a string tries the counts beyond (see
    * [[FuzzChecker.measureFirst]]).
    */
  private[fuzz] val SmallScale: Int = 256

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

  /** The measurements of the sequences of the strings on their first repeat counts (see [[measure]]). */
  val measurements: mutable.Map[IndexedSeq[FString.FChar], Option[Measurement]] = mutable.Map.empty

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

  /** Runs the attacks from the candidates, the steepest growth first. */
  def attack(candidates: Iterator[Candidate]): Iterator[AttackResult] = interrupt:
    def refineOrFail(candidate: Candidate): Option[Candidate] =
      val refined = Option.unless(failedSkeletons.contains(candidate.str.skeleton))(candidate).flatMap(refine)
      if refined.isEmpty then failedSkeletons.add(candidate.str.skeleton)
      refined
    // The candidates are sorted before any attack, with the growth that the larger limit tells.
    // The steepest one is raised first (see [[raise]]), and it stays after the raised one, whose attack may fail.
    Iterator
      .single(())
      .flatMap: _ =>
        val sorted = candidates.toVector.flatMap(refineOrFail).sortBy(-rank(_))
        (sorted.headOption.flatMap(raise) ++ sorted).iterator.flatMap(tryAttack)

  /** The rank of the growth of a candidate, which is the degree, or the largest for the exponential one. */
  private def rank(candidate: Candidate): Int = candidate.measurement.growth match
    case Growth.Exponential   => Int.MaxValue
    case p: Growth.Polynomial => p.degree

  /** The skeletons of the candidates that [[raise]] tried. */
  val raisedSkeletons: mutable.Set[UString] = mutable.Set.empty

  /** Raises the growth of the candidate by adding a character or a repetition to its string again and again, while the
    * growth gets steeper, and returns the steepest one, or `None` if none is steeper.
    *
    * A string from a seed or a mutation may grow less than the pattern allows, when a character ends the matching early
    * or a loop of the pattern has no part to repeat. The growth of each string tells whether an addition helps, as it
    * is exact. It stops on the largest degree to report.
    */
  def raise(candidate: Candidate): Option[Candidate] = interrupt:
    val target = maxTriedDegree
    if !raisedSkeletons.add(candidate.str.skeleton) then return None

    def measured(str: FString): Option[Candidate] =
      val admits = str.countRepeat < maxDegree && str.admitsAnyRepeatCount(pattern.mode)
      Option
        .when(admits)(str)
        .flatMap(str => measurements.getOrElseUpdate(str.seq, measureFirst(str, incubationLimit)))
        .filter(_.growth.isSuperLinear)
        .map(Candidate(str, _))
        // A run that stops tells nothing of the growth until it is measured with the larger limit.
        .flatMap(candidate =>
          if triesExponential then refine(candidate) else Option.unless(candidate.measurement.stops)(candidate)
        )

    // The string without the repetitions that the growth does not need, which may keep an addition from raising it
    // (e.g. `<` repeated in `<{n}0{n}` for `\s*<\w+[^>]+>`, where only the last `<` goes on).
    def reduced(candidate: Candidate): Candidate =
      val positions = candidate.str.seq.indices.filter(candidate.str.seq(_).isInstanceOf[FString.Repeat]).reverse
      positions.foldLeft(candidate): (current, pos) =>
        measured(current.str.delete(pos, 1)).filter(rank(_) >= rank(current)).getOrElse(current)

    var best = reduced(candidate)
    var continues = true
    while continues && rank(best) < target do
      // The first of the steepest ones, for the same result on each run.
      val next =
        additionsOf(best.str).iterator.flatMap(measured).foldLeft(best)((x, y) => if rank(y) > rank(x) then y else x)
      continues = next ne best
      best = if continues then reduced(next) else best
    Option.when(rank(best) > rank(candidate))(best)

  /** Returns the strings adding a character, or a repetition of a character, to the string at its start, its end and
    * the ends of its repetitions.
    */
  def additionsOf(str: FString): Seq[FString] =
    val chars = alphabet.pairs.map(_._1.representative).distinct
    val ends = str.seq.zipWithIndex.collect:
      case (FString.Repeat(_, size), pos) => Seq(pos, (pos + 1 + size).min(str.size))
    val positions = (0 +: ends.flatten :+ str.size).distinct.sorted
    for
      pos <- positions
      c <- chars
      part <- Seq(IndexedSeq(FString.Wrap(c)), IndexedSeq(FString.Repeat(0, 1), FString.Wrap(c)))
    yield str.copy(n = 1).insert(pos, part)

  /** The skeletons of the candidates whose attacks fail. */
  val failedSkeletons: mutable.Set[UString] = mutable.Set.empty

  /** The scale of the pattern (see [[FuzzChecker.scaleOf]]).
    *
    * The steps of a string follow one growth on the repeat counts whose repeated parts are longer than it, where no
    * repetition of the pattern counts them any more.
    */
  val scale: Int = scaleOf(pattern.node)

  /** The largest degree of the polynomial growth that a measurement tells from the exponential one. */
  val measuredDegree: Int = maxDegree + ExtraDegrees

  /** The step limit of the runs of the measurements on executing the strings. */
  val measurementLimit: Int = Math.min(attackLimit.toDouble, incubationLimit * Math.pow(2, maxDegree + 1)).toInt

  /** Returns the size of the input of the string on the repeat count. */
  def sizeOn(str: FString, count: Int): Long =
    val repeats = repeatsOf(str)
    val fixedSize = str.size - repeats.size - repeats.map(_._2).sum
    fixedSize + repeats.map((m, size) => (count.toLong + m).max(0) * size).sum

  /** Returns the steps of the string on the repeat count, or `None` if the run stops on the limit or the timeout. */
  def stepsOn(str: FString, count: Int, limit: Int, timeout: Duration): Option[Long] =
    val opts = Options(limit, usesAcceleration = fuzz.usesAcceleration(accelerationMode))
    val result = Interpreter.runWithTimeout(program, str.copy(n = count).toUString, opts, timeout)
    Option.unless(result.status == Status.Limit || result.status == Status.Timeout)(result.steps.toLong)

  /** Measures the growth of the steps of the string on the repeat counts from `from` up to `to`.
    *
    * It runs the string on the counts from `from`, as few as tell the growth (see [[Growth.of]]). While they tell none,
    * it tries the counts from the next one, on which the steps may follow the growth at last, and every second count,
    * on which the steps may be a polynomial when a loop of the pattern reads the repeated part twice.
    *
    * The growth is exponential if a run stops on the limit or the timeout.
    */
  def measure(
      str: FString,
      from: Int,
      to: Int,
      limit: Int,
      timeout: Duration,
      known: Map[Int, Option[Long]] = Map.empty
  ): Option[Measurement] = interrupt:
    val steps = mutable.Map.from(known)

    def measureOn(start: Int, stride: Int): Option[Measurement] =
      val window = IndexedSeq.newBuilder[Long]
      var count = start
      var size = 0
      while size < measuredDegree + 3 && count <= to do
        steps.getOrElseUpdate(count, stepsOn(str, count, limit, timeout)) match
          case None    => return Some(Measurement(Growth.Exponential, start, count - stride, true))
          case Some(s) =>
            window.addOne(s)
            size += 1
            Growth.of(start, stride, window.result(), measuredDegree) match
              case Some(growth) => return Some(Measurement(growth, start, count, false))
              case None         => count += stride
      None

    Iterator((from, 1), (from + 1, 1), (from, 2)).flatMap(measureOn(_, _)).nextOption()

  /** Measures the growth of the steps of the string on its first repeat counts, or on the first ones beyond the scale
    * of the pattern up to `SmallScale` if they tell none.
    */
  def measureFirst(str: FString, limit: Int): Option[Measurement] =
    measure(str, 1, Int.MaxValue, limit, incubationTimeout).orElse:
      Option
        .when(scale <= SmallScale)(countBeyond(str, scale))
        .flatMap(measure(str, _, Int.MaxValue, limit, incubationTimeout))

  /** Measures the growth of the steps of the string on the repeat counts up to the largest one whose run ends, from its
    * own.
    */
  def measureBelow(str: FString): Option[Measurement] = interrupt:
    var count = str.n
    while count > 1 && stepsOn(str, count, measurementLimit, incubationTimeout).isEmpty do count /= 2
    measure(str, (count - measuredDegree - 2).max(1), count, measurementLimit, incubationTimeout)

  /** Runs the attack from the candidate, whose skeleton is added to [[failedSkeletons]] if it fails. */
  def tryAttack(candidate: Candidate): Option[AttackResult] = interrupt:
    val str = candidate.str
    val skeleton = str.skeleton
    if failedSkeletons.contains(skeleton) then return None

    log(s"""|fuzz: attack (growth: ${show(candidate.measurement.growth)})
            |  string: $str""".stripMargin)
    val result =
      for
        measurement <- confirm(str, candidate.measurement)
        count <- attackCount(str, measurement.growth)
      yield
        val complexity = measurement.growth match
          case p: Growth.Polynomial                   => AttackComplexity.Polynomial(p.degree.min(maxTriedDegree), true)
          case Growth.Exponential if triesExponential => AttackComplexity.Exponential(true)
          case Growth.Exponential                     => AttackComplexity.Polynomial(maxTriedDegree, true)
        val attack = str.copy(n = count)
        log(s"""|fuzz: attack succeeded (growth: ${show(measurement.growth)})
                |  string: $attack""".stripMargin)
        (complexity, attack.toAttackPattern(pattern.mode), hotspotOf(str, measurement, count))
    result match
      case Some((complexity, _, hotspot)) => attacks(str) = (complexity, hotspot) :: attacks.getOrElse(str, Nil)
      case None                           => failedSkeletons.add(skeleton)
    result

  private def show(growth: Growth): String = growth match
    case Growth.Exponential   => "exponential"
    case p: Growth.Polynomial => s"polynomial of degree ${p.degree}"

  /** Measures the growth of a candidate again with the larger limit if a run stopped on measuring it, and returns the
    * candidate with it, or `None` if the steps do not grow more than linearly.
    */
  def refine(candidate: Candidate): Option[Candidate] =
    if !candidate.measurement.stops || candidate.str.isConstant then Some(candidate)
    else
      measureFirst(candidate.str, measurementLimit)
        .filter(_.growth.isSuperLinear)
        .map(measurement => candidate.copy(measurement = measurement))

  /** Confirms that the growth of the measurement lasts beyond the scale of the pattern, and returns the measurement
    * there, or `None` if the steps do not grow more than linearly there.
    *
    * A run on a count beyond the scale, and twice the last count of the measurement or more, tells it. The growth lasts
    * if the run stops on the limit or the timeout, or if its steps are the ones of the polynomial. Otherwise the growth
    * is measured again from the count, and confirmed on a count further once more.
    *
    * The exponential growth that the steps tell beyond the scale is confirmed without a run, and a string without
    * repetitions if its run stops.
    */
  def confirm(str: FString, measurement: Measurement, measuresAgain: Boolean = true): Option[Measurement] = interrupt:
    if str.isConstant then return Option.when(stepsOn(str, str.n, attackLimit, attackTimeout).isEmpty)(measurement)
    val isBeyond = measurement.first >= countBeyond(str, scale)
    if measurement.growth == Growth.Exponential && isBeyond && !measurement.stops then return Some(measurement)

    // The steps reach the limit on the count of the attack string, so the counts to confirm end on it.
    val end = attackCount(str, measurement.growth) match
      case Some(count) => count
      case None        => return None
    // The steps change the growth a little after the repeated parts get longer than the scale, as the fixed parts
    // and the other repeated parts are read with them.
    val least = countBeyond(str, 2 * scale)
    val count = measurement.growth match
      case p: Growth.Polynomial =>
        val k = (least.max(2 * measurement.last) - p.start + p.stride - 1) / p.stride
        (p.start + k * p.stride).min(end)
      case Growth.Exponential => least.max(measurement.last + 1).min(end)
    val steps = stepsOn(str, count, attackLimit, attackTimeout)
    val lasts = steps.forall: steps =>
      measurement.growth match
        case p: Growth.Polynomial => p.stepsOn(count) == steps
        case Growth.Exponential   => false
    if lasts then Some(measurement)
    else if !measuresAgain then None
    else
      measure(str, count, Int.MaxValue, attackLimit, attackTimeout, Map(count -> steps))
        .filter(_.growth.isSuperLinear)
        .flatMap(confirm(str, _, false))

  /** Returns the offset of the count and the size of the part of each repetition of the string.
    *
    * The part of the last repetition may be shorter than the size of its specifier.
    */
  def repeatsOf(str: FString): Seq[(Int, Int)] =
    str.seq.zipWithIndex.collect { case (FString.Repeat(m, size), pos) => (m, size.min(str.size - pos - 1)) }

  /** Returns the least repeat count on which every repetition of the string, which has some, is longer than `size`. */
  def countBeyond(str: FString, size: Int): Int =
    repeatsOf(str).map((m, repeatSize) => size / repeatSize + 1 - m).max.max(1)

  /** Returns the least repeat count on which the steps of the growth reach `attackLimit`, or the largest one within
    * `maxAttackStringSize` if they do not there. It is `None` if no count of the growth is within it.
    */
  def attackCount(str: FString, growth: Growth): Option[Int] =
    growth match
      case Growth.Exponential =>
        val repeatSize = repeatsOf(str).map(_._2).sum
        val fixedSize = (sizeOn(str, 0) min Int.MaxValue).toInt
        Some(RepeatUtil.exponential(attackLimit, fixedSize, repeatSize, maxAttackStringSize).max(1))
      case p: Growth.Polynomial =>
        def reaches(k: Int): Boolean = p.stepsOn(p.start + k * p.stride) >= attackLimit
        // The largest count whose string fits, where every part repeats.
        val repeats = repeatsOf(str)
        val growth = repeats.map(_._2).sum
        val largest =
          (maxAttackStringSize - sizeOn(str, 0) - repeats.map((m, size) => m.toLong.min(0) * size).sum) / growth
        if largest < p.start || sizeOn(str, largest.toInt) > maxAttackStringSize then return None
        // The steps of a polynomial growing more than linearly increase on the counts.
        var low = 0
        var high = ((largest - p.start) / p.stride).toInt
        while low < high do
          val middle = low + (high - low) / 2
          if reaches(middle) then high = middle else low = middle + 1
        Some(p.start + low * p.stride)

  /** Builds the hotspots of the attack string of the repeat count, from the heat of the runs on the repeat counts of
    * the measurement, which is scaled up to the attack string along its growth on each part.
    */
  def hotspotOf(str: FString, measurement: Measurement, count: Int): Hotspot = interrupt:
    // The runs do not memoize, with which the heat takes long.
    val opts = Options(attackLimit, needsHeatmap = true)
    def heatOn(count: Int): Map[SourceLocation, Int] =
      Interpreter.runWithTimeout(program, str.copy(n = count).toUString, opts, incubationTimeout).heatmap

    val heat: Map[SourceLocation, Double] = measurement.growth match
      case _ if str.isConstant || measurement.last < measurement.first =>
        heatOn(measurement.first.max(1)).map((loc, heat) => loc -> heat.toDouble)
      case p: Growth.Polynomial =>
        val heats = (0 to p.degree).map(k => heatOn(p.start + k * p.stride))
        heats
          .flatMap(_.keys)
          .distinct
          .map: loc =>
            val polynomial = Growth.Polynomial.through(p.start, p.stride, heats.map(heat => BigInt(heat(loc))))
            loc -> polynomial.stepsOn(count).toDouble
          .toMap
      case Growth.Exponential =>
        val last = heatOn(measurement.last)
        val previous = if measurement.last > measurement.first then heatOn(measurement.last - 1) else last
        last.map: (loc, heat) =>
          loc -> heat * Math.pow(heat.toDouble / previous(loc).max(1), (count - measurement.last).max(0).toDouble)
    Hotspot.build(heat, heatRatio)

  /** Population is a mutable generation on fuzzing. */
  final class Population(
      var minRate: Double,
      val set: mutable.Set[Trace],
      val visited: mutable.Set[CoverageItem]
  ):

    /** The strings whose steps grow more than linearly, one for each skeleton whose attacks have not failed. */
    val candidates: mutable.LinkedHashMap[UString, Candidate] = mutable.LinkedHashMap.empty

    /** Executes the string and adds its result, and a candidate of an attack if its steps grow more than linearly.
      *
      * The growth of a string with repetitions is measured once for its sequence, on its first repeat counts (see
      * [[measureFirst]]). A string of a few steps per character may grow to an attack on more repetitions, which its
      * rate does not tell. When the run of the string stops on the limit or the timeout without the growth, the growth
      * is measured on the counts below its own too (see [[measureBelow]]), where the steps may grow differently.
      *
      * A string without repetitions is a candidate if its run stops. If it reaches a new coverage item, it is also
      * executed as the one repeating it as a whole. The matching goes as far as the item from each of the starts that
      * the repetition makes.
      */
    def execute(str: FString): Unit =
      if candidates.size >= maxAttackCandidates then return
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

      if str.isConstant then
        if stops then addCandidate(str, Measurement(Growth.Exponential, str.n, str.n - 1, true))
        else if reachesNew && !str.isEmpty then execute(FString(1, FString.Repeat(0, str.size) +: str.seq))
      else
        measurements.getOrElseUpdate(str.seq, measureFirst(str, incubationLimit)) match
          case Some(measurement) if measurement.growth.isSuperLinear => addCandidate(str, measurement)
          case _ if stops && !failedSkeletons.contains(str.skeleton) =>
            measureBelow(str) match
              case Some(measurement) if measurement.growth.isSuperLinear => addCandidate(str, measurement)
              case _                                                     => failedSkeletons.add(str.skeleton)
          case _ => ()

    /** Adds the string as a candidate, unless its skeleton has one or its attacks failed. */
    private def addCandidate(str: FString, measurement: Measurement): Unit =
      val skeleton = str.skeleton
      if !failedSkeletons.contains(skeleton) && !candidates.contains(skeleton) then
        log:
          s"""|fuzz: candidate (growth: ${show(measurement.growth)})
              |  string: $str""".stripMargin
        candidates(skeleton) = Candidate(str, measurement)

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
