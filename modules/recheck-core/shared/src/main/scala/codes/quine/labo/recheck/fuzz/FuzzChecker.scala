package codes.quine.labo.recheck
package fuzz

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.Random

import codes.quine.labo.recheck.common.AccelerationMode
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.Parameters
import codes.quine.labo.recheck.common.Seeder
import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.diagnostics.Hotspot
import codes.quine.labo.recheck.fuzz.FuzzChecker._
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.unicode.ICharSet
import codes.quine.labo.recheck.unicode.UString
import codes.quine.labo.recheck.vm.Interpreter
import codes.quine.labo.recheck.vm.Interpreter.CoverageItem
import codes.quine.labo.recheck.vm.Interpreter.Options
import codes.quine.labo.recheck.vm.Interpreter.Result
import codes.quine.labo.recheck.vm.Interpreter.Status
import codes.quine.labo.recheck.vm.Program

/** ReDoS vulnerable RegExp checker based on fuzzing. */
object FuzzChecker {

  /** Checks whether RegExp is ReDoS vulnerable or not. */
  def check(
      pattern: Pattern,
      fuzz: FuzzProgram,
      random: Random = new Random(Parameters.RandomSeed),
      seeder: Seeder = Parameters.Seeder,
      maxSimpleRepeatCount: Int = Parameters.MaxSimpleRepeatCount,
      seedingLimit: Int = Parameters.SeedingLimit,
      seedingTimeout: Duration = Parameters.SeedingTimeout,
      incubationLimit: Int = Parameters.IncubationLimit,
      incubationTimeout: Duration = Parameters.IncubationTimeout,
      maxGeneStringSize: Int = Parameters.MaxGeneStringSize,
      attackLimit: Int = Parameters.AttackLimit,
      attackTimeout: Duration = Parameters.AttackTimeout,
      crossoverSize: Int = Parameters.CrossoverSize,
      mutationSize: Int = Parameters.MutationSize,
      maxAttackStringSize: Int = Parameters.MaxAttackStringSize,
      maxInitialGenerationSize: Int = Parameters.MaxInitialGenerationSize,
      maxGenerationSize: Int = Parameters.MaxGenerationSize,
      maxIteration: Int = Parameters.MaxIteration,
      maxDegree: Int = Parameters.MaxDegree,
      heatRatio: Double = Parameters.HeatRatio,
      accelerationMode: AccelerationMode = Parameters.AccelerationMode
  )(implicit ctx: Context): Option[(AttackComplexity.Vulnerable, AttackPattern, Hotspot)] =
    ctx.interrupt {
      new FuzzChecker(
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
      ).check()
    }

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
      inputs: Set[UString],
      covered: Set[CoverageItem]
  )
}

/** FuzzChecker is a ReDoS vulnerable RegExp checker based on fuzzing. */
private[fuzz] final class FuzzChecker(
    val pattern: Pattern,
    val fuzz: FuzzProgram,
    val random: Random,
    val seeder: Seeder,
    val maxSimpleRepeatCount: Int,
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
    val maxIteration: Int,
    val maxDegree: Int,
    val heatRatio: Double,
    val accelerationMode: AccelerationMode
)(implicit val ctx: Context) {

  import ctx._

  /** An alias to `fuzz.program`. */
  def program: Program = fuzz.program

  /** An alias to `fuzz.alphabet`. */
  def alphabet: ICharSet = fuzz.alphabet

  /** An alias to `fuzz.program.meta.unicode` */
  def unicode: Boolean = fuzz.program.meta.unicode

  /** A sequence of `fuzz.parts` */
  val parts: Seq[UString] = fuzz.parts.toSeq

  /** A type alias for result of attack. */
  type AttackResult = (AttackComplexity.Vulnerable, AttackPattern, Hotspot)

  /** Runs this fuzzer. */
  def check(): Option[AttackResult] = interrupt {
    log(s"fuzz: start (usesAcceleration: ${fuzz.usesAcceleration(accelerationMode)})")

    var gen = init() match {
      case Right(result) => return Some(result)
      case Left(gen)     => gen
    }

    for (i <- 1 to maxIteration) {
      iterate(i, gen) match {
        case Right(result) => return Some(result)
        case Left(next)    => gen = next
      }
    }

    None
  }

  /** Creates the initial generation from the seed set. */
  def init(): Either[Generation, AttackResult] = interrupt {
    log(s"fuzz: seeding start (seeder: $seeder)")
    val seed: Set[FString] = seeder match {
      case Seeder.Static =>
        val staticSeed = StaticSeeder.seed(pattern, maxSimpleRepeatCount, maxInitialGenerationSize, incubationLimit)
        if (staticSeed.size < maxInitialGenerationSize) {
          val dynamicSeed = DynamicSeeder.seed(
            fuzz,
            seedingLimit,
            seedingTimeout,
            maxInitialGenerationSize - staticSeed.size,
            accelerationMode
          )
          staticSeed ++ dynamicSeed
        } else staticSeed
      case Seeder.Dynamic =>
        DynamicSeeder.seed(fuzz, seedingLimit, seedingTimeout, maxInitialGenerationSize, accelerationMode)
    }
    log(s"""|fuzz: seeding finish
            |  size: ${seed.size}""".stripMargin)

    val pop = new Population(0.0, mutable.Set.empty, mutable.Set.empty, mutable.Set.empty)
    for (str <- seed) {
      pop.execute(str) match {
        case Some(result) => return Right(result)
        case None         => () // Skips
      }
    }

    Left(pop.toGeneration)
  }

  /** Iterates a generation. */
  def iterate(i: Int, gen: Generation): Either[Generation, AttackResult] = interrupt {
    if (gen.traces.isEmpty) return Left(gen)

    log {
      val max = gen.traces.maxBy(_.steps)
      s"""|fuzz: iteration $i
          |  traces: ${gen.traces.size}
          |     max: ${max.str} (steps: ${max.steps})""".stripMargin
    }
    val next = Population.from(gen)

    val crossovers = (1 to crossoverSize).iterator.flatMap(_ => cross(gen, next))
    val mutations = (1 to mutationSize).iterator.flatMap(_ => mutate(gen, next))

    (crossovers ++ mutations).nextOption().fold(Left(next.toGeneration): Either[Generation, AttackResult])(Right(_))
  }

  /** Simulates a crossover. */
  def cross(gen: Generation, next: Population): Option[AttackResult] = interrupt {
    val i1 = random.between(0, gen.traces.size)
    val i2 = random.between(0, gen.traces.size)

    val t1 = gen.traces(i1).str
    val t2 = gen.traces(i2).str

    val pos1 = random.between(0, t1.size + 1)
    val pos2 = random.between(0, t2.size + 1)

    val (s1, s2) = FString.cross(t1, t2, pos1, pos2)
    Seq(s1, s2).iterator.flatMap(next.execute).nextOption()
  }

  /** Simulates a mutation. */
  def mutate(gen: Generation, next: Population): Option[AttackResult] = interrupt {
    val i = random.nextInt(mutators.size)
    mutators(i)(gen, next)
  }

  /** Mutators list defined in this fuzzer. */
  val mutators: IndexedSeq[(Generation, Population) => Option[AttackResult]] = IndexedSeq(
    mutateRepeat,
    mutateInsert,
    mutateInsertPart,
    mutateUpdate,
    mutateCopy,
    mutateDelete
  )

  /** A mutator to update a base repeat number. */
  def mutateRepeat(gen: Generation, next: Population): Option[AttackResult] = interrupt {
    val i = random.between(0, gen.traces.size)
    val t = gen.traces(i).str
    if (t.isConstant) return None

    val s = random.between(0, 2) match {
      case 0 =>
        val d = random.between(-10, 11)
        t.mapN(_ + d)
      case 1 =>
        val d = random.between(2, 8)
        t.mapN(_ * d)
    }
    next.execute(s)
  }

  /** A mutator to insert a character or a repeat specifier. */
  def mutateInsert(gen: Generation, next: Population): Option[AttackResult] = interrupt {
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str

    val fc = random.between(0, 2) match {
      case 0 =>
        val idx = random.between(0, alphabet.pairs.size)
        val c = alphabet.pairs(idx)._1.head
        FString.Wrap(c)
      case 1 =>
        if (t.isEmpty) return None
        val m = random.between(0, 10)
        val size = random.between(0, t.size)
        FString.Repeat(m, size)
    }

    val pos = random.between(0, t.size + 1)
    val s = t.insertAt(pos, fc)
    next.execute(s)
  }

  /** A mutator to insert a part of the pattern (with/without a repeat specifier). */
  def mutateInsertPart(gen: Generation, next: Population): Option[AttackResult] = interrupt {
    // Falls back when there is no part in the pattern.
    if (parts.isEmpty) return mutateInsert(gen, next)

    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str

    val idx = random.between(0, parts.size)
    val part = parts(idx).iterator(unicode).map(FString.Wrap).toIndexedSeq
    val fcs = random.between(0, 2) match {
      case 0 => part
      case 1 =>
        val m = random.between(0, 10)
        IndexedSeq(FString.Repeat(m, part.size)) ++ part
    }

    val pos = random.between(0, t.size + 1)
    val s = t.insert(pos, fcs)
    next.execute(s)
  }

  /** A mutator to update a character or a repeat specifier. */
  def mutateUpdate(gen: Generation, next: Population): Option[AttackResult] = interrupt {
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str
    if (t.isEmpty) return None

    val pos = random.between(0, t.size)
    val fc = t(pos) match {
      case FString.Wrap(_) =>
        val k = random.nextInt(alphabet.pairs.size)
        val c = alphabet.pairs(k)._1.head
        FString.Wrap(c)
      case FString.Repeat(m0, size0) =>
        val m = random.between(0, 2) match {
          case 0 =>
            val d = random.between(-10, 11)
            m0 + d
          case 1 => m0 * 2
        }
        val size = random.between(0, 2) match {
          case 0 => random.between(1, t.size - pos + 1)
          case 1 => size0 + random.between(-10, 11)
        }
        FString.Repeat(m, size)
    }

    val s = t.replaceAt(pos, fc)
    next.execute(s)
  }

  /** A mutator to copy a part of characters of a string. */
  def mutateCopy(gen: Generation, next: Population): Option[AttackResult] = interrupt {
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str
    if (t.size < 2) return None

    val pos1 = random.between(0, t.size - 1)
    val size = random.between(1, t.size - pos1 + 1)
    val part = t.seq.slice(pos1, pos1 + size)

    val pos2 = random.between(0, t.size + 1)
    val s = t.insert(pos2, part)
    next.execute(s)
  }

  /** A mutator to delete a part of characters of a string. */
  def mutateDelete(gen: Generation, next: Population): Option[AttackResult] = interrupt {
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str

    if (t.size < 2) return None

    val pos = random.between(0, t.size)
    val size = random.between(1, t.size - pos + 1)
    val s = t.delete(pos, size)
    next.execute(s)
  }

  /** Construct an attack string. */
  def tryAttack(str: FString): Option[AttackResult] = interrupt {
    tryAttackExponential(str).orElse((maxDegree to 2 by -1).iterator.flatMap(tryAttackPolynomial(str, _)).nextOption())
  }

  /** Construct an attack string on assuming the pattern is exponential. */
  def tryAttackExponential(str: FString): Option[AttackResult] = interrupt {
    log("fuzz: attack (exponential)")
    val r = Math.max(1, Math.log(attackLimit) / Math.log(2) / str.n)
    val attack = str.copy(n = Math.ceil(str.n * r).toInt)
    tryAttackExecute(attack).map { case (pattern, hotspot) => (AttackComplexity.Exponential(true), pattern, hotspot) }
  }

  /** Construct an attack string on assuming the pattern is polynomial. */
  def tryAttackPolynomial(str: FString, degree: Int): Option[AttackResult] = interrupt {
    log(s"fuzz: attack (polynomial: $degree)")
    val r = Math.pow(attackLimit, 1.0 / degree) / str.n
    if (r < 1) None
    else {
      val attack = str.copy(n = Math.ceil(str.n * r).toInt)
      tryAttackExecute(attack).map { case (pattern, hotspot) =>
        (AttackComplexity.Polynomial(degree, true), pattern, hotspot)
      }
    }
  }

  /** Executes the string to construct attack string. */
  def tryAttackExecute(str: FString): Option[(AttackPattern, Hotspot)] = interrupt {
    val input = str.toUString
    if (input.sizeAsString <= maxAttackStringSize) {
      val opts =
        Options(attackLimit, usesAcceleration = fuzz.usesAcceleration(accelerationMode), needsHeatmap = true)
      val result = Interpreter.runWithTimeout(program, input, 0, opts, attackTimeout)
      if (result.status == Status.Limit || result.status == Status.Timeout) {
        log {
          s"""|fuzz: attack succeeded (status: ${result.status})
              |  string: ${str}""".stripMargin
        }
        return Some((str.toAttackPattern, Hotspot.build(result.heatmap, heatRatio)))
      }
    }
    None
  }

  /** Population is a mutable generation on fuzzing. */
  final class Population(
      var minRate: Double,
      val set: mutable.Set[Trace],
      val inputs: mutable.Set[UString],
      val visited: mutable.Set[CoverageItem]
  ) {

    /** Executes the string and adds its result. */
    def execute(str: FString): Option[AttackResult] = {
      if (str.countRepeat >= maxDegree) return None

      val input = str.toUString
      if (inputs.contains(input) || input.sizeAsString >= maxGeneStringSize) return None

      val opts =
        Options(incubationLimit, usesAcceleration = fuzz.usesAcceleration(accelerationMode), needsCoverage = true)
      val result = Interpreter.runWithTimeout(program, input, 0, opts, incubationTimeout)
      add(str, input, result)
      if (result.status == Status.Limit || result.status == Status.Timeout) {
        log {
          s"""|fuzz: attack start (status: ${result.status})
              |  string: $str""".stripMargin
        }
        return tryAttack(str)
      }

      None
    }

    /** Records the execution result. */
    def add(str: FString, input: UString, result: Result): Unit = {
      inputs.add(input)

      val rate = if (input.isEmpty) 0 else result.steps.toDouble / input.sizeAsString
      val coverage = result.coverage
      val trace = Trace(str, rate, result.steps, coverage)

      if (set.contains(trace)) return

      if (rate >= minRate || !coverage.subsetOf(visited)) {
        minRate = Math.min(rate, minRate)
        set.add(trace)
        visited.addAll(coverage)
      }
    }

    /** Converts this to [[Generation]]. */
    def toGeneration: Generation = {
      val traces = set.toIndexedSeq.sortBy(-_.rate).slice(0, maxGenerationSize)
      val newMinRate = traces.map(_.rate).minOption.getOrElse(0.0)
      val newInputs = traces.map(_.str.toUString).toSet
      val newCovered = traces.iterator.flatMap(_.coverage).toSet
      Generation(newMinRate, traces, newInputs, newCovered)
    }
  }

  /** Population utilities. */
  object Population {

    /** Creates a population from the generation. */
    def from(gen: Generation): Population =
      new Population(
        gen.minRate,
        mutable.Set.from(gen.traces),
        mutable.Set.from(gen.inputs),
        mutable.Set.from(gen.covered)
      )
  }
}
