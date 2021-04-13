package codes.quine.labo.recheck
package fuzz

import scala.collection.mutable
import scala.util.Random

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.ICharSet
import codes.quine.labo.recheck.data.UString
import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.diagnostics.Hotspot
import codes.quine.labo.recheck.fuzz.FuzzChecker._
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
      fuzz: FuzzProgram,
      random: Random = Random,
      seedLimit: Int = 10_000,
      incubationLimit: Int = 100_000,
      attackLimit: Int = 1_000_000,
      crossSize: Int = 25,
      mutateSize: Int = 50,
      maxAttackSize: Int = 10_000,
      maxSeedSize: Int = 100,
      maxGenerationSize: Int = 100,
      maxIteration: Int = 30,
      maxDegree: Int = 4,
      heatRate: Double = 0.001,
      usesAcceleration: Boolean = true
  )(implicit ctx: Context): Option[(AttackComplexity.Vulnerable, AttackPattern, Hotspot)] =
    ctx.interrupt {
      new FuzzChecker(
        fuzz,
        random,
        seedLimit,
        incubationLimit,
        attackLimit,
        crossSize,
        mutateSize,
        maxAttackSize,
        maxSeedSize,
        maxGenerationSize,
        maxIteration,
        maxDegree,
        heatRate,
        usesAcceleration
      ).check()
    }

  /** Trace is a summary of IR execution. */
  private[fuzz] final case class Trace(str: FString, rate: Double, steps: Int, coverage: Set[CoverageItem])

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
    val fuzz: FuzzProgram,
    val random: Random,
    val seedLimit: Int,
    val incubationLimit: Int,
    val attackLimit: Int,
    val crossSize: Int,
    val mutateSize: Int,
    val maxAttackSize: Int,
    val maxSeedSize: Int,
    val maxGenerationSize: Int,
    val maxIteration: Int,
    val maxDegree: Int,
    val heatRate: Double,
    val usesAcceleration: Boolean
)(implicit val ctx: Context) {

  import ctx._

  /** An alias to `fuzz.program`. */
  def program: Program = fuzz.program

  /** An alias to `fuzz.alphabet`. */
  def alphabet: ICharSet = fuzz.alphabet

  /** A sequence of `fuzz.parts` */
  val parts: Seq[UString] = fuzz.parts.toSeq

  type AttackResult = (AttackComplexity.Vulnerable, AttackPattern, Hotspot)

  /** Runs this fuzzer. */
  def check(): Option[AttackResult] = interrupt {
    var gen = init() match {
      case Right(result) => return Some(result)
      case Left(gen)     => gen
    }

    for (_ <- 1 to maxIteration; if gen.traces.nonEmpty) {
      iterate(gen) match {
        case Right(result) => return Some(result)
        case Left(next)    => gen = next
      }
    }

    None
  }

  /** Creates the initial generation from the seed set. */
  def init(): Either[Generation, AttackResult] = interrupt {
    val seed: Set[FString] = Seeder.seed(fuzz, seedLimit, maxSeedSize)
    val pop = new Population(0.0, mutable.Set.empty, mutable.Set.empty, mutable.Set.empty, true)
    for (str <- seed) {
      pop.execute(str) match {
        case Some(result) => return Right(result)
        case None         => () // Skips
      }
    }
    Left(pop.toGeneration)
  }

  /** Iterates a generation. */
  def iterate(gen: Generation): Either[Generation, AttackResult] = interrupt {
    val next = Population.from(gen)

    val crossing = (1 to crossSize).iterator.flatMap(_ => cross(gen, next))
    val mutation = (1 to mutateSize).iterator.flatMap(_ => mutate(gen, next))

    (crossing ++ mutation).nextOption().fold(Left(next.toGeneration): Either[Generation, AttackResult])(Right(_))
  }

  /** Simulates a crossing. */
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
      case 1 => t.mapN(_ * 2)
    }
    next.execute(s)
  }

  /** A mutator to insert a character or a repeat specifier. */
  def mutateInsert(gen: Generation, next: Population): Option[AttackResult] = interrupt {
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str

    val fc = random.between(0, 2) match {
      case 0 =>
        val idx = random.between(0, alphabet.chars.size)
        val c = alphabet.chars(idx).head
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
    val part = parts(idx).seq.map(FString.Wrap)
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
        val k = random.nextInt(alphabet.chars.size)
        val c = alphabet.chars(k).head
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

    val pos1 = random.between(0, t.size)
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
    val r = Math.max(1, Math.log(attackLimit) / Math.log(2) / str.n)
    val attack = str.copy(n = Math.ceil(str.n * r).toInt)
    tryAttackExecute(attack).map { case (pattern, hotspot) => (AttackComplexity.Exponential(true), pattern, hotspot) }
  }

  /** Construct an attack string on assuming the pattern is polynomial. */
  def tryAttackPolynomial(str: FString, degree: Int): Option[AttackResult] = interrupt {
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
    if (input.size <= maxAttackSize) {
      val opts = Options(attackLimit, usesAcceleration = usesAcceleration, needsHeatmap = true)
      val result = Interpreter.run(program, input, 0, opts)
      if (result.status == Status.Limit) {
        return Some((str.toAttackPattern, Hotspot.build(result.heatmap, heatRate)))
      }
    }
    None
  }

  /** Population is a mutable generation on fuzzing. */
  final class Population(
      var minRate: Double,
      val set: mutable.Set[Trace],
      val inputs: mutable.Set[UString],
      val visited: mutable.Set[CoverageItem],
      val init: Boolean
  ) {

    /** Executes the string and adds its result. */
    def execute(str: FString): Option[AttackResult] = {
      val input = str.toUString
      if (inputs.contains(input)) return None

      val opts = Options(incubationLimit, usesAcceleration = usesAcceleration, needsCoverage = true)
      val result = Interpreter.run(program, input, 0, opts)
      add(str, input, result)
      if (result.status == Status.Limit) {
        return tryAttack(str)
      }

      None
    }

    /** Records an IR execution result. */
    def add(str: FString, input: UString, result: Result): Unit = {
      inputs.add(input)

      val rate = if (input.size == 0) 0 else result.steps.toDouble / input.size
      val coverage = result.coverage
      val trace = Trace(str, rate, result.steps, coverage)

      if (input.size < maxAttackSize && !set.contains(trace)) {
        if (init || rate >= minRate || !coverage.subsetOf(visited)) {
          minRate = Math.min(rate, minRate)
          set.add(trace)
          visited.addAll(coverage)
        }
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
        mutable.Set.from(gen.covered),
        false
      )
  }
}
