package codes.quine.labo.redos
package fuzz

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

import FuzzChecker._
import backtrack.IR
import backtrack.Tracer.LimitTracer
import backtrack.VM
import data.ICharSet
import data.UString
import util.Timeout

/** ReDoS vulnerable RegExp checker based on fuzzing. */
object FuzzChecker {

  /** Checks whether RegExp is ReDoS vulnerable or not. */
  def check(
      ir: IR,
      alphabet: ICharSet,
      seed: Set[FString],
      random: Random = Random,
      populationLimit: Int = 100_000,
      attackLimit: Int = 1_000_000,
      crossSize: Int = 25,
      mutateSize: Int = 50,
      maxAttackSize: Int = 10_000,
      maxGenerationSize: Int = 100,
      maxIteration: Int = 30
  )(implicit timeout: Timeout = Timeout.NoTimeout): Option[FString] =
    new FuzzChecker(
      ir,
      alphabet,
      seed,
      random,
      populationLimit,
      attackLimit,
      crossSize,
      mutateSize,
      maxAttackSize,
      maxGenerationSize,
      maxIteration,
      timeout
    ).check()

  /** Trace is a summary of IR execution. */
  private[fuzz] final case class Trace(str: FString, rate: Double, steps: Int, coverage: Set[(Int, Seq[Int], Boolean)])

  /** Generation is an immutable generation. */
  private[fuzz] final case class Generation(
      minRate: Double,
      traces: IndexedSeq[Trace],
      inputs: Set[UString],
      covered: Set[(Int, Seq[Int], Boolean)]
  )
}

/** FuzzChecker is a ReDoS vulnerable RegExp checker based on fuzzing. */
private[fuzz] final class FuzzChecker(
    val ir: IR,
    val alphabet: ICharSet,
    val seed: Set[FString],
    val random: Random,
    val populationLimit: Int,
    val attackLimit: Int,
    val crossSize: Int,
    val mutateSize: Int,
    val maxAttackSize: Int,
    val maxGenerationSize: Int,
    val maxIteration: Int,
    implicit val timeout: Timeout
) {

  /** Runs this fuzzer. */
  def check(): Option[FString] = {
    var gen = init() match {
      case Right(attack) => return Some(attack)
      case Left(gen)     => gen
    }

    for (_ <- 1 to maxIteration; if gen.traces.nonEmpty) {
      iterate(gen) match {
        case Right(attack) => return Some(attack)
        case Left(next) =>
          gen = next
      }
    }

    None
  }

  /** Creates the initial generation from the seed set. */
  def init(): Either[Generation, FString] = {
    val pop = new Population(0.0, mutable.Set.empty, mutable.Set.empty, mutable.Set.empty, true)
    for (str <- seed) {
      pop.execute(str) match {
        case Some(attack) => return Right(attack)
        case None         => () // Skips
      }
    }
    Left(pop.toGeneration)
  }

  /** Iterates a generation. */
  def iterate(gen: Generation): Either[Generation, FString] = {
    val next = Population.from(gen)

    for (_ <- 1 to crossSize) {
      cross(gen, next) match {
        case Some(attack) => return Right(attack)
        case None         => () // Skips
      }
    }

    for (_ <- 1 to mutateSize) {
      mutate(gen, next) match {
        case Some(attack) => return Right(attack)
        case None         => () // Skips
      }
    }

    Left(next.toGeneration)
  }

  /** Simulates a crossing. */
  def cross(gen: Generation, next: Population): Option[FString] = {
    val i1 = random.between(0, gen.traces.size)
    val i2 = random.between(0, gen.traces.size)

    val t1 = gen.traces(i1).str
    val t2 = gen.traces(i2).str

    val pos1 = random.between(0, t1.size + 1)
    val pos2 = random.between(0, t2.size + 1)

    val (s1, s2) = FString.cross(t1, t2, pos1, pos2)
    for (s <- Seq(s1, s2)) {
      next.execute(s) match {
        case Some(attack) => return Some(attack)
        case None         => () // Skips
      }
    }

    None
  }

  /** Simulates a mutation. */
  def mutate(gen: Generation, next: Population): Option[FString] = {
    val i = random.nextInt(mutators.size)
    mutators(i)(gen, next)
  }

  /** Mutators list defined in this fuzzer. */
  val mutators: IndexedSeq[(Generation, Population) => Option[FString]] = IndexedSeq(
    mutateRepeat,
    mutateInsert,
    mutateUpdate,
    mutateCopy,
    mutateDelete
  )

  /** A mutator to update a base repeat number. */
  def mutateRepeat(gen: Generation, next: Population): Option[FString] = {
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
  def mutateInsert(gen: Generation, next: Population): Option[FString] = {
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
        FString.Repeat(m, None, size)
    }

    val pos = random.between(0, t.size + 1)
    val s = t.insert(pos, IndexedSeq(fc))
    next.execute(s)
  }

  /** A mutator to update a character or a repeat specifier. */
  def mutateUpdate(gen: Generation, next: Population): Option[FString] = {
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str
    if (t.isEmpty) return None

    val pos = random.between(0, t.size)
    val fc = t(pos) match {
      case FString.Wrap(_) =>
        val k = random.nextInt(alphabet.chars.size)
        val c = alphabet.chars(k).head
        FString.Wrap(c)
      case FString.Repeat(m0, max, size0) =>
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
        FString.Repeat(m, max, size)
    }

    val s = t.replace(pos, 1, IndexedSeq(fc))
    next.execute(s)
  }

  /** A mutator to copy a part of characters of a string. */
  def mutateCopy(gen: Generation, next: Population): Option[FString] = {
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
  def mutateDelete(gen: Generation, next: Population): Option[FString] = {
    val i = random.nextInt(gen.traces.size)
    val t = gen.traces(i).str

    if (t.size < 2) return None

    val pos = random.between(0, t.size)
    val size = random.between(1, t.size - pos + 1)
    val s = t.delete(pos, size)
    next.execute(s)
  }

  /** Tries to construct an attack string. */
  def tryAttack(str: FString): Option[FString] = {
    @tailrec
    def loop(str: FString): Option[FString] = {
      val input = str.toUString
      if (input.size > maxAttackSize || str.n > maxAttackSize) return None
      val t = new LimitTracer(attackLimit, timeout)
      try VM.execute(ir, input, 0, t)
      catch {
        case _: LimitException =>
          return Some(str)
      }
      loop(str.copy(n = str.n * 2))
    }

    loop(str)
  }

  /** Population is a mutable generation on fuzzing. */
  final class Population(
      var minRate: Double,
      val set: mutable.Set[Trace],
      val inputs: mutable.Set[UString],
      val visited: mutable.Set[(Int, Seq[Int], Boolean)],
      val init: Boolean
  ) {

    /** Executes the string and adds its result. */
    def execute(str: FString): Option[FString] = {
      val input = str.toUString
      if (inputs.contains(input)) return None

      val t = new FuzzTracer(ir, input, populationLimit, timeout)
      try VM.execute(ir, input, 0, t)
      catch {
        case _: LimitException =>
          add(t)
          return tryAttack(str)
      }
      add(t)
      None
    }

    /** Adds an IR execution result. */
    def add(t: FuzzTracer): Unit = {
      inputs.add(t.input)

      val str = t.buildFString()
      val rate = t.rate()
      val coverage = t.coverage()
      val trace = Trace(str, rate, t.steps, coverage)

      if (
        str.size < maxAttackSize && !set.contains(trace) && (init || rate >= minRate || !coverage.subsetOf(visited))
      ) {
        minRate = Math.min(rate, minRate)
        set.add(trace)
        visited.addAll(coverage)
      }
    }

    /** Converts this to [[Generation]]. */
    def toGeneration: Generation = {
      val traces = set.toIndexedSeq.sortBy(-_.steps).slice(0, maxGenerationSize)
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
