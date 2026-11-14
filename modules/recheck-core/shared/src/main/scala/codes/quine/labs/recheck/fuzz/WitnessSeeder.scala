package codes.quine.labs.recheck
package fuzz

import scala.concurrent.duration.Duration

import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.vm.Interpreter
import codes.quine.labs.recheck.vm.Interpreter.Options
import codes.quine.labs.recheck.vm.Interpreter.Status
import codes.quine.labs.resyntax.charset.UChar

/** WitnessSeeder computes an initial generation from the witnesses of the vulnerable complexities. */
object WitnessSeeder:

  /** Computes the seeds of the witnesses of the complexities, and the ones with other suffixes for the seeds whose
    * matches succeed (see [[suffixedSeed]]), up to `maxInitialGenerationSize`.
    */
  def seed(
      fuzz: FuzzProgram,
      complexities: Seq[Complexity.Vulnerable[UChar]],
      limit: Int,
      timeout: Duration,
      maxSize: Int,
      maxDegree: Int,
      maxInitialGenerationSize: Int,
      accelerationMode: AccelerationMode
  )(using ctx: Context): Seq[FString] = ctx.interrupt:
    // The first seeds of the witnesses come before the others, so that every witness has a seed.
    val seeds = complexities.map(buildSeeds(_, limit, maxSize, maxDegree))
    val witnessSeed = (seeds.map(_.head) ++ seeds.flatMap(_.tail)).distinct.take(maxInitialGenerationSize)
    witnessSeed ++ suffixedSeed(fuzz, witnessSeed, limit, timeout, maxInitialGenerationSize, accelerationMode)

  /** Builds seeds from the witness of the complexity, for the complexity and for each lower degree. */
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

  /** Computes the seeds whose matches succeed again, with each literal of the pattern as the suffix after the last
    * repetition, up to `maxInitialGenerationSize` with `seeds`.
    */
  private[fuzz] def suffixedSeed(
      fuzz: FuzzProgram,
      seeds: Seq[FString],
      limit: Int,
      timeout: Duration,
      maxInitialGenerationSize: Int,
      accelerationMode: AccelerationMode
  )(using ctx: Context): Seq[FString] = ctx.interrupt:
    val opts = Options(limit, usesAcceleration = fuzz.usesAcceleration(accelerationMode))
    val suffixes = fuzz.parts.toSeq.map(_.iterator.map(FString.Wrap(_)).toIndexedSeq)
    seeds
      .distinctBy(_.seq)
      .iterator
      .filter(seed => Interpreter.runWithTimeout(fuzz.program, seed.toUString, opts, timeout).status == Status.Ok)
      .flatMap: seed =>
        val end = seed.seq.zipWithIndex.collect { case (FString.Repeat(_, size), pos) => pos + 1 + size }.last
        suffixes.map(suffix => FString(seed.n, seed.seq.take(end) ++ suffix))
      .filterNot(seeds.contains)
      .take(maxInitialGenerationSize - seeds.size)
      .toSeq
