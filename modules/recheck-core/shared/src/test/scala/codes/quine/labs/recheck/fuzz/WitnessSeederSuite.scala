package codes.quine.labs.recheck
package fuzz

import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.automaton.Witness
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.fuzz.FString.Repeat
import codes.quine.labs.recheck.fuzz.FString.Wrap
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.dialect.Dialect

class WitnessSeederSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Computes the seeds of the complexities for the pattern with the default parameters. */
  def seed(source: String, complexities: Seq[Complexity.Vulnerable[UChar]]): Seq[FString] =
    import Parameters.*
    WitnessSeeder.seed(
      FuzzProgram.from(ReDoS.parse(source, "", Dialect.JavaScript)),
      complexities,
      DefaultIncubationLimit,
      DefaultIncubationTimeout,
      DefaultMaxGeneStringSize,
      DefaultMaxDegree,
      DefaultMaxInitialGenerationSize,
      DefaultAccelerationMode
    )

  /** Checks the upper bound of the pattern, which has some. */
  def upperBound(source: String): Seq[Complexity.Vulnerable[UChar]] =
    import Parameters.*
    val ir = ReDoS.parse(source, "", Dialect.JavaScript)
    FuzzChecker.upperBound(ir, DefaultMaxRepeatCount, DefaultMaxNFASize, 100).get

  test("WitnessSeeder.seed"):
    val witness = Witness[UChar](Seq((Seq.empty, Seq('a'))), Seq('!'))
    val quadratic = Complexity.Polynomial(2, witness)
    val cubic = Complexity.Polynomial(3, witness)
    def seeds(complexity: Complexity.Vulnerable[UChar]): Seq[FString] =
      WitnessSeeder.buildSeeds(
        complexity,
        Parameters.DefaultIncubationLimit,
        Parameters.DefaultMaxGeneStringSize,
        Parameters.DefaultMaxDegree
      )
    // The first seeds of the witnesses come before the others, without duplicates.
    assertEquals(
      seed("^a*$", Seq(cubic, quadratic, quadratic)),
      Seq(seeds(cubic).head, seeds(quadratic).head) ++ seeds(cubic).tail.filterNot(seeds(quadratic).contains)
    )
    assertEquals(seed("^a*$", Seq.empty), Seq.empty)
    // The seeds with other suffixes follow the ones of the witnesses.
    val suffixed = seed("^(?:a|a)*(?!.*xyz)", upperBound("^(?:a|a)*(?!.*xyz)"))
    assert(suffixed.exists(_.toUString.iterator.toSeq.endsWith("xyz".map(_.toInt))))

  test("WitnessSeeder.buildSeeds"):
    val witness = Witness[UChar](Seq((Seq('x'), Seq('a')), (Seq.empty, Seq('b', 'c', 'd'))), Seq('!'))
    val seq = Vector(Wrap('x'), Repeat(0, 1), Wrap('a'), Repeat(0, 3), Wrap('b'), Wrap('c'), Wrap('d'), Wrap('!'))
    val first = Vector(Wrap('x'), Repeat(0, 1), Wrap('a'))
    // The repeat count reaches the limit by the complexity, and then by each lower degree, for the whole witness and
    // then for the first pump only.
    assertEquals(
      WitnessSeeder.buildSeeds(Complexity.Polynomial(3, witness), 1000, 100, 4),
      Seq(FString(12, seq), FString(23, seq), FString(12, first), FString(23, first))
    )
    assertEquals(
      WitnessSeeder.buildSeeds(Complexity.Exponential(witness), 1000, 100, 3).take(3),
      Seq(FString(8, seq), FString(12, seq), FString(23, seq))
    )

  test("WitnessSeeder.suffixedSeed"):
    import Parameters.*
    def suffixedSeed(source: String): Seq[FString] =
      val fuzz = FuzzProgram.from(ReDoS.parse(source, "", Dialect.JavaScript))
      val seeds = upperBound(source).flatMap:
        WitnessSeeder.buildSeeds(_, DefaultIncubationLimit, DefaultMaxGeneStringSize, DefaultMaxDegree)
      WitnessSeeder.suffixedSeed(
        fuzz,
        seeds,
        DefaultIncubationLimit,
        DefaultIncubationTimeout,
        DefaultMaxInitialGenerationSize,
        DefaultAccelerationMode
      )

    // The match of the witness succeeds unless `xyz` follows, which makes the look-ahead fail.
    val suffixed = suffixedSeed("^(?:a|a)*(?!.*xyz)")
    assert(clue(suffixed).nonEmpty)
    assert(suffixed.forall(_.toUString.iterator.toSeq.endsWith("xyz".map(_.toInt))))
    // A seed whose match fails has no other suffix.
    assertEquals(suffixedSeed("^(?:a|a)*$"), Seq.empty)
