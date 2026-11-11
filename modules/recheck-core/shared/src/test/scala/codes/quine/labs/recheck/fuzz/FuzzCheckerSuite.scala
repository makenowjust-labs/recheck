package codes.quine.labs.recheck
package fuzz

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.Random
import scala.util.Try

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.fuzz.FuzzChecker.Trace
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRPattern

class FuzzCheckerSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** A fixed seed random instance for deterministic test. */
  def random0: Random = new Random(0)

  /** Tests whether the pattern is vulnerable. */
  def check(
      source: String,
      flags: String,
      quick: Boolean = false,
      maxIteration: Int = Parameters.DefaultMaxIteration,
      attackTimeout: Duration = Parameters.DefaultAttackTimeout
  ): Boolean =
    val result = for
      ir <- Try(ReDoS.parse(source, flags, Dialect.JavaScript))
      fuzz = FuzzProgram.from(ir)
    yield FuzzChecker
      .check(
        ir,
        fuzz,
        random = random0,
        maxGeneStringSize = if quick then 400 else 4000,
        maxAttackStringSize = if quick then 400 else Parameters.DefaultMaxAttackStringSize,
        seedingLimit = if quick then 1_00 else 1_000,
        incubationLimit = if quick then 1_000 else 10_000,
        attackLimit = if quick then 10_000 else Parameters.DefaultAttackLimit,
        attackTimeout = attackTimeout,
        maxIteration = maxIteration,
        avoidsAttack = (_, _) => false
      )
      .nextOption()
    result.get.isDefined

  test("FuzzChecker.check: constant"):
    assert(!check("^foo$", ""))
    assert(!check("^(foo|bar)$", ""))
    assert(!check("^(fiz{2}|buz{2){1,2}$", ""))

  test("FuzzChecker.check: linear"):
    assert(!check("(a|a)*", ""))
    assert(!check("(a*)*", ""))
    assert(!check("^(a\\1)$", ""))

    // Linear, but with so many steps per character that its attack string runs over `attackTimeout` (#1865).
    // The exponential attack string is not measured first, so it needs a timeout a slow CI runner cannot exceed.
    assert(!check("(?:a{9}b|a{9}c|a{9}d|a{9}e|a{9}f|a{9}g)", "", maxIteration = 0, attackTimeout = 10.seconds))

  test("FuzzChecker.check: polynomial"):
    assert(check("\\s*$", "", quick = true))
    assert(check("^a*aa*$", "", quick = true))
    assert(check("^((?:a|b)*)\\1$", "", quick = true))

  test("FuzzChecker.check: exponential"):
    assert(check("^(a|a)*$", "", quick = true))
    assert(check("^(a*)*$", "", quick = true))
    assert(check("^(a|b|ab)*$", "", quick = true))
    assert(check("^(a|B|Ab)*$", "i", quick = true))
    assert(check("^(aa|b|aab)*$", "", quick = true))

    assert(check("^(a?){50}a{50}$", "", quick = true))

    // The checker can find an attack string on seeding phase.
    assert:
      val result = for
        ir <- Try(ReDoS.parse("^(a?){50}a{50}$", "", Dialect.JavaScript))
        fuzz = FuzzProgram.from(ir)
      yield FuzzChecker
        .check(
          ir,
          fuzz,
          random0,
          seedingLimit = 10000,
          incubationLimit = 10000,
          attackLimit = 10000,
          avoidsAttack = (_, _) => false
        )
        .nextOption()
      result.get.isDefined

    // The checker cannot find too small attack string
    assert:
      val result = for
        ir <- Try(ReDoS.parse("^(a|a)*$", "", Dialect.JavaScript))
        fuzz = FuzzProgram.from(ir)
      yield FuzzChecker
        .check(
          ir,
          fuzz,
          random0,
          incubationLimit = 100,
          attackLimit = 100,
          maxGeneStringSize = 5,
          maxAttackStringSize = 5,
          avoidsAttack = (_, _) => false
        )
        .nextOption()
      result.get.isEmpty

  /** Builds a checker of the pattern with the default parameters, except the given ones. */
  def checkerOf(
      ir: IRPattern,
      maxGenerationSize: Int = Parameters.DefaultMaxGenerationSize,
      avoidsAttack: (AttackComplexity.Vulnerable, Hotspot) => Boolean = (_, _) => false
  ): FuzzChecker =
    import Parameters.*
    new FuzzChecker(
      ir,
      FuzzProgram.from(ir),
      random0,
      DefaultSeeder,
      DefaultMaxSimpleRepeatCount,
      DefaultMaxNFASize,
      DefaultSeedingLimit,
      DefaultSeedingTimeout,
      DefaultIncubationLimit,
      DefaultIncubationTimeout,
      DefaultMaxGeneStringSize,
      DefaultAttackLimit,
      DefaultAttackTimeout,
      DefaultCrossoverSize,
      DefaultMutationSize,
      DefaultMaxAttackStringSize,
      DefaultMaxInitialGenerationSize,
      maxGenerationSize,
      DefaultMaxIteration,
      DefaultMaxDegree,
      DefaultHeatRatio,
      DefaultAccelerationMode,
      avoidsAttack
    )

  test("FuzzChecker#Population#execute: strings that some repeat count makes no string of the mode"):
    val ir = ReDoS.parse("^[\\uD800-\\uDBFF]*[\\uDC00-\\uDFFF]*$", "u", Dialect.JavaScript)
    val (high, low) = (FString.Wrap(0xd800), FString.Wrap(0xdc00))
    // A high surrogate followed by a low surrogate is one code point in `UTF16CodePoint`, so the string is discarded.
    for seq <- Seq(
        IndexedSeq(high, low),
        IndexedSeq(high, FString.Repeat(0, 1), low),
        IndexedSeq(FString.Repeat(0, 2), low, high)
      )
    do
      val checker = checkerOf(ir)
      val pop = new checker.Population(0, mutable.Set.empty, mutable.Set.empty)
      pop.execute(FString(1, seq))
      assertEquals(checker.inputs.toSet, Set.empty, seq)
    val checker = checkerOf(ir)
    val pop = new checker.Population(0, mutable.Set.empty, mutable.Set.empty)
    pop.execute(FString(1, IndexedSeq(low, high)))
    assertEquals(checker.inputs.toSet, Set(UString(0xdc00, 0xd800)))

  test("FuzzChecker#Population#toGeneration: strings with an avoided attack rank last"):
    val ir = ReDoS.parse("^(a|a)*$", "", Dialect.JavaScript)
    val avoidsAttack = (c: AttackComplexity.Vulnerable, _: Hotspot) => c == AttackComplexity.Exponential(true)
    val checker = checkerOf(ir, 2, avoidsAttack)
    val strs = Seq(1, 2, 3).map(n => FString(n, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'))))
    val traces = strs.zip(Seq(3.0, 2.0, 1.0)).map((s, rate) => Trace(s, rate, 0, Set.empty))
    val pop = new checker.Population(0, mutable.Set.from(traces), mutable.Set.empty)
    checker.attacks(strs(0)) = List((AttackComplexity.Exponential(true), Hotspot.empty))
    checker.attacks(strs(1)) = List((AttackComplexity.Polynomial(2, true), Hotspot.empty))
    assertEquals(pop.toGeneration.traces.map(_.str), IndexedSeq(strs(1), strs(2)))
    // A string with an avoided attack remains when the generation has room.
    val all = new checker.Population(0, mutable.Set.from(traces.take(1)), mutable.Set.empty)
    assertEquals(all.toGeneration.traces.map(_.str), IndexedSeq(strs(0)))
