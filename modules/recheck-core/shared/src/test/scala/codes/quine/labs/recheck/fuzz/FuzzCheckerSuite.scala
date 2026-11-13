package codes.quine.labs.recheck
package fuzz

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.Random
import scala.util.Try

import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.automaton.Witness
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.fuzz.Attacker.Candidate
import codes.quine.labs.recheck.fuzz.Attacker.Measurement
import codes.quine.labs.recheck.fuzz.FuzzChecker.Trace
import codes.quine.labs.resyntax.charset.UChar
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
      ._1
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

    // The steps grow quadratically up to 30 characters, which stay below the attack limit of the quick check (#1310).
    assert(!check("[a-z0-9]{30,}", "", quick = true))

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
        ._1
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
        ._1
        .nextOption()
      result.get.isEmpty

  /** Builds a checker of the pattern with the default parameters, except the given ones. */
  def checkerOf(
      ir: IRPattern,
      maxGenerationSize: Int = Parameters.DefaultMaxGenerationSize,
      avoidsAttack: (AttackComplexity.Vulnerable, Hotspot) => Boolean = (_, _) => false,
      upperBound: Option[Seq[Complexity.Vulnerable[UChar]]] = None
  ): FuzzChecker =
    import Parameters.*
    val fuzz = FuzzProgram.from(ir)
    val attacker = new Attacker(
      ir,
      fuzz,
      DefaultIncubationLimit,
      DefaultIncubationTimeout,
      DefaultAttackLimit,
      DefaultAttackTimeout,
      DefaultMaxAttackStringSize,
      DefaultMaxDegree,
      DefaultHeatRatio,
      DefaultAccelerationMode,
      avoidsAttack,
      upperBound
    )
    new FuzzChecker(
      ir,
      fuzz,
      random0,
      DefaultSeeder,
      DefaultMaxSimpleRepeatCount,
      DefaultMaxNFASize,
      DefaultSeedingLimit,
      DefaultSeedingTimeout,
      DefaultIncubationLimit,
      DefaultIncubationTimeout,
      DefaultMaxGeneStringSize,
      DefaultCrossoverSize,
      DefaultMutationSize,
      DefaultMaxInitialGenerationSize,
      maxGenerationSize,
      DefaultMaxAttackCandidates,
      DefaultMaxIteration,
      DefaultMaxDegree,
      DefaultAccelerationMode,
      upperBound,
      attacker
    )

  test("FuzzChecker.upperBound"):
    import Parameters.*
    def upperBound(source: String, dialect: Dialect = Dialect.JavaScript, maxInitialGenerationSize: Int = 100) =
      val ir = ReDoS.parse(source, "", dialect)
      FuzzChecker.upperBound(ir, DefaultMaxRepeatCount, DefaultMaxNFASize, maxInitialGenerationSize)
    // The bound replaces the back-reference with a copy of `a*`, so the worst is cubic.
    val cubic = upperBound("^(a*)a*\\1b").get
    assertEquals(cubic.head.toAttackComplexity, AttackComplexity.Polynomial(3, false))
    // The look-ahead DFA of the bound tells no failure, so the suffix is a character that the last state cannot read.
    assert(cubic.forall(_.witness.suffix.nonEmpty))
    // One comes even without an initial generation, which tells the complexity of the bound.
    assertEquals(upperBound("^(a+)\\1*$", maxInitialGenerationSize = 0).get.size, 1)
    // A linear bound has none.
    assertEquals(upperBound("^([a-z]+)=\\1$"), Some(Seq.empty))
    // A non-atomic look-around makes no bound.
    assertEquals(upperBound("^(?*a+)b+$", Dialect.PCRE), None)
    // Nor do the repetitions that stay with too many repeats, whose bound would hide the `2^50` paths of `(a?){50}`.
    assertEquals(upperBound("^(a?){50}a{50}$"), None)
    // The repetition of any character at the end matches any rest, so it does not make the bound quadratic.
    val tail = ReDoS.parse("^.*?(?:ab).*$", "s", Dialect.JavaScript)
    assertEquals(FuzzChecker.upperBound(tail, DefaultMaxRepeatCount, DefaultMaxNFASize, 100), Some(Seq.empty))

  test("FuzzChecker.isLinearBeyondCounts"):
    import Parameters.*
    def isLinearBeyondCounts(source: String): Boolean =
      val ir = ReDoS.parse(source, "", Dialect.JavaScript)
      val bound = FuzzChecker.upperBound(ir, DefaultMaxRepeatCount, DefaultMaxNFASize, 100)
      FuzzChecker.isLinearBeyondCounts(ir, bound, DefaultMaxRepeatCount, DefaultMaxNFASize, DefaultAttackLimit)

    // The steps grow quadratically only up to 100 characters after `=` (#1310).
    assert(isLinearBeyondCounts("^.*?=[^&]{100}!"))
    // The steps on so many characters reach the limit.
    assert(!isLinearBeyondCounts("^.*?=[^&]{30000}!"))
    // The repetition is not the only loop reading the characters again.
    assert(!isLinearBeyondCounts("^.*?=[^&]{100}[^&]*!"))
    assert(!isLinearBeyondCounts("^.*?=[^&]*!"))
    // The steps of an exponential bound may reach the limit on a few characters.
    assert(!isLinearBeyondCounts("^(?:a|a){40}$"))
    assert(!isLinearBeyondCounts("^(a?){50}a{50}$"))

  /** A witness of `^a*$` for the upper bounds in the tests. */
  val witness: Witness[UChar] = Witness(Seq((Seq.empty, Seq('a'))), Seq('!'))

  test("FuzzChecker#seederSeed"):
    val ir = ReDoS.parse("^a*$", "", Dialect.JavaScript)
    val checker = checkerOf(ir, upperBound = Some(Seq(Complexity.Polynomial(3, witness))))
    val witnessSeed = checker.witnessSeed()
    assert(witnessSeed.nonEmpty)
    // The seeds of the seeder are the ones without the upper bound, except the seeds of the witnesses.
    val seederSeed = checkerOf(ir).seederSeed(Seq.empty)
    assert(seederSeed.nonEmpty)
    assertEquals(checker.seederSeed(witnessSeed).toSet, seederSeed.toSet -- witnessSeed)
    assertEquals(checker.seederSeed(seederSeed).toSet, Set.empty)

  test("FuzzChecker#init: the seeder runs after the steepest attacks from the upper bound"):
    val ir = ReDoS.parse("^a*a*$", "", Dialect.JavaScript)
    val checker = checkerOf(ir, upperBound = Some(Seq(Complexity.Polynomial(2, witness))))
    val (results, pop) = checker.init()
    assertEquals(results.next()._1, AttackComplexity.Polynomial(2, true))
    val witnessSeed = checker.witnessSeed()
    assertEquals(pop.set.map(_.str).toSet, witnessSeed.toSet)
    // The seeds of the seeder run when the attacks run out.
    results.toSeq
    assert(checker.seederSeed(witnessSeed).exists(str => pop.set.exists(_.str == str)))
    // A candidate below the steepest growth of an exponential upper bound waits for the seeds of the seeder.
    val waiting = checkerOf(ir, upperBound = Some(Seq(Complexity.Exponential(witness))))
    val (waitingResults, waitingPop) = waiting.init()
    assertEquals(waitingResults.next()._1, AttackComplexity.Polynomial(2, true))
    assert(waiting.seederSeed(waiting.witnessSeed()).exists(str => waitingPop.set.exists(_.str == str)))

  test("FuzzChecker#init: the seeds of other suffixes"):
    import Parameters.*
    val ir = ReDoS.parse("^(?:a|a)*(?!.*xyz)", "", Dialect.JavaScript)
    val bound = FuzzChecker.upperBound(ir, DefaultMaxRepeatCount, DefaultMaxNFASize, 100)
    // The match of the witness succeeds unless `xyz` follows, which makes the look-ahead fail.
    assertEquals(checkerOf(ir, upperBound = bound).init()._1.next()._1, AttackComplexity.Exponential(true))

  /** The degree of the polynomial growth, or `-1` for the exponential one. */
  def degreeOf(measurement: Measurement): Int = measurement.growth match
    case p: Growth.Polynomial => p.degree
    case Growth.Exponential   => -1

  /** The candidates after executing the strings. */
  def candidatesOf(checker: FuzzChecker, strs: FString*): Seq[Candidate] =
    val pop = new checker.Population(0, mutable.Set.empty, mutable.Set.empty)
    strs.foreach(pop.execute)
    pop.candidates.values.toSeq

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

  test("FuzzChecker#Population#execute: candidates"):
    val ir = ReDoS.parse("^(?:a|a)*$", "", Dialect.JavaScript)
    val checker = checkerOf(ir)
    val pop = new checker.Population(0, mutable.Set.empty, mutable.Set.empty)
    val max = Parameters.DefaultMaxAttackCandidates
    val strs = (1 to max + 1).map: k =>
      FString(30, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'), FString.Wrap('a' + k)))
    // The same fingerprint is one candidate, and no string runs after `maxAttackCandidates` candidates.
    pop.execute(strs(0))
    pop.execute(strs(0).copy(n = 31))
    pop.execute(strs(0).copy(seq = FString.Repeat(2, 1) +: strs(0).seq.tail))
    strs.foreach(pop.execute)
    assertEquals(pop.candidates.values.map(_.str).toSeq, strs.take(max))
    assertEquals(checker.inputs.size, max + 2)

  test("FuzzChecker#Population#execute: the growth of the steps"):
    val seq = IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'), FString.Wrap('!'))
    // A string of a few steps is a candidate when its steps grow more than linearly.
    val quadratic = checkerOf(ReDoS.parse("^a*a*$", "", Dialect.JavaScript))
    assertEquals(
      candidatesOf(quadratic, FString(1, seq)).map(c => (c.str, degreeOf(c.measurement))),
      Seq((FString(1, seq), 2))
    )
    assertEquals(quadratic.inputs.toSet, Set(UString('a', '!')))
    val linear = checkerOf(ReDoS.parse("^a*$", "", Dialect.JavaScript))
    assertEquals(candidatesOf(linear, FString(1, seq), FString(1000, seq)), Seq.empty)
    // The growth is measured once for a sequence, even after the run of the input.
    val checker = checkerOf(ReDoS.parse("^a*a*$", "", Dialect.JavaScript))
    val candidates = candidatesOf(checker, FString(UString('a', '!')), FString(1, seq), FString(3, seq))
    assertEquals(candidates.map(_.str), Seq(FString(1, seq)))
    assertEquals(checker.attacker.measurements.keySet.toSet, Set(seq, FString.Repeat(0, 2) +: seq.tail))

  test("FuzzChecker#Population#execute: a string whose run stops without the growth on the first counts"):
    // The steps grow on the counts beyond the bounded width of the pattern only, which the first counts are not.
    val checker = checkerOf(ReDoS.parse("^(?:a{300}a*a*)?$", "", Dialect.JavaScript))
    val seq = IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'), FString.Wrap('!'))
    assertEquals(candidatesOf(checker, FString(1, seq)), Seq.empty)
    val candidates = candidatesOf(checker, FString(1000, seq))
    assertEquals(candidates.map(c => (c.str, degreeOf(c.measurement))), Seq((FString(1000, seq), 2)))

  test("FuzzChecker#Population#execute: a string reaching a new coverage item repeats as a whole"):
    val checker = checkerOf(ReDoS.parse("foo.+bar", "", Dialect.JavaScript))
    val pop = new checker.Population(0, mutable.Set.empty, mutable.Set.empty)
    val str = FString(UString('f', 'o', 'o', 'x', 'x'))
    val whole = FString.Repeat(0, 5) +: str.seq
    pop.execute(str)
    assertEquals(pop.candidates.values.map(_.str).toSeq, Seq(FString(1, whole)))
    // The string reaches no new coverage item.
    pop.execute(FString(UString('f', 'o', 'o', 'x', 'x', 'x')))
    assertEquals(checker.attacker.measurements.keySet.toSet, Set(whole))

  test("FuzzChecker#Population#execute: no candidate of a fingerprint whose attack failed"):
    val ir = ReDoS.parse("^(?:a|a)*$", "", Dialect.JavaScript)
    val checker = checkerOf(ir)
    val pop = new checker.Population(0, mutable.Set.empty, mutable.Set.empty)
    val str = FString(30, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'), FString.Wrap('!')))
    checker.attacker.failedFingerprints.add(str.fingerprint)
    pop.execute(str)
    assert(pop.candidates.isEmpty)

  test("FuzzChecker#Population#toGeneration: strings with an avoided attack rank last"):
    val ir = ReDoS.parse("^(a|a)*$", "", Dialect.JavaScript)
    val avoidsAttack = (c: AttackComplexity.Vulnerable, _: Hotspot) => c == AttackComplexity.Exponential(true)
    val checker = checkerOf(ir, 2, avoidsAttack)
    val strs = Seq(1, 2, 3).map(n => FString(n, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'))))
    val traces = strs.zip(Seq(3.0, 2.0, 1.0)).map((s, rate) => Trace(s, rate, 0, Set.empty))
    val pop = new checker.Population(0, mutable.Set.from(traces), mutable.Set.empty)
    checker.attacker.attacks(strs(0)) = List((AttackComplexity.Exponential(true), Hotspot.empty))
    checker.attacker.attacks(strs(1)) = List((AttackComplexity.Polynomial(2, true), Hotspot.empty))
    assertEquals(pop.toGeneration.traces.map(_.str), IndexedSeq(strs(1), strs(2)))
    // A string with an avoided attack remains when the generation has room.
    val all = new checker.Population(0, mutable.Set.from(traces.take(1)), mutable.Set.empty)
    assertEquals(all.toGeneration.traces.map(_.str), IndexedSeq(strs(0)))
