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
import codes.quine.labs.recheck.fuzz.FString.Repeat
import codes.quine.labs.recheck.fuzz.FString.Wrap
import codes.quine.labs.recheck.fuzz.FuzzChecker.Candidate
import codes.quine.labs.recheck.fuzz.FuzzChecker.Measurement
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
      DefaultMaxAttackCandidates,
      DefaultMaxIteration,
      DefaultMaxDegree,
      DefaultHeatRatio,
      DefaultAccelerationMode,
      avoidsAttack,
      upperBound
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

  test("FuzzChecker.buildSeeds"):
    val witness = Witness[UChar](Seq((Seq('x'), Seq('a')), (Seq.empty, Seq('b', 'c', 'd'))), Seq('!'))
    val seq = Vector(Wrap('x'), Repeat(0, 1), Wrap('a'), Repeat(0, 3), Wrap('b'), Wrap('c'), Wrap('d'), Wrap('!'))
    val first = Vector(Wrap('x'), Repeat(0, 1), Wrap('a'))
    // The repeat count reaches the limit by the complexity, and then by each lower degree, for the whole witness and
    // then for the first pump only.
    assertEquals(
      FuzzChecker.buildSeeds(Complexity.Polynomial(3, witness), 1000, 100, 4),
      Seq(FString(12, seq), FString(23, seq), FString(12, first), FString(23, first))
    )
    assertEquals(
      FuzzChecker.buildSeeds(Complexity.Exponential(witness), 1000, 100, 3).take(3),
      Seq(FString(8, seq), FString(12, seq), FString(23, seq))
    )

  /** A witness of `^a*$` for the upper bounds in the tests. */
  val witness: Witness[UChar] = Witness(Seq((Seq.empty, Seq('a'))), Seq('!'))

  test("FuzzChecker#triesExponential"):
    val ir = ReDoS.parse("^a*$", "", Dialect.JavaScript)
    assert(checkerOf(ir).triesExponential)
    assert(checkerOf(ir, upperBound = Some(Seq(Complexity.Exponential(witness)))).triesExponential)
    assert(!checkerOf(ir, upperBound = Some(Seq(Complexity.Polynomial(2, witness)))).triesExponential)

  test("FuzzChecker#maxTriedDegree"):
    val ir = ReDoS.parse("^a*$", "", Dialect.JavaScript)
    def maxTriedDegree(upperBound: Option[Seq[Complexity.Vulnerable[UChar]]]): Int =
      checkerOf(ir, upperBound = upperBound).maxTriedDegree

    assertEquals(maxTriedDegree(None), Parameters.DefaultMaxDegree)
    assertEquals(maxTriedDegree(Some(Seq(Complexity.Exponential(witness)))), Parameters.DefaultMaxDegree)
    assertEquals(maxTriedDegree(Some(Seq(Complexity.Polynomial(3, witness), Complexity.Polynomial(2, witness)))), 3)
    assertEquals(
      maxTriedDegree(Some(Seq(Complexity.Polynomial(Parameters.DefaultMaxDegree + 1, witness)))),
      Parameters.DefaultMaxDegree
    )

  test("FuzzChecker#seed"):
    val ir = ReDoS.parse("^a*$", "", Dialect.JavaScript)
    val quadratic = Complexity.Polynomial(2, witness)
    val cubic = Complexity.Polynomial(3, witness)
    val seed = checkerOf(ir, upperBound = Some(Seq(cubic, quadratic, quadratic))).seed()
    def seeds(complexity: Complexity.Vulnerable[UChar]): Seq[FString] =
      FuzzChecker.buildSeeds(
        complexity,
        Parameters.DefaultIncubationLimit,
        Parameters.DefaultMaxGeneStringSize,
        Parameters.DefaultMaxDegree
      )
    // The first seeds of the witnesses come before the others, without duplicates.
    val upperBoundSeed =
      Seq(seeds(cubic).head, seeds(quadratic).head) ++ seeds(cubic).tail.filterNot(seeds(quadratic).contains)
    assertEquals(seed.take(upperBoundSeed.size), upperBoundSeed)
    assertEquals(seed.drop(upperBoundSeed.size).toSet, checkerOf(ir).seed().toSet -- upperBoundSeed)

  /** A string of `n` repetitions of the character, followed by `!`. */
  def repeatOf(c: Char, n: Int): FString =
    FString(n, IndexedSeq(FString.Repeat(0, 1), FString.Wrap(c), FString.Wrap('!')))

  /** The degree of the polynomial growth, or `-1` for the exponential one. */
  def degreeOf(measurement: Measurement): Int = measurement.growth match
    case p: Growth.Polynomial => p.degree
    case Growth.Exponential   => -1

  test("FuzzChecker.boundedWidthOf"):
    def boundedWidthOf(source: String): Int =
      FuzzChecker.boundedWidthOf(ReDoS.parse(source, "", Dialect.JavaScript).node)
    assertEquals(boundedWidthOf("abc"), 3)
    assertEquals(boundedWidthOf("ab|cde"), 3)
    // A repetition without the largest count iterates the least times, or once.
    assertEquals(boundedWidthOf("a*b+c?"), 3)
    assertEquals(boundedWidthOf("a{3}b{2,5}c{4,}"), 12)
    assertEquals(boundedWidthOf("(?:ab{3}){2}"), 8)

  test("FuzzChecker#measure"):
    import Parameters.*
    val ir = ReDoS.parse("^(?:a*$|b*b*$|(?:c|c)*$|(?:dd)*(?:dd)*$)", "", Dialect.JavaScript)
    val checker = checkerOf(ir)
    def measure(c: Char, limit: Int = DefaultIncubationLimit, to: Int = Int.MaxValue): Option[Measurement] =
      checker.measure(repeatOf(c, 1), 1, to, limit, DefaultIncubationTimeout)

    // The fewest counts tell the growth.
    assertEquals(measure('a').map(m => (degreeOf(m), m.first, m.last, m.stops)), Some((1, 1, 4, false)))
    assertEquals(measure('b').map(m => (degreeOf(m), m.first, m.last, m.stops)), Some((2, 1, 5, false)))
    assertEquals(measure('c').map(m => (degreeOf(m), m.first, m.stops)), Some((-1, 1, false)))
    // A run that stops tells the exponential growth.
    assertEquals(measure('c', limit = 100).map(m => (degreeOf(m), m.stops)), Some((-1, true)))
    // The steps are a polynomial on every second count when a loop reads the repeated part twice.
    assertEquals(measure('d').map(_.growth).collect { case p: Growth.Polynomial => (p.degree, p.stride) }, Some((2, 2)))
    // The counts above `to` do not run.
    assertEquals(measure('b', to = 4), None)

  test("FuzzChecker#measureBelow"):
    val checker = checkerOf(ReDoS.parse("^b*b*$", "", Dialect.JavaScript))
    assertEquals(checker.measureBelow(repeatOf('b', 100)).map(m => (degreeOf(m), m.first, m.last)), Some((2, 91, 95)))

  test("FuzzChecker#confirm"):
    def confirm(source: String, str: FString): Option[Int] =
      val checker = checkerOf(ReDoS.parse(source, "", Dialect.JavaScript))
      checker.measureFirst(str, Parameters.DefaultIncubationLimit).flatMap(checker.confirm(str, _)).map(degreeOf)

    assertEquals(confirm("^b*b*$", repeatOf('b', 1)), Some(2))
    assertEquals(confirm("^(?:c|c)*$", repeatOf('c', 1)), Some(-1))
    // The steps grow quadratically up to 100 characters, and linearly on longer strings (#1310).
    assertEquals(confirm("^.*?=[^&]{100}", FString(1, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('=')))), None)
    assertEquals(confirm("[a-z0-9]{100,}", FString(1, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a')))), None)
    // The growth beyond the bounded width is the one of the attack.
    assertEquals(confirm("^(?:b{0,20}c|b*b*b*$)", repeatOf('b', 1)), Some(3))
    // The steps grow quadratically until the twelfth copy, on which the match succeeds, though the copies are longer
    // than twice the bounded width before it.
    val copies = FString(1, FString.Repeat(0, 6) +: "00000;".map(c => FString.Wrap(c.toInt)))
    assertEquals(confirm("(?:\\d+;){12}", copies), None)
    assertEquals(confirm("^(?:\\d+;)*(?:\\d+;)*$", FString(1, copies.seq :+ FString.Wrap('!'))), Some(2))
    // A string without repetitions is confirmed if its run stops.
    val exponential = checkerOf(ReDoS.parse("^(?:c|c)*$", "", Dialect.JavaScript))
    val measurement = Measurement(Growth.Exponential, 1, 0, true)
    assertEquals(
      exponential.confirm(FString(UString.from(Seq.fill(40)('c'.toInt) :+ '!'.toInt)), measurement),
      Some(measurement)
    )
    assertEquals(exponential.confirm(FString(UString('c', '!')), measurement), None)

  test("FuzzChecker#attackCount"):
    val checker = checkerOf(ReDoS.parse("^b*b*$", "", Dialect.JavaScript))
    val square = Growth.Polynomial(1, 1, Vector(1, 3, 2))
    // The least count of `n * n` steps reaching the limit.
    assertEquals(checker.attackCount(repeatOf('b', 1), square), Some(38730))
    // The largest count within `maxAttackStringSize` when the steps do not reach the limit.
    val long = FString(1, FString.Repeat(0, 100) +: IndexedSeq.fill(100)(FString.Wrap('b')))
    assertEquals(checker.attackCount(long, square), Some(3000))

  /** The candidates after executing the strings. */
  def candidatesOf(checker: FuzzChecker, strs: FString*): Seq[Candidate] =
    val pop = new checker.Population(0, mutable.Set.empty, mutable.Set.empty)
    strs.foreach(pop.execute)
    pop.candidates.values.toSeq

  test("FuzzChecker#attack: the steepest candidate first"):
    val ir = ReDoS.parse("^(?:b*b*$|(?:c|c)*$)", "", Dialect.JavaScript)
    val checker = checkerOf(ir)
    val results = checker.attack(candidatesOf(checker, repeatOf('b', 1), repeatOf('c', 1)).iterator)
    assertEquals(results.next()._1, AttackComplexity.Exponential(true))
    assertEquals(results.next()._1, AttackComplexity.Polynomial(2, true))
    // A string whose steps grow linearly is not a candidate.
    assertEquals(candidatesOf(checker, FString(1000, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('x')))), Seq.empty)

  test("FuzzChecker#additionsOf"):
    val checker = checkerOf(ReDoS.parse("^a*b$", "", Dialect.JavaScript))
    val seq = IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'))
    val additions = checker.additionsOf(FString(5, seq))
    // A character and its repetition are added at the start and at the end of the repetition.
    assert(additions.contains(FString(1, FString.Wrap('b') +: seq)))
    assert(additions.contains(FString(1, seq :+ FString.Wrap('b'))))
    assert(additions.contains(FString(1, seq ++ IndexedSeq(FString.Repeat(0, 1), FString.Wrap('b')))))
    assertEquals(additions.size, 2 * 2 * checker.alphabet.pairs.size)

  test("FuzzChecker#boost"):
    val checker = checkerOf(ReDoS.parse("\\s*<(\\w+)(?:[^>]+)?>", "", Dialect.JavaScript))
    // The steps of `<{n}0{n}` grow quadratically, where only the last `<` goes on to `\w+`.
    val seq = IndexedSeq(FString.Repeat(0, 1), FString.Wrap('<'), FString.Repeat(0, 1), FString.Wrap('0'))
    val candidate = candidatesOf(checker, FString(1, seq)).head
    assertEquals(degreeOf(candidate.measurement), 2)
    // Without the repetition of `<`, a repetition of a space before it makes the steps cubic.
    val boosted = checker.boost(candidate).get
    assertEquals(degreeOf(boosted.measurement), 3)
    assertEquals(boosted.str.countRepeat, 2)
    // A fingerprint is boosted once.
    assertEquals(checker.boost(candidate), None)

  test("FuzzChecker#attack: the degree is exact"):
    // The steps of the lower degrees are many, which a slope of the steps takes for the cubic growth.
    val ir = ReDoS.parse("^(?:a|b)*[ab]*[ab]*[ab]*$", "", Dialect.JavaScript)
    val checker = checkerOf(ir)
    val results = checker.attack(candidatesOf(checker, repeatOf('a', 1)).iterator)
    assertEquals(results.next()._1, AttackComplexity.Polynomial(4, true))

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

  test("FuzzChecker#attack: the fingerprints of failed attacks"):
    // The steps grow quadratically up to 100 characters, and the match succeeds on longer strings (#1310).
    val ir = ReDoS.parse("[a-z0-9]{100,}", "", Dialect.JavaScript)
    val checker = checkerOf(ir)
    val str = FString(30, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a')))
    val same = FString(15, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'), FString.Repeat(0, 1), FString.Wrap('a')))
    val candidates = candidatesOf(checker, str, same)
    assertEquals(candidates.map(_.str), Seq(str))
    assertEquals(checker.attack(candidates.iterator).toSeq, Seq.empty)
    assertEquals(checker.failedFingerprints.toSet, Set(str.fingerprint))
    // The quadratic steps of 30 characters stay below the attack limit of the quick check.
    assert(!check("[a-z0-9]{30,}", "", quick = true))

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
    assertEquals(checker.measurements.keySet.toSet, Set(seq, FString.Repeat(0, 2) +: seq.tail))

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
    assertEquals(checker.measurements.keySet.toSet, Set(whole))

  test("FuzzChecker#hotspotOf"):
    val source = "^x*(?:a|a)*b*b*$"
    val checker = checkerOf(ReDoS.parse(source, "", Dialect.JavaScript))
    val str = FString(1, IndexedSeq(FString.Wrap('x'), FString.Repeat(0, 1), FString.Wrap('b'), FString.Wrap('!')))
    val measurement = checker.measureFirst(str, Parameters.DefaultIncubationLimit).get
    // The second `b*` reads the characters again on each backtracking of the first, so it is hot on the attack string.
    val hot = checker
      .hotspotOf(str, measurement, 50000)
      .spots
      .collect:
        case Hotspot.Spot(start, end, Hotspot.Hot) => (start, end)
    assertEquals(hot, Seq((13, 14)))

  test("FuzzChecker#Population#execute: no candidate of a fingerprint whose attack failed"):
    val ir = ReDoS.parse("^(?:a|a)*$", "", Dialect.JavaScript)
    val checker = checkerOf(ir)
    val pop = new checker.Population(0, mutable.Set.empty, mutable.Set.empty)
    val str = FString(30, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'), FString.Wrap('!')))
    checker.failedFingerprints.add(str.fingerprint)
    pop.execute(str)
    assert(pop.candidates.isEmpty)

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
