package codes.quine.labs.recheck
package fuzz

import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.automaton.Witness
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.fuzz.Attacker.Candidate
import codes.quine.labs.recheck.fuzz.Attacker.Measurement
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRPattern

class AttackerSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Builds an attacker of the pattern with the default parameters, except the given ones. */
  def attackerOf(ir: IRPattern, upperBound: Option[Seq[Complexity.Vulnerable[UChar]]] = None): Attacker =
    import Parameters.*
    new Attacker(
      ir,
      FuzzProgram.from(ir),
      DefaultIncubationLimit,
      DefaultIncubationTimeout,
      DefaultAttackLimit,
      DefaultAttackTimeout,
      DefaultMaxAttackStringSize,
      DefaultMaxDegree,
      DefaultHeatRatio,
      DefaultAccelerationMode,
      (_, _) => false,
      upperBound
    )

  /** A witness of `^a*$` for the upper bounds in the tests. */
  val witness: Witness[UChar] = Witness(Seq((Seq.empty, Seq('a'))), Seq('!'))

  test("Attacker#triesExponential"):
    val ir = ReDoS.parse("^a*$", "", Dialect.JavaScript)
    assert(attackerOf(ir).triesExponential)
    assert(attackerOf(ir, Some(Seq(Complexity.Exponential(witness)))).triesExponential)
    assert(!attackerOf(ir, Some(Seq(Complexity.Polynomial(2, witness)))).triesExponential)

  test("Attacker#maxTriedDegree"):
    val ir = ReDoS.parse("^a*$", "", Dialect.JavaScript)
    def maxTriedDegree(upperBound: Option[Seq[Complexity.Vulnerable[UChar]]]): Int =
      attackerOf(ir, upperBound).maxTriedDegree

    assertEquals(maxTriedDegree(None), Parameters.DefaultMaxDegree)
    assertEquals(maxTriedDegree(Some(Seq(Complexity.Exponential(witness)))), Parameters.DefaultMaxDegree)
    assertEquals(maxTriedDegree(Some(Seq(Complexity.Polynomial(3, witness), Complexity.Polynomial(2, witness)))), 3)
    assertEquals(
      maxTriedDegree(Some(Seq(Complexity.Polynomial(Parameters.DefaultMaxDegree + 1, witness)))),
      Parameters.DefaultMaxDegree
    )

  /** A string of `n` repetitions of the character, followed by `!`. */
  def repeatOf(c: Char, n: Int): FString =
    FString(n, IndexedSeq(FString.Repeat(0, 1), FString.Wrap(c), FString.Wrap('!')))

  /** The degree of the polynomial growth, or `-1` for the exponential one. */
  def degreeOf(measurement: Measurement): Int = measurement.growth match
    case p: Growth.Polynomial => p.degree
    case Growth.Exponential   => -1

  test("Attacker.boundedWidthOf"):
    def boundedWidthOf(source: String): Int =
      Attacker.boundedWidthOf(ReDoS.parse(source, "", Dialect.JavaScript).node)
    assertEquals(boundedWidthOf("abc"), 3)
    assertEquals(boundedWidthOf("ab|cde"), 3)
    // A repetition without the largest count iterates the least times, or once.
    assertEquals(boundedWidthOf("a*b+c?"), 3)
    assertEquals(boundedWidthOf("a{3}b{2,5}c{4,}"), 12)
    assertEquals(boundedWidthOf("(?:ab{3}){2}"), 8)

  test("Attacker#measure"):
    import Parameters.*
    val ir = ReDoS.parse("^(?:a*$|b*b*$|(?:c|c)*$|(?:dd)*(?:dd)*$)", "", Dialect.JavaScript)
    val attacker = attackerOf(ir)
    def measure(c: Char, limit: Int = DefaultIncubationLimit, to: Int = Int.MaxValue): Option[Measurement] =
      attacker.measure(repeatOf(c, 1), 1, to, limit, DefaultIncubationTimeout)

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

  test("Attacker#measureBelow"):
    val attacker = attackerOf(ReDoS.parse("^b*b*$", "", Dialect.JavaScript))
    assertEquals(attacker.measureBelow(repeatOf('b', 100)).map(m => (degreeOf(m), m.first, m.last)), Some((2, 91, 95)))

  test("Attacker#confirm"):
    def confirm(source: String, str: FString): Option[Int] =
      val attacker = attackerOf(ReDoS.parse(source, "", Dialect.JavaScript))
      attacker.measureFirst(str, Parameters.DefaultIncubationLimit).flatMap(attacker.confirm(str, _)).map(degreeOf)

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
    val exponential = attackerOf(ReDoS.parse("^(?:c|c)*$", "", Dialect.JavaScript))
    val measurement = Measurement(Growth.Exponential, 1, 0, true)
    assertEquals(
      exponential.confirm(FString(UString.from(Seq.fill(40)('c'.toInt) :+ '!'.toInt)), measurement),
      Some(measurement)
    )
    assertEquals(exponential.confirm(FString(UString('c', '!')), measurement), None)

  test("Attacker#attackCount"):
    val attacker = attackerOf(ReDoS.parse("^b*b*$", "", Dialect.JavaScript))
    val square = Growth.Polynomial(1, 1, Vector(1, 3, 2))
    // The least count of `n * n` steps reaching the limit.
    assertEquals(attacker.attackCount(repeatOf('b', 1), square), Some(38730))
    // The largest count within `maxAttackStringSize` when the steps do not reach the limit.
    val long = FString(1, FString.Repeat(0, 100) +: IndexedSeq.fill(100)(FString.Wrap('b')))
    assertEquals(attacker.attackCount(long, square), Some(3000))

  /** The candidates of the strings, whose runs do not stop. */
  def candidatesOf(attacker: Attacker, strs: FString*): Seq[Candidate] =
    strs.flatMap(attacker.candidateOf(_, false))

  test("Attacker#candidateOf"):
    val attacker = attackerOf(ReDoS.parse("^(?:b*b*$|(?:c|c)*$)", "", Dialect.JavaScript))
    assertEquals(candidatesOf(attacker, repeatOf('b', 1)).map(c => degreeOf(c.measurement)), Seq(2))
    // A string whose steps grow linearly is not a candidate.
    assertEquals(candidatesOf(attacker, FString(1000, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('x')))), Seq.empty)
    // A string without repetitions is a candidate if its run stops.
    val constant = FString(UString.from(Seq.fill(40)('c'.toInt) :+ '!'.toInt))
    assertEquals(attacker.candidateOf(constant, false), None)
    assertEquals(attacker.candidateOf(constant, true).map(c => degreeOf(c.measurement)), Some(-1))

  test("Attacker#attack: the steepest candidate first"):
    val ir = ReDoS.parse("^(?:b*b*$|(?:c|c)*$)", "", Dialect.JavaScript)
    val attacker = attackerOf(ir)
    val results = attacker.attack(candidatesOf(attacker, repeatOf('b', 1), repeatOf('c', 1)).iterator)
    assertEquals(results.next()._1, AttackComplexity.Exponential(true))
    assertEquals(results.next()._1, AttackComplexity.Polynomial(2, true))

  test("Attacker#additionsOf"):
    val attacker = attackerOf(ReDoS.parse("^a*b$", "", Dialect.JavaScript))
    val seq = IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'))
    val additions = attacker.additionsOf(FString(5, seq))
    // A character and its repetition are added at the start and at the end of the repetition.
    assert(additions.contains(FString(1, FString.Wrap('b') +: seq)))
    assert(additions.contains(FString(1, seq :+ FString.Wrap('b'))))
    assert(additions.contains(FString(1, seq ++ IndexedSeq(FString.Repeat(0, 1), FString.Wrap('b')))))
    assertEquals(additions.size, 2 * 2 * attacker.fuzz.alphabet.pairs.size)

  test("Attacker#boost"):
    val attacker = attackerOf(ReDoS.parse("\\s*<(\\w+)(?:[^>]+)?>", "", Dialect.JavaScript))
    // The steps of `<{n}0{n}` grow quadratically, where only the last `<` goes on to `\w+`.
    val seq = IndexedSeq(FString.Repeat(0, 1), FString.Wrap('<'), FString.Repeat(0, 1), FString.Wrap('0'))
    val candidate = candidatesOf(attacker, FString(1, seq)).head
    assertEquals(degreeOf(candidate.measurement), 2)
    // Without the repetition of `<`, a repetition of a space before it makes the steps cubic.
    val boosted = attacker.boost(candidate).get
    assertEquals(degreeOf(boosted.measurement), 3)
    assertEquals(boosted.str.countRepeat, 2)
    // A fingerprint is boosted once.
    assertEquals(attacker.boost(candidate), None)

  test("Attacker#attack: the degree is exact"):
    // The steps of the lower degrees are many, which a slope of the steps takes for the cubic growth.
    val ir = ReDoS.parse("^(?:a|b)*[ab]*[ab]*[ab]*$", "", Dialect.JavaScript)
    val attacker = attackerOf(ir)
    val results = attacker.attack(candidatesOf(attacker, repeatOf('a', 1)).iterator)
    assertEquals(results.next()._1, AttackComplexity.Polynomial(4, true))

  test("Attacker#attack: the fingerprints of failed attacks"):
    // The steps grow quadratically up to 100 characters, and the match succeeds on longer strings (#1310).
    val ir = ReDoS.parse("[a-z0-9]{100,}", "", Dialect.JavaScript)
    val attacker = attackerOf(ir)
    val str = FString(30, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a')))
    val same = FString(15, IndexedSeq(FString.Repeat(0, 1), FString.Wrap('a'), FString.Repeat(0, 1), FString.Wrap('a')))
    val candidates = candidatesOf(attacker, str)
    assertEquals(candidates.map(_.str), Seq(str))
    assertEquals(attacker.attack(candidates.iterator).toSeq, Seq.empty)
    assertEquals(attacker.failedFingerprints.toSet, Set(str.fingerprint))
    // A string of the fingerprint is not a candidate any more.
    assertEquals(candidatesOf(attacker, same), Seq.empty)

  test("Attacker#hotspotOf"):
    val source = "^x*(?:a|a)*b*b*$"
    val attacker = attackerOf(ReDoS.parse(source, "", Dialect.JavaScript))
    val str = FString(1, IndexedSeq(FString.Wrap('x'), FString.Repeat(0, 1), FString.Wrap('b'), FString.Wrap('!')))
    val measurement = attacker.measureFirst(str, Parameters.DefaultIncubationLimit).get
    // The second `b*` reads the characters again on each backtracking of the first, so it is hot on the attack string.
    val hot = attacker
      .hotspotOf(str, measurement, 50000)
      .spots
      .collect:
        case Hotspot.Spot(start, end, Hotspot.Hot) => (start, end)
    assertEquals(hot, Seq((13, 14)))
