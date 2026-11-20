package codes.quine.labs.recheck.verification

import scala.concurrent.duration.*

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.DiagnosticsState
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.Hotspot.*
import codes.quine.labs.recheck.diagnostics.RefutedAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.recheck.exception.RuntimeAttacksRefutedException
import codes.quine.labs.recheck.parameters.AttackStringCharacteristics
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.recheck.verification.RuntimeAttacker.AttackKey
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

class RuntimeAttackerSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  def attackOf(prefix: String, pump: String, suffix: String, n: Int): AttackPattern =
    AttackPattern(
      Seq((UnitMode.UTF16.decode(prefix), UnitMode.UTF16.decode(pump), 0)),
      UnitMode.UTF16.decode(suffix),
      n
    )

  val hotspot: Hotspot = Hotspot(Seq(Spot(1, 2, Hot)))

  def runtimeAttack(result: RuntimeAttackResult): Option[RuntimeAttack] =
    Some(RuntimeAttack("node v0", 1.second, result))

  val finish: Option[RuntimeAttack] = runtimeAttack(RuntimeAttackResult.Finish(1.millis))

  val timeout: Option[RuntimeAttack] = runtimeAttack(RuntimeAttackResult.Timeout)

  /** Returns the state of a checking of `^a*a*$` and its runtime attacker, whose runtime gives the results in order and
    * whose attack strings have 10 repeats.
    */
  def runtimeAttackerOf(
      results: Seq[Option[RuntimeAttack]],
      maxRefutedAttacks: Int = 100
  ): (DiagnosticsState, RuntimeAttacker) =
    val state = new DiagnosticsState("^a*a*$", "", Dialect.JavaScript)
    state.unitMode = Some(UnitMode.UTF16)
    state.checker = Some(Diagnostics.Checker.Automaton)
    val rest = results.iterator
    val limit = Parameters.DefaultRuntimeAttackLimit
    val attacker =
      new RuntimeAttacker(state, _ => rest.next(), Set.empty, Seq.empty, UnitMode.UTF16, limit, 10, maxRefutedAttacks)
    (state, attacker)

  def attack(attacker: RuntimeAttacker, degree: Int): Option[Diagnostics.Vulnerable] =
    attacker.attack(AttackComplexity.Polynomial(degree, false), attackOf("", "a", "", 1), hotspot)

  def refuted(degree: Int): RefutedAttack =
    RefutedAttack(AttackComplexity.Polynomial(degree, false), attackOf("", "a", "", 10), hotspot, finish.get)

  def vulnerable(
      degree: Int,
      runtimeAttack: Option[RuntimeAttack],
      refutedAttacks: Seq[RefutedAttack]
  ): Diagnostics.Vulnerable =
    Diagnostics.Vulnerable(
      "^a*a*$",
      "",
      Dialect.JavaScript,
      UnitMode.UTF16,
      AttackComplexity.Polynomial(degree, false),
      attackOf("", "a", "", 10),
      hotspot,
      runtimeAttack,
      refutedAttacks,
      Diagnostics.Checker.Automaton
    )

  test("RuntimeAttacker#isRefuted"):
    val (_, attacker) = runtimeAttackerOf(Seq(finish))
    def isRefuted(degree: Int): Boolean =
      attacker.isRefuted(AttackComplexity.Polynomial(degree, false), attackOf("", "a", "", 1), hotspot)

    assert(!isRefuted(3))
    attack(attacker, 3)
    assert(isRefuted(3))
    assert(!isRefuted(2))

  test("RuntimeAttacker#attack"):
    // A refuted attack is recorded, and the diagnostics of the one that is not refuted have it.
    val (state, attacker) = runtimeAttackerOf(Seq(finish, timeout))
    assertEquals(attack(attacker, 3), None)
    assertEquals(state.refutedAttacks.toSeq, Seq(refuted(3)))
    // The same attack as a refuted one is not run, so the next one takes the next result.
    assertEquals(attack(attacker, 3), None)
    assertEquals(attack(attacker, 2), Some(vulnerable(2, timeout, Seq(refuted(3)))))
    // An attack that is not run is not refuted.
    assertEquals(attack(runtimeAttackerOf(Seq(None))._2, 3), Some(vulnerable(3, None, Seq.empty)))

    // It stops at the limit of refuted attacks.
    val (limitedState, limited) = runtimeAttackerOf(Seq(finish, finish), maxRefutedAttacks = 2)
    assertEquals(attack(limited, 4), None)
    intercept[RuntimeAttacksRefutedException](attack(limited, 3))
    assertEquals(limitedState.refutedAttacks.toSeq, Seq(refuted(4), refuted(3)))
    // A non-positive limit stops at the first refuted attack.
    intercept[RuntimeAttacksRefutedException](attack(runtimeAttackerOf(Seq(finish), maxRefutedAttacks = 0)._2, 3))

  test("RuntimeAttacker.characteristicSets"):
    def setsOf(source: String, characteristics: AttackStringCharacteristics): Seq[CharSet] =
      RuntimeAttacker.characteristicSets(ReDoS.parse(source, "", Dialect.JavaScript), characteristics)

    val rest = CharSet.range(0x00, 0xffff).diff(CharSet.of('a', 'b', 'c', 'd'))

    assertEquals(setsOf("ab[cd]", AttackStringCharacteristics.None), Seq.empty)
    assertEquals(setsOf("ab[cd]", AttackStringCharacteristics.Latin1), Seq(CharSet.range(0x100, 0xffff)))
    assertEquals(setsOf("ab[cd]", AttackStringCharacteristics.CharClass).toSet, Set(CharSet.range('c', 'd'), rest))
    assertEquals(
      setsOf("ab[cd]", AttackStringCharacteristics.Alphabet).toSet,
      Set(CharSet.single('a'), CharSet.single('b'), CharSet.range('c', 'd'), rest)
    )
    // A set of characters that is in the set of a literal character is not of `CharClass`.
    assertEquals(
      setsOf("a[ab]", AttackStringCharacteristics.CharClass).toSet,
      Set(CharSet.single('b'), CharSet.range(0x00, 0xffff).diff(CharSet.of('a', 'b')))
    )

  test("RuntimeAttacker.AttackKey"):
    val readStarts = Set(SourceLocation(0, 1), SourceLocation(2, 3), SourceLocation(4, 5), SourceLocation(6, 7))
    val attack = attackOf("", "a", "", 10)
    def key(complexity: AttackComplexity.Vulnerable, hotspot: Hotspot): AttackKey =
      AttackKey(complexity, attack, hotspot, readStarts, Seq.empty)

    val hotspot = Hotspot(Seq(Spot(0, 1, Warm), Spot(2, 3, Cold), Spot(4, 5, Hot), Spot(6, 7, Hot)))
    assertEquals(
      key(AttackComplexity.Exponential(true), hotspot),
      AttackKey(None, Seq(Spot(0, 1, Warm), Spot(2, 3, Cold), Spot(4, 5, Hot), Spot(6, 7, Hot)), Set.empty)
    )
    assertEquals(
      key(AttackComplexity.Polynomial(2, false), hotspot),
      key(AttackComplexity.Polynomial(2, true), hotspot)
    )
    assertNotEquals(
      key(AttackComplexity.Polynomial(2, false), hotspot),
      key(AttackComplexity.Polynomial(2, false), Hotspot(Seq(Spot(0, 1, Warm), Spot(4, 5, Hot), Spot(6, 7, Hot))))
    )
    assertNotEquals(
      key(AttackComplexity.Polynomial(2, false), hotspot),
      key(AttackComplexity.Polynomial(3, false), hotspot)
    )

    // A part is in the spots as its character read first.
    val ir = ReDoS.parse("form.*edit_profile", "", Dialect.JavaScript)
    val quadratic = AttackComplexity.Polynomial(2, true)
    def partKey(partSize: Int)(spots: Spot*): AttackKey =
      AttackKey(quadratic, attack, Hotspot(spots), ir.readStartLocations(partSize), Seq.empty)

    val form = Seq(Spot(0, 4, Warm), Spot(4, 5, Hot))
    assertEquals(
      partKey(1)(form :+ Spot(6, 7, Warm)*),
      AttackKey(Some(2), Seq(Spot(0, 1, Warm), Spot(4, 5, Hot), Spot(6, 7, Warm)), Set.empty)
    )
    assertEquals(partKey(1)(form :+ Spot(6, 12, Warm)*), partKey(1)(form :+ Spot(6, 7, Warm)*))
    assertEquals(partKey(1)(form :+ Spot(6, 18, Warm)*), partKey(1)(form :+ Spot(6, 7, Warm)*))
    assertNotEquals(partKey(1)(form*), partKey(1)(form :+ Spot(6, 7, Warm)*))
    // With three characters of a part, the spots tell the first three characters read of `edit_profile` apart.
    assertNotEquals(partKey(3)(form :+ Spot(6, 8, Warm)*), partKey(3)(form :+ Spot(6, 7, Warm)*))
    assertNotEquals(partKey(3)(form :+ Spot(6, 9, Warm)*), partKey(3)(form :+ Spot(6, 8, Warm)*))
    assertEquals(partKey(3)(form :+ Spot(6, 12, Warm)*), partKey(3)(form :+ Spot(6, 9, Warm)*))
    // A spot over a part and the characters around it has each of them.
    val around = ReDoS.parse("\\dab\\d", "", Dialect.JavaScript).readStartLocations(1)
    assertEquals(
      AttackKey(quadratic, attack, Hotspot(Seq(Spot(0, 6, Warm))), around, Seq.empty),
      AttackKey(Some(2), Seq(Spot(0, 2, Warm), Spot(2, 3, Warm), Spot(4, 6, Warm)), Set.empty)
    )

    // The characteristics are the sets that have a character of the prefixes, the pumps and the suffix.
    val sets = Seq(CharSet.single('a'), CharSet.single('b'), CharSet.single('c'), CharSet.single('d'))
    def characteristicsOf(attack: AttackPattern): Set[Int] =
      AttackKey(quadratic, attack, Hotspot.empty, Set.empty, sets).characteristics

    assertEquals(characteristicsOf(attackOf("", "a", "", 10)), Set(0))
    assertEquals(characteristicsOf(attackOf("b", "aa", "d", 10)), Set(0, 1, 3))
    assertEquals(characteristicsOf(attackOf("", "x", "", 10)), Set.empty)
