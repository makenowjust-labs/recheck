package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class AttackPatternSuite extends munit.FunSuite:

  test("AttackPattern#fixedSize"):
    val attack = AttackPattern(
      Seq((UnitMode.UTF16.decode("ab"), UnitMode.UTF16.decode("cd"), 2)),
      UnitMode.UTF16.decode("ef"),
      1
    )
    assertEquals(attack.fixedSize, 4)

  test("AttackPattern#repeatSize"):
    val attack = AttackPattern(
      Seq((UnitMode.UTF16.decode("ab"), UnitMode.UTF16.decode("cd"), 2)),
      UnitMode.UTF16.decode("ef"),
      1
    )
    assertEquals(attack.repeatSize, 2)

  test("AttackPattern#adjust"):
    val attack = AttackPattern(
      Seq((UnitMode.UTF16.decode("ab"), UnitMode.UTF16.decode("cd"), 2)),
      UnitMode.UTF16.decode("ef"),
      1
    )
    assertEquals(attack.adjust(AttackComplexity.Polynomial(2, true), 100, 1000, UnitMode.UTF16).n, 10)
    assertEquals(attack.adjust(AttackComplexity.Exponential(true), 100, 1000, UnitMode.UTF16).n, 6)
    val paired = AttackPattern(Seq((UString(0xd800), UString(0xdc00), 0)), UString.empty, 1)
    intercept[UnexpectedException](
      paired.adjust(AttackComplexity.Exponential(true), 100, 1000, UnitMode.UTF16CodePoint)
    )

  test("AttackPattern#admitsAnyRepeatCount"):
    val (high, low, a) = (UString(0xd800), UString(0xdc00), UString('a'))

    def attack(pumps: (UString, UString)*)(suffix: UString): AttackPattern =
      AttackPattern(pumps.map((s, t) => (s, t, 0)), suffix, 2)

    val paired = Seq(
      // In a part.
      attack()(high ++ low),
      attack((a, high ++ low))(UString.empty),
      // From a prefix to its pump.
      attack((high, low))(UString.empty),
      // From a pump to itself.
      attack((UString.empty, low ++ a ++ high))(UString.empty),
      // From a pump to the next part.
      attack((a, high), (low, a))(UString.empty),
      attack((a, high))(low),
      // From a prefix to the next part, when the pump repeats no times.
      attack((high, a), (low, a))(UString.empty),
      attack((high, a))(low),
      attack((high, a), (UString.empty, a))(low),
      attack((high, a), (UString.empty, low))(UString.empty)
    )
    for p <- paired do
      assert(!p.admitsAnyRepeatCount(UnitMode.UTF16CodePoint), p)
      for mode <- Seq(UnitMode.UTF16, UnitMode.CodePoint) do assert(p.admitsAnyRepeatCount(mode), p)
    // A low surrogate before a high surrogate, or one after another unit, is a lone surrogate.
    assert(attack((low, a ++ high))(a ++ low).admitsAnyRepeatCount(UnitMode.UTF16CodePoint))
    assert(attack((high ++ a, low))(high ++ a).admitsAnyRepeatCount(UnitMode.UTF16CodePoint))

  test("AttackPattern.build"):
    val pumps = Seq((UString(0xd800), UString('a'), 0))
    assertEquals(
      AttackPattern.build(pumps, UString(0xdc00), 1, UnitMode.CodePoint),
      AttackPattern(pumps, UString(0xdc00), 1)
    )
    interceptMessage[UnexpectedException](
      "The attack string has a high surrogate followed by a low surrogate in UTF16CodePoint mode"
    )(AttackPattern.build(pumps, UString(0xdc00), 1, UnitMode.UTF16CodePoint))

  test("AttackPattern#asUString"):
    val attack = AttackPattern(
      Seq((UnitMode.UTF16.decode("ab"), UnitMode.UTF16.decode("cd"), 2)),
      UnitMode.UTF16.decode("ef"),
      1
    )
    assertEquals(attack.asUString, UnitMode.UTF16.decode("abcdcdcdef"))

  test("AttackPattern#toString"):
    val attack1 = AttackPattern(
      Seq((UnitMode.UTF16.decode("ab"), UnitMode.UTF16.decode("cd"), 2)),
      UnitMode.UTF16.decode("ef"),
      1
    )
    assertEquals(attack1.toString(AttackPatternStyle.JavaScript(unicode = false)), "'ab' + 'cd'.repeat(3) + 'ef'")
    val attack2 =
      AttackPattern(Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("cd"), 2)), UnitMode.UTF16.decode("ef"), 1)
    assertEquals(attack2.toString(AttackPatternStyle.JavaScript(unicode = false)), "'cd'.repeat(3) + 'ef'")
    val attack3 =
      AttackPattern(Seq((UnitMode.UTF16.decode("ab"), UnitMode.UTF16.decode("cd"), 2)), UnitMode.UTF16.decode(""), 1)
    assertEquals(attack3.toString(AttackPatternStyle.JavaScript(unicode = false)), "'ab' + 'cd'.repeat(3)")

    // The default style is `JavaScript`.
    assertEquals(attack1.toString, "'ab' + 'cd'.repeat(3) + 'ef'")
    assertEquals(AttackPattern(Seq.empty, UString.empty, 0).toString, "''")
