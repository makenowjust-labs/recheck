package codes.quine.labs.recheck
package diagnostics

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
    assertEquals(attack.adjust(AttackComplexity.Polynomial(2, true), 100, 1000).n, 10)
    assertEquals(attack.adjust(AttackComplexity.Exponential(true), 100, 1000).n, 6)

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
