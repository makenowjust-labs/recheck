package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.util.UStringUtil

class AttackPatternSuite extends munit.FunSuite:
  test("AttackPattern#fixedSize"):
    val attack = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack.fixedSize, 4)

  test("AttackPattern#repeatSize"):
    val attack = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack.repeatSize, 2)

  test("AttackPattern#adjust"):
    val attack = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack.adjust(AttackComplexity.Polynomial(2, true), 100, 1000).n, 10)
    assertEquals(attack.adjust(AttackComplexity.Exponential(true), 100, 1000).n, 6)

  test("AttackPattern#asUString"):
    val attack = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack.asUString, UStringUtil.fromString("abcdcdcdef"))

  test("AttackPattern#toString"):
    val attack1 = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack1.toString(AttackPattern.JavaScript), "'ab' + 'cd'.repeat(3) + 'ef'")
    assertEquals(attack1.toString(AttackPattern.Superscript), "'ab' 'cd'³ 'ef'")
    val attack2 =
      AttackPattern(Seq((UStringUtil.fromString(""), UStringUtil.fromString("cd"), 2)), UStringUtil.fromString("ef"), 1)
    assertEquals(attack2.toString(AttackPattern.JavaScript), "'cd'.repeat(3) + 'ef'")
    assertEquals(attack2.toString(AttackPattern.Superscript), "'cd'³ 'ef'")
    val attack3 =
      AttackPattern(Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)), UStringUtil.fromString(""), 1)
    assertEquals(attack3.toString(AttackPattern.JavaScript), "'ab' + 'cd'.repeat(3)")
    assertEquals(attack3.toString(AttackPattern.Superscript), "'ab' 'cd'³")

    // The default style is `JavaScript`.
    assertEquals(attack1.toString, "'ab' + 'cd'.repeat(3) + 'ef'")
