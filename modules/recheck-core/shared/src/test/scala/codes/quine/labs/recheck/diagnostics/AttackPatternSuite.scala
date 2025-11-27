package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.unicode.UString

class AttackPatternSuite extends munit.FunSuite:
  test("AttackPattern#fixedSize"):
    val attack = AttackPattern(Seq((UString("ab"), UString("cd"), 2)), UString("ef"), 1)
    assertEquals(attack.fixedSize, 4)

  test("AttackPattern#repeatSize"):
    val attack = AttackPattern(Seq((UString("ab"), UString("cd"), 2)), UString("ef"), 1)
    assertEquals(attack.repeatSize, 2)

  test("AttackPattern#adjust"):
    val attack = AttackPattern(Seq((UString("ab"), UString("cd"), 2)), UString("ef"), 1)
    assertEquals(attack.adjust(AttackComplexity.Polynomial(2, true), 100, 1000).n, 10)
    assertEquals(attack.adjust(AttackComplexity.Exponential(true), 100, 1000).n, 6)

  test("AttackPattern#asUString"):
    val attack = AttackPattern(Seq((UString("ab"), UString("cd"), 2)), UString("ef"), 1)
    assertEquals(attack.asUString, UString("abcdcdcdef"))

  test("AttackPattern#toString"):
    val attack1 = AttackPattern(Seq((UString("ab"), UString("cd"), 2)), UString("ef"), 1)
    assertEquals(attack1.toString(AttackPattern.JavaScript), "'ab' + 'cd'.repeat(3) + 'ef'")
    assertEquals(attack1.toString(AttackPattern.Superscript), "'ab' 'cd'³ 'ef'")
    val attack2 = AttackPattern(Seq((UString(""), UString("cd"), 2)), UString("ef"), 1)
    assertEquals(attack2.toString(AttackPattern.JavaScript), "'cd'.repeat(3) + 'ef'")
    assertEquals(attack2.toString(AttackPattern.Superscript), "'cd'³ 'ef'")
    val attack3 = AttackPattern(Seq((UString("ab"), UString("cd"), 2)), UString(""), 1)
    assertEquals(attack3.toString(AttackPattern.JavaScript), "'ab' + 'cd'.repeat(3)")
    assertEquals(attack3.toString(AttackPattern.Superscript), "'ab' 'cd'³")

    // The default style is `JavaScript`.
    assertEquals(attack1.toString, "'ab' + 'cd'.repeat(3) + 'ef'")
