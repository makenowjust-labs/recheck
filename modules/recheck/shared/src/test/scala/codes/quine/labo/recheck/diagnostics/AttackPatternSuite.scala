package codes.quine.labo.recheck
package diagnostics

import codes.quine.labo.recheck.data.UString

class AttackPatternSuite extends munit.FunSuite {
  test("AttackPattern#asUString") {
    val attack = AttackPattern(
      Seq((UString.from("ab", false), UString.from("cd", false), 2)),
      UString.from("ef", false),
      1
    )
    assertEquals(attack.asUString, UString.from("abcdcdcdef", false))
  }

  test("AttackPattern#toString") {
    val attack = AttackPattern(
      Seq((UString.from("ab", false), UString.from("cd", false), 2)),
      UString.from("ef", false),
      1
    )
    assertEquals(attack.toString, "'ab' 'cd'³ 'ef'")
  }
}
