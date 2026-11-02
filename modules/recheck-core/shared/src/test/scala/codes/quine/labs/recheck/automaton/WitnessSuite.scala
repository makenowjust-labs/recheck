package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.resyntax.charset.UString

class WitnessSuite extends munit.FunSuite {
  test("Witness#buildAttackPattern") {
    assertEquals(
      Witness(Seq((Seq('a'.toInt), Seq('b'.toInt))), Seq('c'.toInt)).buildAttackPattern(2),
      AttackPattern(Seq((UString('a'), UString('b'), 0)), UString('c'), 2)
    )
  }

  test("Witness#repeatSize") {
    assertEquals(Witness(Seq((Seq(1), Seq(2))), Seq(3)).repeatSize, 1)
  }

  test("Witness#fixedSize") {
    assertEquals(Witness(Seq((Seq(1), Seq(2))), Seq(3)).fixedSize, 2)
  }
}
