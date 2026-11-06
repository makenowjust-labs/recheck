package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class WitnessSuite extends munit.FunSuite {
  test("Witness#buildAttackPattern") {
    assertEquals(
      Witness(Seq((Seq('a'.toInt), Seq('b'.toInt))), Seq('c'.toInt)).buildAttackPattern(2, UnitMode.UTF16),
      AttackPattern(Seq((UString('a'), UString('b'), 0)), UString('c'), 2)
    )
    intercept[UnexpectedException](
      Witness(Seq((Seq(0xd800), Seq(0xdc00))), Seq.empty).buildAttackPattern(2, UnitMode.UTF16CodePoint)
    )
  }

  test("Witness#repeatSize") {
    assertEquals(Witness(Seq((Seq(1), Seq(2))), Seq(3)).repeatSize, 1)
  }

  test("Witness#fixedSize") {
    assertEquals(Witness(Seq((Seq(1), Seq(2))), Seq(3)).fixedSize, 2)
  }
}
