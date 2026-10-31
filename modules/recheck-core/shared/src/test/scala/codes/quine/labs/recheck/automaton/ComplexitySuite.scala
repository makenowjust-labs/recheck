package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.automaton.Complexity.*
import codes.quine.labs.recheck.diagnostics.AttackComplexity

class ComplexitySuite extends munit.FunSuite {
  test("Complexity#toAttackComplexity") {
    assertEquals(Constant.toAttackComplexity, AttackComplexity.Constant)
    assertEquals(Linear.toAttackComplexity, AttackComplexity.Linear)
    assertEquals(
      Polynomial(2, Witness(Seq((Seq('a'.toInt), Seq('a'.toInt))), Seq('a'.toInt))).toAttackComplexity,
      AttackComplexity.Polynomial(2, false)
    )
    assertEquals(
      Exponential(Witness(Seq((Seq('a'.toInt), Seq('a'.toInt))), Seq('a'.toInt))).toAttackComplexity,
      AttackComplexity.Exponential(false)
    )
  }

  test("Complexity.Vulnerable#buildAttackPattern") {
    val w = Witness(Seq((Seq('a'.toInt), Seq('a'.toInt))), Seq('\u0000'.toInt))
    assertEquals(Exponential(w).buildAttackPattern(100_000, 4_000).asUString.length, 19)
    assertEquals(Polynomial(2, w).buildAttackPattern(100_000, 4_000).asUString.length, 450)
  }
}
