package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.automaton.Complexity.*
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.unicode.UChar

class ComplexitySuite extends munit.FunSuite {
  test("Complexity#toAttackComplexity") {
    assertEquals(Constant.toAttackComplexity, AttackComplexity.Constant)
    assertEquals(Linear.toAttackComplexity, AttackComplexity.Linear)
    assertEquals(
      Polynomial(2, Witness(Seq((Seq(UChar('a')), Seq(UChar('a')))), Seq(UChar('a')))).toAttackComplexity,
      AttackComplexity.Polynomial(2, false)
    )
    assertEquals(
      Exponential(Witness(Seq((Seq(UChar('a')), Seq(UChar('a')))), Seq(UChar('a')))).toAttackComplexity,
      AttackComplexity.Exponential(false)
    )
  }

  test("Complexity.Vulnerable#buildAttackPattern") {
    val w = Witness(Seq((Seq(UChar('a')), Seq(UChar('a')))), Seq(UChar('\u0000')))
    assertEquals(Exponential(w).buildAttackPattern(100_000, 4_000).asUString.sizeAsString, 19)
    assertEquals(Polynomial(2, w).buildAttackPattern(100_000, 4_000).asUString.sizeAsString, 450)
  }
}
