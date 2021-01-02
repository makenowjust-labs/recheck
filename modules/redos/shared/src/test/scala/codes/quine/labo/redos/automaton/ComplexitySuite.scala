package codes.quine.labo.redos
package automaton

import Complexity._
import data.IChar

class ComplexitySuite extends munit.FunSuite {
  test("Complexity#isSafe") {
    assert(Constant.isSafe)
    assert(Linear.isSafe)
    assert(!Polynomial(2, Witness(Seq((Seq(IChar('a')), Seq(IChar('a')))), Seq(IChar('a')))).isSafe)
    assert(!Exponential(Witness(Seq((Seq(IChar('a')), Seq(IChar('a')))), Seq(IChar('a')))).isSafe)
  }

  test("Complexity.Vulnerable#buildAttack") {
    val w = Witness(Seq((Seq(IChar('a')), Seq(IChar('a')))), Seq(IChar('\u0000')))
    assertEquals(Exponential(w).buildAttack(1_000_000, 1.5, 10_000).size, 22)
    assertEquals(Polynomial(2, w).buildAttack(1_000_000, 1.5, 10_000).size, 819)
  }
}
