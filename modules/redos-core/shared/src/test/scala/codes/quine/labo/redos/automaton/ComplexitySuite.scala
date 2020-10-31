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
}
