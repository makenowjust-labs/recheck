package codes.quine.labo.redos

import scala.util.Success

import data.IChar
import Checker._

class CheckerSuite extends munit.FunSuite {
  test("Checker.check: constant") {
    assertEquals(Checker.check("^foo$", ""), Success(Complexity.Constant))
    assertEquals(Checker.check("^((fi|bu)z{2}){1,2}$", ""), Success(Complexity.Constant))
  }

  test("Checker.check: linear") {
    assertEquals(Checker.check("a*", ""), Success(Complexity.Linear))
    assertEquals(Checker.check("(a*)*", ""), Success(Complexity.Linear))
  }

  test("Checker.check: polynomial") {
    val a = IChar('a')
    val other = IChar('a').complement(false)
    assertEquals(
      Checker.check("^.*a.*a$", "s"),
      Success(Complexity.Polynomial(2, Witness(Seq((Seq(a), Seq(a))), Seq(other))))
    )
    assertEquals(
      Checker.check("^(.a)*a(.a)*a$", "s"),
      Success(Complexity.Polynomial(2, Witness(Seq((Seq(other, a), Seq(a, a))), Seq(a, a, a))))
    )
    assertEquals(
      Checker.check("^.*a.*a.*a$", "s"),
      Success(Complexity.Polynomial(3, Witness(Seq((Seq(a), Seq(a)), (Seq.empty, Seq(a))), Seq(other))))
    )
  }

  test("Checker.check: exponential") {
    val a = IChar('a')
    val b = IChar('b')
    val other1 = IChar('a').complement(false)
    val other2 = IChar.union(Seq(a, b)).complement(false)
    assertEquals(
      Checker.check("^(a|a)*$", ""),
      Success(Complexity.Exponential(Witness(Seq((Seq(a), Seq(a))), Seq(other1))))
    )
    assertEquals(
      Checker.check("^((a)*)*$", ""),
      Success(Complexity.Exponential(Witness(Seq((Seq(a), Seq(a))), Seq(other1))))
    )
    assertEquals(
      Checker.check("^(a|b|ab)*$", ""),
      Success(Complexity.Exponential(Witness(Seq((Seq(b), Seq(b, a))), Seq(other2))))
    )
  }
}
