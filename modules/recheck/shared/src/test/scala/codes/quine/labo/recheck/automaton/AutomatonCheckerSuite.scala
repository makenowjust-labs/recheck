package codes.quine.labo.recheck
package automaton

import scala.util.Success
import scala.util.Try

import codes.quine.labo.recheck.automaton.Complexity._
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.IChar
import codes.quine.labo.recheck.diagnostics.Hotspot
import codes.quine.labo.recheck.diagnostics.Hotspot._
import codes.quine.labo.recheck.regexp.Parser

class AutomatonCheckerSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  /** Runs a checker against the RegExp. */
  def check(source: String, flags: String): Try[Complexity[IChar]] =
    for {
      pattern <- Parser.parse(source, flags)
      epsNFA <- EpsNFACompiler.compile(pattern)
      nfa = epsNFA.toOrderedNFA.rename
      result <- Try(AutomatonChecker.check(nfa))
    } yield result

  test("AutomatonChecker.check: constant") {
    assertEquals(check("^$", ""), Success(Constant))
    assertEquals(check("^foo$", ""), Success(Constant))
    assertEquals(check("^((fi|bu)z{2}){1,2}$", ""), Success(Constant))
  }

  test("AutomatonChecker.check: linear") {
    assertEquals(check("a*", ""), Success(Linear))
    assertEquals(check("(a*)*", ""), Success(Linear))
    assertEquals(check("^([a:]|\\b)*$", ""), Success(Linear))
    assertEquals(check("^(\\w|\\W)*$", "i"), Success(Linear))
    assertEquals(check("^(a()*a)*$", ""), Success(Linear))
    assertEquals(check("^(a(\\b)*:)*$", ""), Success(Linear))
  }

  test("AutomatonChecker.check: polynomial") {
    val a = IChar('a')
    val other = IChar('a').complement(false)
    assertEquals(
      check("^.*a.*a$", "s"),
      Success(
        Polynomial(2, Witness(Seq((Seq(a), Seq(a))), Seq(other)), Hotspot(Seq(Spot(1, 2, Heat), Spot(3, 5, Heat))))
      )
    )
    assertEquals(
      check("^(.a)*a(.a)*a$", "s"),
      Success(
        Polynomial(
          2,
          Witness(Seq((Seq(a, a), Seq(a, a))), Seq(a, other, a, a, a)),
          Hotspot(Seq(Spot(2, 4, Heat), Spot(6, 7, Heat), Spot(8, 10, Heat)))
        )
      )
    )
    assertEquals(
      check("^.*a.*a.*a$", "s"),
      Success(
        Polynomial(
          3,
          Witness(Seq((Seq(other), Seq(a)), (Seq.empty, Seq(a))), Seq(other)),
          Hotspot(Seq(Spot(1, 2, Heat), Spot(3, 5, Heat), Spot(6, 8, Heat)))
        )
      )
    )
  }

  test("AutomatonChecker.check: exponential") {
    val a = IChar('a')
    val b = IChar('b')
    val other1 = IChar('a').complement(false)
    val other2 = IChar.union(Seq(a, b)).complement(false)
    assertEquals(
      check("^(a|a)*$", ""),
      Success(
        Exponential(Witness(Seq((Seq(a), Seq(a))), Seq(other1)), Hotspot(Seq(Spot(2, 3, Heat), Spot(4, 5, Heat))))
      )
    )
    assertEquals(
      check("^((a)*)*$", ""),
      Success(Exponential(Witness(Seq((Seq(a), Seq(a))), Seq(other1)), Hotspot(Seq(Spot(3, 4, Heat)))))
    )
    assertEquals(
      check("^(a|b|ab)*$", ""),
      Success(
        Exponential(
          Witness(Seq((Seq(a), Seq(a, b))), Seq(other2)),
          Hotspot(Seq(Spot(2, 3, Heat), Spot(4, 5, Heat), Spot(6, 8, Heat)))
        )
      )
    )
    assertEquals(
      check("^(aa|b|aab)*$", ""),
      Success(
        Exponential(
          Witness(Seq((Seq(a, a), Seq(b, a, a, b, a, a))), Seq(other2)),
          Hotspot(Seq(Spot(2, 4, Heat), Spot(5, 6, Heat), Spot(7, 10, Heat)))
        )
      )
    )
  }
}
