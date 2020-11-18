package codes.quine.labo.redos
package fuzz

import scala.util.Random

import regexp.Parser

class FuzzCheckerSuite extends munit.FunSuite {

  /** A fixed seed random instance for deterministic test. */
  def random0: Random = new Random(0)

  /** Tests the pattern is vulnerable or not. */
  def check(source: String, flags: String): Boolean = {
    val result = for {
      pattern <- Parser.parse(source, flags)
      ctx <- FuzzContext.from(pattern)
    } yield FuzzChecker.check(ctx, random0)
    result.get.isDefined
  }

  test("FuzzChecker.check: constant") {
    assert(!check("^foo$", ""))
    assert(!check("^(foo|bar)$", ""))
    assert(!check("^(fiz{2}|buz{2){1,2}$", ""))
  }

  test("FuzzChecker.check: linear") {
    assert(!check("(a|a)*", ""))
    assert(!check("(a*)*", ""))
  }

  test("FuzzChecker.check: polynomial") {
    assert(check("\\s*$", ""))
    assert(check("^a*aa*$", ""))
  }

  test("FuzzChecker.check: exponential") {
    assert(check("^(a|a)*$", ""))
    assert(check("^(a*)*$", ""))
    assert(check("^(a?){50}a{50}$", ""))
    assert(check("^(a|b|ab)*$", ""))
    assert(check("^(a|B|Ab)*$", "i"))
    assert(check("^(aa|b|aab)*$", ""))

    assert {
      val result = for {
        pattern <- Parser.parse("^(a|a)*$", "")
        ctx <- FuzzContext.from(pattern)
      } yield FuzzChecker.check(ctx, random0, populationLimit = 100, maxAttackSize = 5)
      result.get.isEmpty
    }
  }
}
