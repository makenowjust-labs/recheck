package codes.quine.labo.redos
package fuzz

import scala.util.Random

import regexp.Parser

class FuzzCheckerSuite extends munit.FunSuite {

  /** A fixed seed random instance for deterministic test. */
  def random0: Random = new Random(0)

  /** Tests the pattern is vulnerable or not. */
  def check(source: String, flags: String, quick: Boolean = false): Boolean = {
    val result = for {
      pattern <- Parser.parse(source, flags)
      ctx <- FuzzContext.from(pattern)
    } yield FuzzChecker.check(
      ctx,
      random0,
      maxAttackSize = if (quick) 400 else 4000,
      seedLimit = if (quick) 1_00 else 1_000,
      populationLimit = if (quick) 1_000 else 10_000,
      attackLimit = if (quick) 10_000 else 100_000
    )
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
    assert(check("\\s*$", "", quick = true))
    assert(check("^a*aa*$", "", quick = true))
  }

  test("FuzzChecker.check: exponential") {
    assert(check("^(a|a)*$", "", quick = true))
    assert(check("^(a*)*$", "", quick = true))
    assert(check("^(a|b|ab)*$", "", quick = true))
    assert(check("^(a|B|Ab)*$", "i", quick = true))
    assert(check("^(aa|b|aab)*$", "", quick = true))

    assert(check("^(a?){50}a{50}$", ""))

    assert {
      val result = for {
        pattern <- Parser.parse("^(a|a)*$", "")
        ctx <- FuzzContext.from(pattern)
      } yield FuzzChecker.check(ctx, random0, populationLimit = 100, maxAttackSize = 5)
      result.get.isEmpty
    }
  }
}
