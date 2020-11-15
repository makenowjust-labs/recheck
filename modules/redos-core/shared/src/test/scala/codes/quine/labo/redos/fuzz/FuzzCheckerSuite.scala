package codes.quine.labo.redos
package fuzz

import scala.util.Random

import backtrack.IRCompiler
import regexp.Parser

class FuzzCheckerSuite extends munit.FunSuite {

  /** A fixed seed random instance for deterministic test. */
  def random0: Random = new Random(0)

  /** Tests the pattern is vulnerable or not. */
  def check(source: String, flags: String, maxAttackSize: Int = 10_000): Boolean = {
    val result = for {
      pattern <- Parser.parse(source, flags)
      alphabet <- pattern.alphabet
      ir <- IRCompiler.compile(pattern)
    } yield FuzzChecker.check(ir, alphabet, Seeder.seed(ir, alphabet), random0, maxAttackSize = maxAttackSize)
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
    assert(!check("^(a|a)*$", "", maxAttackSize = 5))
    assert(check("^(a*)*$", ""))
    assert(check("^(a?){50}a{50}$", ""))
    assert(check("^(a|b|ab)*$", ""))
    assert(check("^(a|B|Ab)*$", "i"))
    assert(check("^(aa|b|aab)*$", ""))
  }
}
