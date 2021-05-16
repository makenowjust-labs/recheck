package codes.quine.labo.recheck
package fuzz

import scala.util.Failure
import scala.util.Random
import scala.util.Success

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.regexp.Parser

class FuzzCheckerSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  /** A fixed seed random instance for deterministic test. */
  def random0: Random = new Random(0)

  /** Tests the pattern is vulnerable or not. */
  def check(source: String, flags: String, quick: Boolean = false): Boolean = {
    val result = for {
      pattern <- Parser.parse(source, flags) match {
        case Right(pattern) => Success(pattern)
        case Left(message)  => Failure(new InvalidRegExpException(message))
      }
      fuzz <- FuzzProgram.from(pattern)
    } yield FuzzChecker.check(
      fuzz,
      random0,
      maxAttackSize = if (quick) 400 else 4000,
      seedLimit = if (quick) 1_00 else 1_000,
      incubationLimit = if (quick) 1_000 else 10_000,
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

    assert(check("^(a?){50}a{50}$", "", quick = true))

    // The checker can find an attack string on seeding phase.
    assert {
      val result = for {
        pattern <- Parser.parse("^(a?){50}a{50}$", "") match {
          case Right(pattern) => Success(pattern)
          case Left(message)  => Failure(new InvalidRegExpException(message))
        }
        fuzz <- FuzzProgram.from(pattern)
      } yield FuzzChecker.check(fuzz, random0, seedLimit = 10000, incubationLimit = 10000, attackLimit = 10000)
      result.get.isDefined
    }

    // The checker cannot find too small attack string.
    assert {
      val result = for {
        pattern <- Parser.parse("^(a|a)*$", "") match {
          case Right(pattern) => Success(pattern)
          case Left(message)  => Failure(new InvalidRegExpException(message))
        }
        fuzz <- FuzzProgram.from(pattern)
      } yield FuzzChecker.check(fuzz, random0, incubationLimit = 100, attackLimit = 100, maxAttackSize = 5)
      result.get.isEmpty
    }
  }
}
