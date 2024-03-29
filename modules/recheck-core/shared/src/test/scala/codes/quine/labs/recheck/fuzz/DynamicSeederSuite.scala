package codes.quine.labs.recheck
package fuzz

import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.fuzz.DynamicSeeder._
import codes.quine.labs.recheck.regexp.Parser
import codes.quine.labs.recheck.unicode.IChar
import codes.quine.labs.recheck.unicode.UString

class DynamicSeederSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("DynamicSeeder.seed") {
    def seed(source: String, flags: String): Set[String] = {
      val result = for {
        pattern <- Parser.parse(source, flags) match {
          case Right(pattern) => Success(pattern)
          case Left(ex)       => Failure(new InvalidRegExpException(ex.getMessage))
        }
        fuzz <- FuzzProgram.from(pattern)
      } yield DynamicSeeder.seed(fuzz, timeout = Duration.Inf)
      result.get.map(_.toString)
    }

    assertEquals(seed("^(a|b)$", ""), Set("''", "'a'", "'b'"))
    assertEquals(seed("^(a|b)$", "i"), Set("''", "'A'", "'B'"))
    assertEquals(seed("^(A|B)$", "iu"), Set("''", "'a'", "'b'"))
    assertEquals(seed("^[ab]$", ""), Set("''", "'a'"))
    assertEquals(seed("^[^ab]$", ""), Set("''", "'\\x00'"))
    assertEquals(seed("^(a)\\1$", ""), Set("''", "'a'", "'aa'"))
    assertEquals(seed("^a*$", ""), Set("''", "'\\x00'", "'a'"))
    assertEquals(seed("^a{2}$", ""), Set("''", "'a'", "'aa'", "'a'.repeat(2)"))
    assertEquals(seed("^(a?){50}a{50}$", ""), Set("''", "'a'", "'aa'", "'a'.repeat(2)"))
  }

  test("DynamicSeeder.Patch.InsertChar#apply") {
    assertEquals(
      Patch.InsertChar(1, Set(IChar('x'))).apply(UString("012"), false),
      Seq(UString("0x12"), UString("0x2"))
    )
  }

  test("DynamicSeeder.Patch.InsertString#apply") {
    assertEquals(
      Patch.InsertString(1, UString("xyz")).apply(UString("012"), false),
      Seq(UString("0xyz12"))
    )
  }
}
