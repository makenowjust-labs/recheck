package codes.quine.labo.recheck
package fuzz

import scala.util.Failure
import scala.util.Success

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.fuzz.Seeder._
import codes.quine.labo.recheck.regexp.Parser
import codes.quine.labo.recheck.unicode.IChar
import codes.quine.labo.recheck.unicode.UString

class SeederSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("Seeder.seed") {
    def seed(source: String, flags: String): Set[String] = {
      val result = for {
        pattern <- Parser.parse(source, flags) match {
          case Right(pattern) => Success(pattern)
          case Left(ex)       => Failure(new InvalidRegExpException(ex.getMessage))
        }
        fuzz <- FuzzProgram.from(pattern)
      } yield Seeder.seed(fuzz)
      result.get.map(_.toString)
    }

    assertEquals(seed("^(a|b)$", ""), Set("''", "'a'", "'b'"))
    assertEquals(seed("^(a|b)$", "i"), Set("''", "'A'", "'B'"))
    assertEquals(seed("^(A|B)$", "iu"), Set("''", "'a'", "'b'"))
    assertEquals(seed("^[ab]$", ""), Set("''", "'a'"))
    assertEquals(seed("^[^ab]$", ""), Set("''", "'\\x00'"))
    assertEquals(seed("^(a)\\1$", ""), Set("''", "'a'", "'aa'"))
    assertEquals(seed("^a*$", ""), Set("''", "'\\x00'", "'a'"))
    assertEquals(seed("^a{2}$", ""), Set("''", "'a'", "'aa'", "'a'²"))
    assertEquals(seed("^(a?){50}a{50}$", ""), Set("''", "'a'", "'aa'", "'a'²", "'aaa'", "'a'³"))
  }

  test("Seeder.Patch.InsertChar#apply") {
    assertEquals(
      Patch.InsertChar(1, Set(IChar('x'))).apply(UString("012"), false),
      Seq(UString("0x12"), UString("0x2"))
    )
  }

  test("Seeder.Patch.InsertString#apply") {
    assertEquals(
      Patch.InsertString(1, UString("xyz")).apply(UString("012"), false),
      Seq(UString("0xyz12"))
    )
  }
}
