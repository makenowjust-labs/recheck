package codes.quine.labo.recheck.fuzz

import scala.util.Failure
import scala.util.Success

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.regexp.Parser

class StaticSeederSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("StaticSeeder.seed") {
    def seed(source: String, flags: String): Set[String] = {
      val result = for {
        pattern <- Parser.parse(source, flags) match {
          case Right(pattern) => Success(pattern)
          case Left(ex)       => Failure(new InvalidRegExpException(ex.getMessage))
        }
      } yield StaticSeeder.seed(pattern, limit = 25000, maxSimpleRepeatSize = 10)
      result.get.map(_.toString)
    }

    assertEquals(seed("^a+a+$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    assertEquals(seed("^(a+)\\1$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    assertEquals(
      seed("^(?<foo>a+)\\k<foo>$", ""),
      Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'")
    )
    assertEquals(seed("^a+(?=a+)$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    assertEquals(seed("^a+(?<=a+)$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    assertEquals(seed("^a{11,}a{11,}$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))

    assertEquals(seed("^(fizz)*(fizz)*$", ""), Set("'fizz'.repeat(112) + 'i'"))
  }
}
