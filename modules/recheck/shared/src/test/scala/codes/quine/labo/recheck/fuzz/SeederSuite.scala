package codes.quine.labo.recheck
package fuzz
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.IChar
import codes.quine.labo.recheck.data.UString
import codes.quine.labo.recheck.fuzz.Seeder._
import codes.quine.labo.recheck.regexp.Parser

class SeederSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("Seeder.seed") {
    def seed(source: String, flags: String): Set[String] = {
      val result = for {
        pattern <- Parser.parse(source, flags)
        fuzz <- FuzzProgram.from(pattern)
      } yield Seeder.seed(fuzz)
      result.get.map(_.toString)
    }

    assertEquals(seed("^(a|b)$", ""), Set("''", "'a'", "'b'"))
    assertEquals(seed("^(a|b)$", "i"), Set("''", "'A'", "'B'"))
    assertEquals(seed("^(A|B)$", "iu"), Set("''", "'a'", "'b'"))
    assertEquals(seed("^a*$", ""), Set("''", "'\\x00'", "'a'"))
    assertEquals(seed("^a{2}$", ""), Set("''", "'a'", "'aa'", "'a'²"))
    assertEquals(seed("^(a?){50}a{50}$", ""), Set("''", "'a'", "'aa'", "'a'²", "'aaa'", "'a'³"))
  }

  test("Seeder.Patch.InsertChar#apply") {
    assertEquals(
      Patch.InsertChar(1, Set(IChar('x'))).apply(UString.from("012", false)),
      Seq(UString.from("0x12", false), UString.from("0x2", false))
    )
  }

  test("Seeder.Patch.InsertString#apply") {
    assertEquals(
      Patch.InsertString(1, UString.from("xyz", false)).apply(UString.from("012", false)),
      Seq(UString.from("0xyz12", false))
    )
  }
}
