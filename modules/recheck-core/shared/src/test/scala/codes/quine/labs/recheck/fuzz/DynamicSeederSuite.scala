package codes.quine.labs.recheck
package fuzz

import scala.concurrent.duration.*
import scala.language.implicitConversions
import scala.util.Try

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.fuzz.DynamicSeeder.*
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.CharSet

class DynamicSeederSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  test("DynamicSeeder.seed"):
    def seed(source: String, flags: String): Set[String] =
      val result = for
        ir <- Try(ReDoS.parse(source, flags, Dialect.JavaScript))
        fuzz <- FuzzProgram.from(ir)
      yield DynamicSeeder.seed(fuzz, timeout = Duration.Inf)
      result.get.map(_.toString)

    assertEquals(seed("^(a|b)$", ""), Set("''", "'a'", "'b'"))
    assertEquals(seed("^(a|b)$", "i"), Set("''", "'A'", "'B'"))
    assertEquals(seed("^(A|B)$", "iu"), Set("''", "'A'", "'B'"))
    assertEquals(seed("^[ab]$", ""), Set("''", "'a'"))
    assertEquals(seed("^[^ab]$", ""), Set("''", "'\\x00'"))
    assertEquals(seed("^(a)\\1$", ""), Set("''", "'a'", "'aa'"))
    assertEquals(seed("^a*$", ""), Set("''", "'\\x00'", "'a'"))
    assertEquals(seed("^a{2}$", ""), Set("''", "'a'", "'aa'", "'a'.repeat(2)"))
    assertEquals(seed("^(a?){50}a{50}$", ""), Set("''", "'a'", "'aa'", "'a'.repeat(2)"))

  test("DynamicSeeder.Patch.InsertChar#apply"):
    assertEquals(
      Patch.InsertChar(1, Set(CharSet.single('x'))).apply(UStringUtil.fromString("012"), false),
      Seq(UStringUtil.fromString("0x12"), UStringUtil.fromString("0x2"))
    )

  test("DynamicSeeder.Patch.InsertString#apply"):
    assertEquals(
      Patch.InsertString(1, UStringUtil.fromString("xyz")).apply(UStringUtil.fromString("012"), false),
      Seq(UStringUtil.fromString("0xyz12"))
    )
