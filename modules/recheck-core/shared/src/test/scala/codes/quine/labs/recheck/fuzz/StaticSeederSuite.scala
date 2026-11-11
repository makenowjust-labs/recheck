package codes.quine.labs.recheck.fuzz

import scala.util.Try

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.resyntax.dialect.Dialect

class StaticSeederSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  test("StaticSeeder.seed"):
    def seed(source: String, flags: String, dialect: Dialect = Dialect.JavaScript): Set[String] =
      val result =
        for ir <- Try(ReDoS.parse(source, flags, dialect))
        yield StaticSeeder.seed(ir, limit = 25000, maxSimpleRepeatSize = 10, maxNFASize = Int.MaxValue)
      result.get.map(_.toString)

    assertEquals(seed("^a+a+$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    assertEquals(seed("^(a+)\\1$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    assertEquals(
      seed("^(?<foo>a+)\\k<foo>$", ""),
      Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'")
    )
    assertEquals(seed("^a+(?=a+)$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    // A look-behind is a look-ahead of its inner expression, which reads the input forward.
    assertEquals(seed("^a+(?<=a+)$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    // The look-arounds may nest.
    assertEquals(seed("^a+(?<=a(?=a+))$", ""), seed("^a+(?=a(?=a+))$", ""))
    assert(seed("^a+(?=a(?<=a+))$", "").nonEmpty)

    // A back-reference in a referred group being copied matches the empty string.
    assertEquals(seed("^(a+\\1)$", ""), seed("^(a+(?:a+|))$", ""))
    assertEquals(seed("^(?<x>a+\\k<x>)$", ""), seed("^(?<x>a+(?:a+|))$", ""))
    assertEquals(seed("^(a+\\2)(b+\\1)$", ""), seed("^(a+(?:b+(?:a+|)|))(b+(?:a+(?:b+|)|))$", ""))
    assertEquals(seed("^a{11,}a{11,}$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))

    assertEquals(seed("^(fizz)*(fizz)*$", ""), Set("'fizz'.repeat(112) + 'i'"))
    // An atomic group is a choice of its alternatives.
    assertEquals(seed("^(?>a+)a+$", "", Dialect.PCRE), seed("^a+a+$", "", Dialect.PCRE))

    // A dead suffix is not `,`, which lets the look-ahead succeed and the match end.
    assertEquals(seed("^a+a+(?=,)", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))

    assertEquals(seed("^a(?=a)$", ""), Set.empty[String])
    assertEquals(seed("^a(?<=a)$", ""), Set.empty[String])

    // A dead suffix after a high surrogate is not a low surrogate, which the engine reads together with it.
    assertEquals(
      seed("^(?:\\uD800|\\uDC00)*(?:\\uD800|\\uDC00)*$", "u"),
      Set(
        "'\\uD800' + '\\uD800'.repeat(224) + '\\x00'",
        "'\\uD800' + '\\uD800'.repeat(15) + '\\x00'",
        "'\\uDC00' + '\\uDC00'.repeat(224) + '\\x00'",
        "'\\uDC00' + '\\uDC00'.repeat(15) + '\\x00'"
      )
    )
