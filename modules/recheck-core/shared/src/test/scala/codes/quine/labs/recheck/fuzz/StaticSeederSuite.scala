package codes.quine.labs.recheck.fuzz

import codes.quine.labs.recheck.SourcePattern
import codes.quine.labs.recheck.common.Context

class StaticSeederSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  test("StaticSeeder.seed"):
    def seed(source: String, flags: String): Set[String] =
      val result =
        for (pattern, ir) <- SourcePattern.parse(source, flags).toTry
        yield StaticSeeder.seed(ir, pattern.preferredChars, limit = 25000, maxSimpleRepeatSize = 10)
      result.get.map(_.toString)

    assertEquals(seed("^a+a+$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    assertEquals(seed("^(a+)\\1$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    assertEquals(
      seed("^(?<foo>a+)\\k<foo>$", ""),
      Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'")
    )
    assertEquals(seed("^a+(?=a+)$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))
    assertEquals(seed("^a+(?<=a+)$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))

    // A back-reference to an enclosing capture, or to one being expanded, matches the empty string.
    assertEquals(seed("^(a+\\1)$", ""), seed("^(a+)$", ""))
    assertEquals(seed("^(?<x>a+\\k<x>)$", ""), seed("^(?<x>a+)$", ""))
    assertEquals(seed("^(a+\\2)(b+\\1)$", ""), seed("^(a+(?:b+|))(b+(?:a+|))$", ""))
    assertEquals(seed("^a{11,}a{11,}$", ""), Set("'a' + 'a'.repeat(224) + '\\x00'", "'a' + 'a'.repeat(15) + '\\x00'"))

    assertEquals(seed("^(fizz)*(fizz)*$", ""), Set("'fizz'.repeat(112) + 'i'"))

    assertEquals(seed("^a(?=a)$", ""), Set.empty[String])
    assertEquals(seed("^a(?<=a)$", ""), Set.empty[String])
