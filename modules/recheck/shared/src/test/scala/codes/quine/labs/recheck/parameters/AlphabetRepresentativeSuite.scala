package codes.quine.labs.recheck.parameters

import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.PropertyTable

class AlphabetRepresentativeSuite extends munit.FunSuite:

  test("AlphabetRepresentative#of"):
    val set = CharSet.range(0x61, 0x7a).union(CharSet.range(0x400, 0x4ff))
    assertEquals(AlphabetRepresentative.NonLatin1.of(set), 0x400)
    assertEquals(AlphabetRepresentative.NonLatin1.of(CharSet.range(0x61, 0x7a)), 0x61)
    assertEquals(AlphabetRepresentative.Smallest.of(set), 0x61)

  test("AlphabetRepresentative#of: pretty"):
    def category(name: String): CharSet = PropertyTable.GeneralCategory.value(name).get.set
    def pretty(set: CharSet): Int = AlphabetRepresentative.Pretty.of(set)

    // The first one of the preferred characters that the set has: `あ`, `α`, `Ω`, `字`, `０`, `、`, `∆` and the
    // ideographic space.
    assertEquals(pretty(CharSet.range(0x00, 0x10ffff).diff(CharSet.of('a'))), 0x3042)
    assertEquals(pretty(category("Ll")), 0x03b1)
    assertEquals(pretty(category("Lu")), 0x03a9)
    assertEquals(pretty(CharSet.range(0x4e00, 0x9fff)), 0x5b57)
    assertEquals(pretty(category("Nd")), 0xff10)
    assertEquals(pretty(category("P")), 0x3001)
    assertEquals(pretty(category("Sm")), 0x2206)
    assertEquals(pretty(category("Zs")), 0x3000)

    // The smallest letter, number, punctuation or symbol outside Latin-1: U+3040 is not assigned.
    assertEquals(pretty(CharSet.range(0x3040, 0x3041)), 0x3041)
    // The smallest character outside Latin-1.
    assertEquals(pretty(CharSet.of(0x09, 0x20, 0x1680, 0x2028)), 0x1680)
    // The smallest printable ASCII character, with the space after the others.
    assertEquals(pretty(CharSet.range(0x00, 0x7f)), 0x21)
    assertEquals(pretty(CharSet.of(0x09, 0x20)), 0x20)
    // The smallest character.
    assertEquals(pretty(CharSet.range(0x00, 0x1f)), 0x00)

  test("AlphabetRepresentative#toString"):
    assertEquals(AlphabetRepresentative.Pretty.toString, "pretty")
    assertEquals(AlphabetRepresentative.NonLatin1.toString, "non-latin1")
    assertEquals(AlphabetRepresentative.Smallest.toString, "smallest")
