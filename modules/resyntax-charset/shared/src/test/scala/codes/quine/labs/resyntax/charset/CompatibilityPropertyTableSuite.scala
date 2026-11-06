package codes.quine.labs.resyntax.charset

import codes.quine.labs.resyntax.charset.CompatibilityPropertyTable.*

class CompatibilityPropertyTableSuite extends munit.FunSuite:

  def assertMembers(set: CharSet, members: Seq[Int], nonMembers: Seq[Int])(using munit.Location): Unit =
    for c <- members do assert(set.contains(c), f"U+$c%04X")
    for c <- nonMembers do assert(!set.contains(c), f"U+$c%04X")

  test("CompatibilityPropertyTable: single properties"):
    assertEquals(Alpha, PropertyTable.binary("Alphabetic").get.set)
    assertEquals(Lower, PropertyTable.binary("Lowercase").get.set)
    assertEquals(Upper, PropertyTable.binary("Uppercase").get.set)
    assertEquals(Punct, PropertyTable.GeneralCategory.value("P").get.set)
    assertEquals(Digit, PropertyTable.GeneralCategory.value("Nd").get.set)
    assertEquals(Space, PropertyTable.binary("White_Space").get.set)
    assertEquals(Cntrl, PropertyTable.GeneralCategory.value("Cc").get.set)

  test("CompatibilityPropertyTable: combined properties"):
    // U+FF21 is a fullwidth `A`, U+0660 an Arabic-Indic digit, U+2160 a Roman numeral (`Nl` and Alphabetic).
    assertMembers(XDigit, Seq('0', 'F', 'f', 0xff21, 0x0660), Seq('G', 'g'))
    assertMembers(Alnum, Seq('a', '0', 0x0660, 0x2160), Seq('_', '-'))
    assertMembers(Blank, Seq('\t', ' ', 0x00a0, 0x3000), Seq('\n', '\r', 0x0085, 0x2028))
    // U+00AD is a soft hyphen (`Cf`), U+0378 is unassigned, and U+D800 is a surrogate.
    assertMembers(Graph, Seq('a', '!', 0x00ad), Seq(' ', '\t', 0x00a0, 0x0378, 0xd800))
    assertMembers(Print, Seq('a', ' ', 0x3000), Seq('\t', '\n', 0x0378))
    // U+0301, U+0903 and U+20DD are marks (`Mn`, `Mc`, `Me`), and U+200C and U+200D are Join_Control.
    assertMembers(Word, Seq('a', '_', '0', 0x0301, 0x0903, 0x20dd, 0x2160, 0x200c, 0x200d), Seq('-', ' ', 0x00b2))

  test("CompatibilityPropertyTable.lookup"):
    val sets = Seq(
      "alpha" -> Alpha,
      "lower" -> Lower,
      "upper" -> Upper,
      "punct" -> Punct,
      "digit" -> Digit,
      "xdigit" -> XDigit,
      "alnum" -> Alnum,
      "space" -> Space,
      "blank" -> Blank,
      "cntrl" -> Cntrl,
      "graph" -> Graph,
      "print" -> Print,
      "word" -> Word
    )
    for (name, set) <- sets do assertEquals(lookup(name), Some(set), name)
    assertEquals(lookup("ascii"), None)
    assertEquals(lookup("Alpha"), None)
