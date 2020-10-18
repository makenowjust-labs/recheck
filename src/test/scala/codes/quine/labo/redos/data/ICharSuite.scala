package codes.quine.labo.redos.data

import minitest.SimpleTestSuite

import IntervalSet._

object ICharSuite extends SimpleTestSuite {
  test("IChar.Any") {
    assertEquals(IChar.Any, IChar(IntervalSet((UChar(0), UChar(0x110000))), false, false))
  }

  test("IChar.Digit") {
    assert(IChar.Digit.contains(UChar('0')))
    assert(!IChar.Digit.contains(UChar('a')))
  }

  test("IChar.Space") {
    assert(IChar.Space.contains(UChar('\n')))
    assert(IChar.Space.contains(UChar(' ')))
    assert(!IChar.Space.contains(UChar('a')))
  }

  test("IChar.LineTerminator") {
    assert(IChar.LineTerminator.contains(UChar('\n')))
    assert(!IChar.LineTerminator.contains(UChar(' ')))
  }

  test("IChar.Word") {
    assert(IChar.Word.contains(UChar('a')))
    assert(IChar.Word.contains(UChar('0')))
    assert(!IChar.Word.contains(UChar('!')))
  }

  test("IChar.UnicodeProperty") {
    assertEquals(IChar.UnicodeProperty("invalid"), None)
    assert(IChar.UnicodeProperty("ASCII").get.contains(UChar('A')))
    assert(!IChar.UnicodeProperty("ASCII").get.contains(UChar(0x80)))
    assert(IChar.UnicodeProperty("Letter").get.contains(UChar('A')))
    assert(IChar.UnicodeProperty("L").get.contains(UChar('A')))
  }

  test("IChar.UnicodePropertyValue") {
    assertEquals(IChar.UnicodePropertyValue("invalid", "invalid"), None)
    assert(IChar.UnicodePropertyValue("General_Category", "N").get.contains(UChar('0')))
    assert(!IChar.UnicodePropertyValue("gc", "Number").get.contains(UChar('a')))
    assert(IChar.UnicodePropertyValue("Script", "Hira").get.contains(UChar('あ')))
    assert(IChar.UnicodePropertyValue("Script_Extensions", "Hira").get.contains(UChar('あ')))
  }

  test("IChar.apply") {
    assertEquals(IChar('a'), IChar(IntervalSet((UChar(0x61), UChar(0x62))), false, false))
    assertEquals(IChar(UChar('a')), IChar(IntervalSet((UChar(0x61), UChar(0x62))), false, false))
  }

  test("IChar.empty") {
    assert(IChar.empty.isEmpty)
  }

  test("IChar.range") {
    assertEquals(IChar.range(UChar('a'), UChar('a')), IChar('a'))
    assertEquals(IChar.range(UChar('z'), UChar('a')), IChar.empty)
    assertEquals(IChar.range(UChar('a'), UChar('z')), IChar(IntervalSet((UChar('a'), UChar('z' + 1)))))
  }

  test("IChar.canonicalize") {
    assertEquals(IChar.canonicalize(IChar('n'), false), IChar('N'))
    assertEquals(IChar.canonicalize(IChar('N'), true), IChar('n'))

    // U+FF21: FULLWIDTH LATIN CAPITAL LETTER A
    // U+FF41: FULLWIDTH LATIN SMALL LETTER A
    assertEquals(IChar.canonicalize(IChar(0xff41), false), IChar(0xff21))
    assertEquals(IChar.canonicalize(IChar(0xff21), true), IChar(0xff41))

    // U+017F: LATIN SMALL LETTER LONG S
    assertEquals(IChar.canonicalize(IChar(0x017f), false), IChar(0x017f))
    assertEquals(IChar.canonicalize(IChar(0x017f), true), IChar('s'))
  }

  test("IChar#isEmpty") {
    assert(IChar(IntervalSet.empty, false, false).isEmpty)
    assert(!IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false).isEmpty)
  }

  test("IChar#nonEmpty") {
    assert(!IChar(IntervalSet.empty, false, false).nonEmpty)
    assert(IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false).nonEmpty)
  }

  test("IChar#complement") {
    assertEquals(IChar('a').complement, IChar(IntervalSet((UChar(0), UChar('a')), (UChar('b'), UChar(0x110000)))))
    assert(IChar.Any.complement.isEmpty)
  }

  test("IChar#union") {
    assertEquals(IChar('a') union IChar('b'), IChar(IntervalSet((UChar('a'), UChar('c'))), false, false))
  }

  test("IChar#partition") {
    assertEquals(
      IChar(IntervalSet((UChar(0x41), UChar(0x42))), true, false)
        .partition(IChar(IntervalSet((UChar(0x41), UChar(0x5b))), false, true)),
      Partition(
        IChar(IntervalSet((UChar(0x41), UChar(0x42))), true, true),
        IChar(IntervalSet.empty, true, false),
        IChar(IntervalSet((UChar(0x42), UChar(0x5b))), false, true)
      )
    )
  }

  test("IChar#compare") {
    val c1 = IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false)
    val c2 = IChar(IntervalSet((UChar(0x42), UChar(0x43))), false, false)
    assertEquals(c1.compare(c1), 0)
    assertEquals(c1.compare(c2), -1)
    assertEquals(c2.compare(c1), 1)
  }
}
