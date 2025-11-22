package codes.quine.labs.recheck.unicode

import scala.language.implicitConversions

import codes.quine.labs.recheck.unicode.IntervalSet.*
class ICharSuite extends munit.FunSuite:
  test("IChar.Any"):
    assertEquals(IChar.Any, IChar(IntervalSet((UChar(0), UChar(0x110000)))))

  test("IChar.Digit"):
    assert(IChar.Digit.contains(UChar('0')))
    assert(!IChar.Digit.contains(UChar('a')))

  test("IChar.Space"):
    assert(IChar.Space.contains(UChar('\n')))
    assert(IChar.Space.contains(UChar(' ')))
    assert(!IChar.Space.contains(UChar('a')))

  test("IChar.LineTerminator"):
    assert(IChar.LineTerminator.contains(UChar('\n')))
    assert(!IChar.LineTerminator.contains(UChar(' ')))

  test("IChar.Word"):
    assert(IChar.Word.contains(UChar('a')))
    assert(IChar.Word.contains(UChar('z')))
    assert(IChar.Word.contains(UChar('A')))
    assert(IChar.Word.contains(UChar('Z')))
    assert(IChar.Word.contains(UChar('0')))
    assert(IChar.Word.contains(UChar('9')))
    assert(IChar.Word.contains(UChar('_')))
    assert(!IChar.Word.contains(UChar('!')))

  test("IChar.UnicodeProperty"):
    assertEquals(IChar.UnicodeProperty("invalid"), None)
    assert(IChar.UnicodeProperty("ASCII").get.contains(UChar('A')))
    assert(!IChar.UnicodeProperty("ASCII").get.contains(UChar(0x80)))
    assert(IChar.UnicodeProperty("Letter").get.contains(UChar('A')))
    assert(IChar.UnicodeProperty("L").get.contains(UChar('A')))

  test("IChar.UnicodePropertyValue"):
    assertEquals(IChar.UnicodePropertyValue("invalid", "invalid"), None)
    assert(IChar.UnicodePropertyValue("General_Category", "N").get.contains(UChar('0')))
    assert(!IChar.UnicodePropertyValue("gc", "Number").get.contains(UChar('a')))
    assert(IChar.UnicodePropertyValue("Script", "Hira").get.contains(UChar('あ')))
    assert(IChar.UnicodePropertyValue("Script_Extensions", "Hira").get.contains(UChar('あ')))

  test("IChar.apply"):
    assertEquals(IChar('a'), IChar(IntervalSet((UChar(0x61), UChar(0x62)))))

  test("IChar.empty"):
    assert(IChar.empty.isEmpty)

  test("IChar.any"):
    assertEquals(IChar.any(false), IChar.Any16)
    assertEquals(IChar.any(true), IChar.Any)

  test("IChar.dot"):
    assertEquals(IChar.dot(false, true, false), IChar.Any16)
    assertEquals(IChar.dot(false, true, true), IChar.Any)
    assertEquals(IChar.dot(false, false, false), IChar.Any16.diff(IChar.LineTerminator))
    assertEquals(IChar.dot(false, false, true), IChar.Any.diff(IChar.LineTerminator))
    assertEquals(IChar.dot(true, true, false), IChar.canonicalize(IChar.Any16, false))
    assertEquals(IChar.dot(true, true, true), IChar.canonicalize(IChar.Any, true))
    assertEquals(IChar.dot(true, false, false), IChar.canonicalize(IChar.Any16.diff(IChar.LineTerminator), false))
    assertEquals(IChar.dot(true, false, true), IChar.canonicalize(IChar.Any.diff(IChar.LineTerminator), true))

  test("IChar.range"):
    assertEquals(IChar.range('a', 'a'), IChar('a'))
    assertEquals(IChar.range('z', 'a'), IChar.empty)
    assertEquals(IChar.range('a', 'z'), IChar(IntervalSet((UChar('a'), UChar('z' + 1)))))

  test("IChar.union"):
    assertEquals(IChar.union(Seq(IChar('a'), IChar('z'))), IChar('a').union(IChar('z')))
    assertEquals(IChar.union(Seq.empty), IChar.empty)

  test("IChar.canonicalize"):
    assertEquals(IChar.canonicalize(IChar('n'), false), IChar('N'))
    assertEquals(IChar.canonicalize(IChar('N'), true), IChar('n'))

    // U+FF21: FULLWIDTH LATIN CAPITAL LETTER A
    // U+FF41: FULLWIDTH LATIN SMALL LETTER A
    assertEquals(IChar.canonicalize(IChar('\uff41'), false), IChar('\uff21'))
    assertEquals(IChar.canonicalize(IChar('\uff21'), true), IChar('\uff41'))

    // U+017F: LATIN SMALL LETTER LONG S
    assertEquals(IChar.canonicalize(IChar('\u017f'), false), IChar('\u017f'))
    assertEquals(IChar.canonicalize(IChar('\u017f'), true), IChar('s'))

  test("IChar#isEmpty"):
    assert(IChar(IntervalSet.empty[UChar]).isEmpty)
    assert(!IChar(IntervalSet((UChar(0x41), UChar(0x42)))).isEmpty)

  test("IChar#nonEmpty"):
    assert(!IChar(IntervalSet.empty[UChar]).nonEmpty)
    assert(IChar(IntervalSet((UChar(0x41), UChar(0x42)))).nonEmpty)

  test("IChar#complement"):
    assertEquals(IChar('a').complement(true), IChar(IntervalSet((UChar(0), UChar('a')), (UChar('b'), UChar(0x110000)))))
    assertEquals(IChar('a').complement(false), IChar(IntervalSet((UChar(0), UChar('a')), (UChar('b'), UChar(0x10000)))))
    assert(IChar.Any.complement(true).isEmpty)
    assert(IChar.Any16.complement(false).isEmpty)

  test("IChar#union"):
    assertEquals(IChar('a') union IChar('b'), IChar(IntervalSet((UChar('a'), UChar('c')))))

  test("IChar#partition"):
    assertEquals(
      IChar(IntervalSet((UChar(0x41), UChar(0x42))))
        .partition(IChar(IntervalSet((UChar(0x41), UChar(0x5b))))),
      Partition(
        IChar(IntervalSet((UChar(0x41), UChar(0x42)))),
        IChar(IntervalSet.empty[UChar]),
        IChar(IntervalSet((UChar(0x42), UChar(0x5b))))
      )
    )

  test("IChar#diff"):
    assertEquals(IChar.range('a', 'c').diff(IChar('a')), IChar.range('b', 'c'))

  test("IChar#contains"):
    assert(IChar.range('a', 'z').contains(UChar('a')))
    assert(!IChar.range('a', 'z').contains(UChar('A')))

  test("IChar#head"):
    assertEquals(IChar('A').head, UChar('A'))
    assertEquals(IChar.range('a', 'z').head, UChar('a'))
    intercept[NoSuchElementException](IChar.empty.head)

  test("IChar#compare"):
    val c1 = IChar(IntervalSet((UChar(0x41), UChar(0x42))))
    val c2 = IChar(IntervalSet((UChar(0x42), UChar(0x43))))
    assertEquals(c1.compare(c1), 0)
    assertEquals(c1.compare(c2), -1)
    assertEquals(c2.compare(c1), 1)

  test("IChar#toString"):
    assertEquals(
      IChar(IntervalSet((UChar('a'), UChar('b')), (UChar('A'), UChar('Z' + 1)))).toString,
      "[A-Za]"
    )
    assertEquals(
      IChar('\u0001').union(IChar('-')).union(IChar(UChar(0x10ffff))).toString,
      "[\\cA\\-\\u{10FFFF}]"
    )
