package codes.quine.labo.re
package unicode

import minitest.SimpleTestSuite

import data.UChar

object PropertySuite extends SimpleTestSuite {
  test("Property.binary") {
    assertEquals(Property.binary("invalid"), None)
    val set = Property.binary("AHex")
    assert(set.isDefined)
    assert(set.get.contains(UChar('a')))
    assert(!set.get.contains(UChar('z')))
  }

  test("Property.binary: ASCII") {
    val set = Property.binary("ASCII")
    assert(set.isDefined)
    assert(set.get.contains(UChar('a')))
    assert(!set.get.contains(UChar('あ')))
  }

  test("Property.binary: Any") {
    val set = Property.binary("Any")
    assert(set.isDefined)
    assert(set.get.contains(UChar('a')))
    assert(!set.get.contains(UChar(0x110000)))
  }

  test("Property.binary: Assigned") {
    val set = Property.binary("Assigned")
    assert(set.isDefined)
    assert(set.get.contains(UChar('a')))
    assert(!set.get.contains(UChar(0x10ffff)))
  }

  test("Property.generalCategory") {
    assertEquals(Property.generalCategory("invalid"), None)
    val set = Property.generalCategory("Lowercase_Letter")
    assert(set.isDefined)
    assert(set.get.contains(UChar('a')))
    assert(!set.get.contains(UChar('A')))
  }

  test("Property.script") {
    assertEquals(Property.script("invalid"), None)
    val set = Property.script("Hira")
    assert(set.isDefined)
    assert(!set.get.contains(UChar('a')))
    assert(set.get.contains(UChar('あ')))
    assert(!set.get.contains(UChar(0x30fc))) // U+30FC: KATAKANA-HIRAGANA PROLONGED SOUND MARK
  }

  test("Property.scriptExtensions") {
    assertEquals(Property.scriptExtensions("invalid"), None)
    val set = Property.scriptExtensions("Hira")
    assert(set.isDefined)
    assert(!set.get.contains(UChar('a')))
    assert(set.get.contains(UChar('あ')))
    assert(set.get.contains(UChar(0x30fc))) // U+30FC: KATAKANA-HIRAGANA PROLONGED SOUND MARK
  }
}
