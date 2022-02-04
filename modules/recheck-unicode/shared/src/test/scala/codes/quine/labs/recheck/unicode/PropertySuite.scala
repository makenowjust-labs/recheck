package codes.quine.labs.recheck.unicode

class PropertySuite extends munit.FunSuite {
  test("Property.binary") {
    assertEquals(Property.binary("invalid"), None)
    val set = Property.binary("AHex")
    assert(set.isDefined)
    assert(set.get.contains(UChar('a')))
    assert(!set.get.contains(UChar('z')))

    for (name <- Property.BinaryPropertyNames)
      assert(Property.binary(name).isDefined)
    for (name <- Property.BinaryPropertyAliases.keys)
      assert(Property.binary(name).isDefined)
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

    for (value <- Property.GeneralCategoryValues)
      assert(Property.generalCategory(value).isDefined)
    for (value <- Property.GeneralCategoryValueGroups.keys)
      assert(Property.generalCategory(value).isDefined)
    for (value <- Property.GeneralCategoryValueAliases.keys)
      assert(Property.generalCategory(value).isDefined)
  }

  test("Property.script") {
    assertEquals(Property.script("invalid"), None)
    val set = Property.script("Hira")
    assert(set.isDefined)
    assert(!set.get.contains(UChar('a')))
    assert(set.get.contains(UChar('あ')))
    assert(!set.get.contains(UChar(0x30fc))) // U+30FC: KATAKANA-HIRAGANA PROLONGED SOUND MARK

    for (value <- Property.ScriptValues)
      assert(Property.script(value).isDefined)
    for (value <- Property.ScriptValueAliases.keys)
      assert(Property.script(value).isDefined)
  }

  test("Property.scriptExtensions") {
    assertEquals(Property.scriptExtensions("invalid"), None)
    val set = Property.scriptExtensions("Hira")
    assert(set.isDefined)
    assert(!set.get.contains(UChar('a')))
    assert(set.get.contains(UChar('あ')))
    assert(set.get.contains(UChar(0x30fc))) // U+30FC: KATAKANA-HIRAGANA PROLONGED SOUND MARK

    for (value <- Property.ScriptValues)
      assert(Property.scriptExtensions(value).isDefined)
    for (value <- Property.ScriptValueAliases.keys)
      assert(Property.scriptExtensions(value).isDefined)
  }
}
