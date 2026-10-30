package codes.quine.labs.resyntax.charset

class UStringSuite extends munit.FunSuite:
  test("UString.fromString"):
    assertEquals(UString.fromString("aあ😀"), UString(0x61, 0x3042, 0x1f600))
    assertEquals(UString.fromString("\ud83d"), UString(0xd83d))
    assertEquals(UString.fromUtf16Units("😀"), UString(0xd83d, 0xde00))

  test("UString#encode"):
    val s = UString(0x61, 0xe9, 0x3042, 0x1f600)
    assertEquals(s.encode(UnitMode.CodePoint).toSeq, Seq(0x61, 0xe9, 0x3042, 0x1f600))
    assertEquals(s.encode(UnitMode.Utf16).toSeq, Seq(0x61, 0xe9, 0x3042, 0xd83d, 0xde00))
    assertEquals(
      s.encode(UnitMode.Byte).toSeq,
      Seq(0x61, 0xc3, 0xa9, 0xe3, 0x81, 0x82, 0xf0, 0x9f, 0x98, 0x80)
    )
    assertEquals(s.asString, "aéあ😀")

  test("UString basic operations"):
    val s = UString(1, 2, 3)
    assertEquals(s.length, 3)
    assertEquals(s(1), 2)
    assert(UString.empty.isEmpty)
    assertEquals(s ++ UString(4), UString(1, 2, 3, 4))
    assertEquals(s.slice(1, 3), UString(2, 3))
    assertEquals(s.map(_ * 2), UString(2, 4, 6))
    assertEquals(UString.from(Iterator(1, 2)), UString(1, 2))
    assertEquals(s.toString, "UString(U+1 U+2 U+3)")

  test("UString#compare"):
    assert(UString(1, 2) < UString(1, 3))
    assert(UString(1, 2) < UString(1, 2, 0))
    assert(UString(2) > UString(1, 9))
    assertEquals(UString(1, 2).compare(UString(1, 2)), 0)
