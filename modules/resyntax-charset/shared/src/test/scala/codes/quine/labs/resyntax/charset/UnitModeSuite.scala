package codes.quine.labs.resyntax.charset

class UnitModeSuite extends munit.FunSuite:
  test("UnitMode#decode"):
    assertEquals(UnitMode.UTF16.decode(""), UString.empty)
    assertEquals(UnitMode.UTF16.decode("x🍣"), UString('x', 0xd83c, 0xdf63))
    assertEquals(UnitMode.UTF16.decode("\udf63"), UString(0xdf63))
    assertEquals(UnitMode.CodePoint.decode("aあ😀"), UString(0x61, 0x3042, 0x1f600))
    assertEquals(UnitMode.CodePoint.decode("\ud83d"), UString(0xd83d))
    assertEquals(UnitMode.Byte.decode("a\u00e9\u00ff"), UString(0x61, 0xe9, 0xff))
    intercept[IllegalArgumentException](UnitMode.Byte.decode("\u0100"))

  test("UnitMode#encode"):
    assertEquals(UnitMode.UTF16.encode(UString(0x61, 0xd83d, 0xde00, 0xd800)), "a😀\ud800")
    assertEquals(UnitMode.CodePoint.encode(UString(0x61, 0xe9, 0x3042, 0x1f600, 0xd800)), "aéあ😀\ud800")
    assertEquals(UnitMode.Byte.encode(UString(0x61, 0xc3, 0xa9)), "a\u00c3\u00a9")
    intercept[IllegalArgumentException](UnitMode.UTF16.encode(UString(0x1f600)))
    intercept[IllegalArgumentException](UnitMode.Byte.encode(UString(0x100)))
    for (mode, s) <- Seq(
        UnitMode.UTF16 -> UString(0x61, 0xd83d, 0xde00, 0xdc00),
        UnitMode.CodePoint -> UString(0x61, 0x1f600, 0xdc00, 0xd800),
        UnitMode.Byte -> UString(0x61, 0xc3, 0xa9, 0xff)
      )
    do assertEquals(mode.decode(mode.encode(s)), s)
    // A surrogate pair of code points is read back as one code point.
    assertEquals(UnitMode.CodePoint.decode(UnitMode.CodePoint.encode(UString(0xd83d, 0xde00))), UString(0x1f600))

  test("UnitMode#fromCodePoints"):
    val s = UString(0x61, 0xe9, 0x3042, 0x1f600)
    assertEquals(UnitMode.CodePoint.fromCodePoints(s), s)
    assertEquals(UnitMode.UTF16.fromCodePoints(s), UString(0x61, 0xe9, 0x3042, 0xd83d, 0xde00))
    assertEquals(
      UnitMode.Byte.fromCodePoints(s),
      UString(0x61, 0xc3, 0xa9, 0xe3, 0x81, 0x82, 0xf0, 0x9f, 0x98, 0x80)
    )
    assertEquals(UnitMode.Byte.fromCodePoints(UString(0xd800)), UString(0xed, 0xa0, 0x80))
