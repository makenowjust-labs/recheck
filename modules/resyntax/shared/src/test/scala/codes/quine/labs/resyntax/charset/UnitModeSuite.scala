package codes.quine.labs.resyntax.charset

class UnitModeSuite extends munit.FunSuite:

  test("UnitMode#limit / max / contains / universe"):
    assertEquals(UnitMode.values.toSeq.map(_.limit), Seq(0x10000, 0x110000, 0x110000, 0x110000, 0x100))
    assertEquals(UnitMode.UTF16.max, 0xffff)
    assertEquals(UnitMode.CodePoint.max, 0x10ffff)
    assertEquals(UnitMode.Byte.max, 0xff)
    assert(UnitMode.Byte.contains(0xff))
    assert(!UnitMode.Byte.contains(0x100))
    assert(!UnitMode.CodePoint.contains(-1))
    assertEquals(UnitMode.UTF16.universe, CharSet.range(0, 0xffff))
    // Only `ScalarValue` has no surrogates.
    for mode <- Seq(UnitMode.UTF16, UnitMode.UTF16CodePoint, UnitMode.CodePoint) do
      assert(mode.contains(0xd800) && mode.contains(0xdfff), mode)
    assert(!UnitMode.ScalarValue.contains(0xd800) && !UnitMode.ScalarValue.contains(0xdfff))
    assert(UnitMode.ScalarValue.contains(0xd7ff) && UnitMode.ScalarValue.contains(0xe000))
    assertEquals(UnitMode.ScalarValue.universe, CharSet.range(0, 0x10ffff).diff(UTF16Encoding.Surrogates))
    assertEquals(UnitMode.UTF16CodePoint.universe, CharSet.range(0, 0x10ffff))

  test("UnitMode#isCodePoint"):
    assertEquals(
      UnitMode.values.toSeq.filter(_.isCodePoint),
      Seq(UnitMode.UTF16CodePoint, UnitMode.CodePoint, UnitMode.ScalarValue)
    )

  test("UnitMode#decode"):
    assertEquals(UnitMode.UTF16.decode(""), UString.empty)
    assertEquals(UnitMode.UTF16.decode("x🍣"), UString('x', 0xd83c, 0xdf63))
    assertEquals(UnitMode.UTF16.decode("\udf63"), UString(0xdf63))
    assertEquals(UnitMode.CodePoint.decode("aあ😀"), UString(0x61, 0x3042, 0x1f600))
    assertEquals(UnitMode.CodePoint.decode("\ud83d"), UString(0xd83d))
    for mode <- Seq(UnitMode.UTF16CodePoint, UnitMode.ScalarValue) do
      assertEquals(mode.decode("aあ😀"), UString(0x61, 0x3042, 0x1f600))
    assertEquals(UnitMode.UTF16CodePoint.decode("\ud83d"), UString(0xd83d))
    interceptMessage[IllegalArgumentException]("Invalid unit in ScalarValue mode: D83D"):
      UnitMode.ScalarValue.decode("a\ud83d")
    assertEquals(UnitMode.Byte.decode("a\u00e9\u00ff"), UString(0x61, 0xe9, 0xff))
    intercept[IllegalArgumentException](UnitMode.Byte.decode("\u0100"))

  test("UnitMode#encode"):
    assertEquals(UnitMode.UTF16.encode(UString(0x61, 0xd83d, 0xde00, 0xd800)), "a😀\ud800")
    assertEquals(UnitMode.CodePoint.encode(UString(0x61, 0xe9, 0x3042, 0x1f600, 0xd800)), "aéあ😀\ud800")
    assertEquals(UnitMode.Byte.encode(UString(0x61, 0xc3, 0xa9)), "a\u00c3\u00a9")
    intercept[IllegalArgumentException](UnitMode.UTF16.encode(UString(0x1f600)))
    intercept[IllegalArgumentException](UnitMode.Byte.encode(UString(0x100)))
    intercept[IllegalArgumentException](UnitMode.CodePoint.encode(UString(0x110000)))
    assertEquals(UnitMode.UTF16CodePoint.encode(UString(0x61, 0x1f600, 0xd800)), "a😀\ud800")
    assertEquals(UnitMode.ScalarValue.encode(UString(0x61, 0x1f600)), "a😀")
    intercept[IllegalArgumentException](UnitMode.ScalarValue.encode(UString(0xd800)))
    for (mode, s) <- Seq(
        UnitMode.UTF16 -> UString(0x61, 0xd83d, 0xde00, 0xdc00),
        UnitMode.UTF16CodePoint -> UString(0x61, 0x1f600, 0xdc00, 0xd800),
        UnitMode.CodePoint -> UString(0x61, 0x1f600, 0xdc00, 0xd800),
        UnitMode.ScalarValue -> UString(0x61, 0x1f600),
        UnitMode.Byte -> UString(0x61, 0xc3, 0xa9, 0xff)
      )
    do assertEquals(mode.decode(mode.encode(s)), s)
    // A surrogate pair of code points is read back as one code point.
    assertEquals(UnitMode.CodePoint.decode(UnitMode.CodePoint.encode(UString(0xd83d, 0xde00))), UString(0x1f600))

  test("UnitMode#fromCodePoints"):
    val s = IArray(0x61, 0xe9, 0x3042, 0x1f600)
    for mode <- Seq(UnitMode.UTF16CodePoint, UnitMode.CodePoint, UnitMode.ScalarValue) do
      assertEquals(mode.fromCodePoints(s), UString(0x61, 0xe9, 0x3042, 0x1f600))
    assertEquals(UnitMode.CodePoint.fromCodePoints(IArray(0xd800)), UString(0xd800))
    interceptMessage[IllegalArgumentException]("Invalid code point in ScalarValue mode: D800"):
      UnitMode.ScalarValue.fromCodePoints(IArray(0xd800))
    assertEquals(UnitMode.UTF16.fromCodePoints(s), UString(0x61, 0xe9, 0x3042, 0xd83d, 0xde00))
    assertEquals(
      UnitMode.Byte.fromCodePoints(s),
      UString(0x61, 0xc3, 0xa9, 0xe3, 0x81, 0x82, 0xf0, 0x9f, 0x98, 0x80)
    )
    for mode <- UnitMode.values do intercept[IllegalArgumentException](mode.fromCodePoints(IArray(0x110000)))
