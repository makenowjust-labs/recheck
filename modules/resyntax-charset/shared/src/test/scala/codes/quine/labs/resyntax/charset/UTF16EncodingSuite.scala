package codes.quine.labs.resyntax.charset

class UTF16EncodingSuite extends munit.FunSuite:

  test("UTF16Encoding.isHighSurrogate"):
    assert(UTF16Encoding.isHighSurrogate(0xd800))
    assert(UTF16Encoding.isHighSurrogate(0xdbff))
    assert(!UTF16Encoding.isHighSurrogate(0xdc00))
    assert(!UTF16Encoding.isHighSurrogate(0x61))

  test("UTF16Encoding.isLowSurrogate"):
    assert(UTF16Encoding.isLowSurrogate(0xdc00))
    assert(UTF16Encoding.isLowSurrogate(0xdfff))
    assert(!UTF16Encoding.isLowSurrogate(0xdbff))
    assert(!UTF16Encoding.isLowSurrogate(0xe000))

  test("UTF16Encoding.isSurrogate"):
    assert(UTF16Encoding.isSurrogate(0xd800))
    assert(UTF16Encoding.isSurrogate(0xdfff))
    assert(!UTF16Encoding.isSurrogate(0xd7ff))
    assert(!UTF16Encoding.isSurrogate(0xe000))

  test("UTF16Encoding.encode"):
    assertEquals(UTF16Encoding.encode(0x61), Seq(0x61))
    assertEquals(UTF16Encoding.encode(0xffff), Seq(0xffff))
    assertEquals(UTF16Encoding.encode(0xd800), Seq(0xd800))
    assertEquals(UTF16Encoding.encode(0x1f600), Seq(0xd83d, 0xde00))
    assertEquals(UTF16Encoding.encode(0x10ffff), Seq(0xdbff, 0xdfff))
    intercept[IllegalArgumentException](UTF16Encoding.encode(0x110000))
    intercept[IllegalArgumentException](UTF16Encoding.encode(-1))

  test("UTF16Encoding.decodeSurrogatePair"):
    assertEquals(UTF16Encoding.decodeSurrogatePair(0xd83d, 0xde00), 0x1f600)
    assertEquals(UTF16Encoding.decodeSurrogatePair(0xd800, 0xdc00), 0x10000)
    assertEquals(UTF16Encoding.decodeSurrogatePair(0xdbff, 0xdfff), 0x10ffff)
