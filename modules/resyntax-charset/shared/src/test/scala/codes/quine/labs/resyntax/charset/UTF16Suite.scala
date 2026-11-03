package codes.quine.labs.resyntax.charset

class UTF16Suite extends munit.FunSuite:
  test("UTF16.isHighSurrogate"):
    assert(UTF16.isHighSurrogate(0xd800))
    assert(UTF16.isHighSurrogate(0xdbff))
    assert(!UTF16.isHighSurrogate(0xdc00))
    assert(!UTF16.isHighSurrogate(0x61))

  test("UTF16.isLowSurrogate"):
    assert(UTF16.isLowSurrogate(0xdc00))
    assert(UTF16.isLowSurrogate(0xdfff))
    assert(!UTF16.isLowSurrogate(0xdbff))
    assert(!UTF16.isLowSurrogate(0xe000))

  test("UTF16.isSurrogate"):
    assert(UTF16.isSurrogate(0xd800))
    assert(UTF16.isSurrogate(0xdfff))
    assert(!UTF16.isSurrogate(0xd7ff))
    assert(!UTF16.isSurrogate(0xe000))

  test("UTF16.encode"):
    assertEquals(UTF16.encode(0x61), Seq(0x61))
    assertEquals(UTF16.encode(0xffff), Seq(0xffff))
    assertEquals(UTF16.encode(0xd800), Seq(0xd800))
    assertEquals(UTF16.encode(0x1f600), Seq(0xd83d, 0xde00))
    assertEquals(UTF16.encode(0x10ffff), Seq(0xdbff, 0xdfff))

  test("UTF16.decodeSurrogatePair"):
    assertEquals(UTF16.decodeSurrogatePair(0xd83d, 0xde00), 0x1f600)
    assertEquals(UTF16.decodeSurrogatePair(0xd800, 0xdc00), 0x10000)
    assertEquals(UTF16.decodeSurrogatePair(0xdbff, 0xdfff), 0x10ffff)
