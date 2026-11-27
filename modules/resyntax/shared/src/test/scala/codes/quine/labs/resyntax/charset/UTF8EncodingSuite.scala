package codes.quine.labs.resyntax.charset

class UTF8EncodingSuite extends munit.FunSuite:

  test("UTF8Encoding.encode"):
    assertEquals(UTF8Encoding.encode(0x61), Seq(0x61))
    assertEquals(UTF8Encoding.encode(0xe9), Seq(0xc3, 0xa9))
    assertEquals(UTF8Encoding.encode(0x3042), Seq(0xe3, 0x81, 0x82))
    assertEquals(UTF8Encoding.encode(0x1f600), Seq(0xf0, 0x9f, 0x98, 0x80))
    assertEquals(UTF8Encoding.encode(0xd800), Seq(0xed, 0xa0, 0x80))
    intercept[IllegalArgumentException](UTF8Encoding.encode(0x110000))
    intercept[IllegalArgumentException](UTF8Encoding.encode(-1))

  test("UTF8Encoding.decode"):
    for c <- Seq(0x00, 0x61, 0x7f, 0x80, 0xe9, 0x7ff, 0x800, 0x3042, 0xd800, 0xffff, 0x10000, 0x1f600, 0x10ffff) do
      assertEquals(UTF8Encoding.decode(UTF8Encoding.encode(c)), c)

  test("UTF8Encoding.sequenceLength"):
    assertEquals(UTF8Encoding.sequenceLength(0x61), 1)
    assertEquals(UTF8Encoding.sequenceLength(0x80), 0)
    assertEquals(UTF8Encoding.sequenceLength(0xc1), 0)
    assertEquals(UTF8Encoding.sequenceLength(0xc3), 2)
    assertEquals(UTF8Encoding.sequenceLength(0xe3), 3)
    assertEquals(UTF8Encoding.sequenceLength(0xf0), 4)
    assertEquals(UTF8Encoding.sequenceLength(0xf5), 0)

  test("UTF8Encoding.isContinuation"):
    assert(UTF8Encoding.isContinuation(0x80))
    assert(UTF8Encoding.isContinuation(0xbf))
    assert(!UTF8Encoding.isContinuation(0x61))
    assert(!UTF8Encoding.isContinuation(0xc3))
