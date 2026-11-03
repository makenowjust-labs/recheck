package codes.quine.labs.resyntax.charset

class UTF8Suite extends munit.FunSuite:
  test("UTF8.encode"):
    assertEquals(UTF8.encode(0x61), Seq(0x61))
    assertEquals(UTF8.encode(0xe9), Seq(0xc3, 0xa9))
    assertEquals(UTF8.encode(0x3042), Seq(0xe3, 0x81, 0x82))
    assertEquals(UTF8.encode(0x1f600), Seq(0xf0, 0x9f, 0x98, 0x80))
    assertEquals(UTF8.encode(0xd800), Seq(0xed, 0xa0, 0x80))

  test("UTF8.decode"):
    for c <- Seq(0x00, 0x61, 0x7f, 0x80, 0xe9, 0x7ff, 0x800, 0x3042, 0xd800, 0xffff, 0x10000, 0x1f600, 0x10ffff) do
      assertEquals(UTF8.decode(UTF8.encode(c)), c)

  test("UTF8.sequenceLength"):
    assertEquals(UTF8.sequenceLength(0x61), 1)
    assertEquals(UTF8.sequenceLength(0x80), 0)
    assertEquals(UTF8.sequenceLength(0xc1), 0)
    assertEquals(UTF8.sequenceLength(0xc3), 2)
    assertEquals(UTF8.sequenceLength(0xe3), 3)
    assertEquals(UTF8.sequenceLength(0xf0), 4)
    assertEquals(UTF8.sequenceLength(0xf5), 0)

  test("UTF8.isContinuation"):
    assert(UTF8.isContinuation(0x80))
    assert(UTF8.isContinuation(0xbf))
    assert(!UTF8.isContinuation(0x61))
    assert(!UTF8.isContinuation(0xc3))
