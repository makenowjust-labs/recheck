package codes.quine.labs.resyntax.charset

import codes.quine.labs.resyntax.charset.UString.fromUTF16Units

class UStringSuite extends munit.FunSuite:
  test("UString.fromString"):
    assertEquals(UString.fromString("aあ😀"), UString(0x61, 0x3042, 0x1f600))
    assertEquals(UString.fromString("\ud83d"), UString(0xd83d))

  test("UString.fromUTF16Units"):
    assertEquals(fromUTF16Units(""), UString.empty)
    assertEquals(fromUTF16Units("x🍣"), UString('x', 0xd83c, 0xdf63))
    assertEquals(fromUTF16Units("\udf63"), UString(0xdf63))

  test("UString#encode"):
    val s = UString(0x61, 0xe9, 0x3042, 0x1f600)
    assertEquals(s.encode(UnitMode.CodePoint).toSeq, Seq(0x61, 0xe9, 0x3042, 0x1f600))
    assertEquals(s.encode(UnitMode.UTF16).toSeq, Seq(0x61, 0xe9, 0x3042, 0xd83d, 0xde00))
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

  test("UString#getAt"):
    val s = UString(0x78, 0x1f363, 0xd83c)
    assertEquals(s.getAt(0), Some(0x78))
    assertEquals(s.getAt(1), Some(0x1f363))
    assertEquals(s.getAt(2), Some(0xd83c))
    assertEquals(s.getAt(-1), None)
    assertEquals(s.getAt(3), None)

  test("UString#getBefore"):
    val s = UString(0x78, 0x1f363, 0xd83c)
    assertEquals(s.getBefore(1), Some(0x78))
    assertEquals(s.getBefore(2), Some(0x1f363))
    assertEquals(s.getBefore(3), Some(0xd83c))
    assertEquals(s.getBefore(0), None)
    assertEquals(s.getBefore(4), None)

  test("UString#insert"):
    val s = fromUTF16Units("012")
    assertEquals(s.insert(0, fromUTF16Units("xyz")), fromUTF16Units("xyz012"))
    assertEquals(s.insert(1, fromUTF16Units("xyz")), fromUTF16Units("0xyz12"))
    assertEquals(s.insert(2, fromUTF16Units("xyz")), fromUTF16Units("01xyz2"))
    assertEquals(s.insert(3, fromUTF16Units("xyz")), fromUTF16Units("012xyz"))
