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

  test("UString.fromCodePointsAsUTF16"):
    assertEquals(UString.fromCodePointsAsUTF16(Seq.empty), UString.empty)
    assertEquals(UString.fromCodePointsAsUTF16("xyz".map(_.toInt)), fromUTF16Units("xyz"))
    assertEquals(UString.fromCodePointsAsUTF16(Seq(0x1f363)), fromUTF16Units("🍣"))

  test("UString.charWidth"):
    assertEquals(UString.charWidth('x'), 1)
    assertEquals(UString.charWidth(0xffff), 1)
    assertEquals(UString.charWidth(0x1f363), 2)

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
    val s1 = fromUTF16Units("xyz")
    val s2 = fromUTF16Units("x🍣z")
    assertEquals(s1.getAt(0, false), Some('x'.toInt))
    assertEquals(s1.getAt(1, false), Some('y'.toInt))
    assertEquals(s1.getAt(2, false), Some('z'.toInt))
    assertEquals(s1.getAt(-1, false), None)
    assertEquals(s1.getAt(3, false), None)
    assertEquals(s2.getAt(1, false), Some(0xd83c))
    assertEquals(s2.getAt(1, true), Some(0x1f363))
    assertEquals(s2.getAt(2, true), Some(0xdf63))

  test("UString#getBefore"):
    val s1 = fromUTF16Units("xyz")
    val s2 = fromUTF16Units("x🍣z")
    assertEquals(s1.getBefore(1, false), Some('x'.toInt))
    assertEquals(s1.getBefore(2, false), Some('y'.toInt))
    assertEquals(s1.getBefore(3, false), Some('z'.toInt))
    assertEquals(s1.getBefore(0, false), None)
    assertEquals(s1.getBefore(4, false), None)
    assertEquals(s2.getBefore(2, false), Some(0xd83c))
    assertEquals(s2.getBefore(2, true), Some(0xd83c))
    assertEquals(s2.getBefore(3, true), Some(0x1f363))

  test("UString#chars"):
    val s = fromUTF16Units("0🍣4")
    assertEquals(s.chars(false).toSeq, Seq('0'.toInt, 0xd83c, 0xdf63, '4'.toInt))
    assertEquals(s.chars(true).toSeq, Seq('0'.toInt, 0x1f363, '4'.toInt))

  test("UString#insert"):
    val s = fromUTF16Units("012")
    assertEquals(s.insert(0, fromUTF16Units("xyz")), fromUTF16Units("xyz012"))
    assertEquals(s.insert(1, fromUTF16Units("xyz")), fromUTF16Units("0xyz12"))
    assertEquals(s.insert(2, fromUTF16Units("xyz")), fromUTF16Units("01xyz2"))
    assertEquals(s.insert(3, fromUTF16Units("xyz")), fromUTF16Units("012xyz"))

  test("UString#insertAt"):
    val s = fromUTF16Units("012")
    assertEquals(s.insertAt(0, 'x'), fromUTF16Units("x012"))
    assertEquals(s.insertAt(1, 'x'), fromUTF16Units("0x12"))
    assertEquals(s.insertAt(2, 'x'), fromUTF16Units("01x2"))
    assertEquals(s.insertAt(3, 'x'), fromUTF16Units("012x"))
    assertEquals(s.insertAt(1, 0x1f363), fromUTF16Units("0🍣12"))

  test("UString#replaceAt"):
    val s1 = fromUTF16Units("012")
    val s2 = fromUTF16Units("0🍣4")
    assertEquals(s1.replaceAt(0, 'x', false), fromUTF16Units("x12"))
    assertEquals(s1.replaceAt(1, 'x', false), fromUTF16Units("0x2"))
    assertEquals(s1.replaceAt(2, 'x', false), fromUTF16Units("01x"))
    assertEquals(s2.replaceAt(1, 'x', false), fromUTF16Units("0x\udf634"))
    assertEquals(s2.replaceAt(1, 'x', true), fromUTF16Units("0x4"))
