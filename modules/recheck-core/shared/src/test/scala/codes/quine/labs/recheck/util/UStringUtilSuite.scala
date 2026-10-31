package codes.quine.labs.recheck.util

import codes.quine.labs.recheck.util.UStringUtil.*
import codes.quine.labs.resyntax.charset.UString

class UStringUtilSuite extends munit.FunSuite:
  test("UStringUtil.fromString"):
    assertEquals(fromString(""), UString.empty)
    assertEquals(fromString("x🍣"), UString('x', 0xd83c, 0xdf63))
    assertEquals(fromString("\udf63"), UString(0xdf63))

  test("UStringUtil.fromCodePoints"):
    assertEquals(fromCodePoints(Seq.empty), UString.empty)
    assertEquals(fromCodePoints("xyz".map(_.toInt)), fromString("xyz"))
    assertEquals(fromCodePoints(Seq(0x1f363)), fromString("🍣"))

  test("UStringUtil.canonicalize"):
    assertEquals(canonicalize(fromString("xyz"), false), fromString("XYZ"))
    assertEquals(canonicalize(fromString("XYZ"), true), fromString("xyz"))

  test("UStringUtil.show"):
    assertEquals(show(fromString("")), "''")
    assertEquals(show(fromString("xyz")), "'xyz'")
    assertEquals(show(fromString("a\nb")), "'a\\nb'")
    assertEquals(show(fromString("a\\b")), "'a\\\\b'")
    assertEquals(show(fromString("a'b")), "'a\\'b'")
    assertEquals(show(fromString("🍣")), "'🍣'")

  test("UStringUtil#getAt"):
    val s1 = fromString("xyz")
    val s2 = fromString("x🍣z")
    assertEquals(s1.getAt(0, false), Some('x'.toInt))
    assertEquals(s1.getAt(1, false), Some('y'.toInt))
    assertEquals(s1.getAt(2, false), Some('z'.toInt))
    assertEquals(s1.getAt(-1, false), None)
    assertEquals(s1.getAt(3, false), None)
    assertEquals(s2.getAt(1, false), Some(0xd83c))
    assertEquals(s2.getAt(1, true), Some(0x1f363))
    assertEquals(s2.getAt(2, true), Some(0xdf63))

  test("UStringUtil#getBefore"):
    val s1 = fromString("xyz")
    val s2 = fromString("x🍣z")
    assertEquals(s1.getBefore(1, false), Some('x'.toInt))
    assertEquals(s1.getBefore(2, false), Some('y'.toInt))
    assertEquals(s1.getBefore(3, false), Some('z'.toInt))
    assertEquals(s1.getBefore(0, false), None)
    assertEquals(s1.getBefore(4, false), None)
    assertEquals(s2.getBefore(2, false), Some(0xd83c))
    assertEquals(s2.getBefore(2, true), Some(0xd83c))
    assertEquals(s2.getBefore(3, true), Some(0x1f363))

  test("UStringUtil#insertAt"):
    val s = fromString("012")
    assertEquals(s.insertAt(0, 'x'), fromString("x012"))
    assertEquals(s.insertAt(1, 'x'), fromString("0x12"))
    assertEquals(s.insertAt(2, 'x'), fromString("01x2"))
    assertEquals(s.insertAt(3, 'x'), fromString("012x"))
    assertEquals(s.insertAt(1, 0x1f363), fromString("0🍣12"))

  test("UStringUtil#insert"):
    val s = fromString("012")
    assertEquals(s.insert(0, fromString("xyz")), fromString("xyz012"))
    assertEquals(s.insert(1, fromString("xyz")), fromString("0xyz12"))
    assertEquals(s.insert(2, fromString("xyz")), fromString("01xyz2"))
    assertEquals(s.insert(3, fromString("xyz")), fromString("012xyz"))

  test("UStringUtil#replaceAt"):
    val s1 = fromString("012")
    val s2 = fromString("0🍣4")
    assertEquals(s1.replaceAt(0, 'x', false), fromString("x12"))
    assertEquals(s1.replaceAt(1, 'x', false), fromString("0x2"))
    assertEquals(s1.replaceAt(2, 'x', false), fromString("01x"))
    assertEquals(s2.replaceAt(1, 'x', false), fromString("0x\udf634"))
    assertEquals(s2.replaceAt(1, 'x', true), fromString("0x4"))

  test("UStringUtil#chars"):
    val s = fromString("0🍣4")
    assertEquals(s.chars(false).toSeq, Seq('0'.toInt, 0xd83c, 0xdf63, '4'.toInt))
    assertEquals(s.chars(true).toSeq, Seq('0'.toInt, 0x1f363, '4'.toInt))
