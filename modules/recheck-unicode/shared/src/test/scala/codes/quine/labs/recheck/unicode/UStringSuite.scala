package codes.quine.labs.recheck.unicode

class UStringSuite extends munit.FunSuite {
  test("UString,empty") {
    assertEquals(UString.empty, UString(""))
  }

  test("UString.from") {
    assertEquals(UString.from(IndexedSeq.empty[UChar]), UString(""))
    assertEquals(UString.from("xyz".map(UChar(_))), UString("xyz"))
    assertEquals(UString.from(IndexedSeq(UChar(0x1f363))), UString("🍣"))
  }

  test("UString.canonicalize") {
    assertEquals(UString.canonicalize(UString("xyz"), false), UString("XYZ"))
    assertEquals(UString.canonicalize(UString("XYZ"), true), UString("xyz"))
  }

  test("UString#isEmpty") {
    assert(UString.empty.isEmpty)
    assert(!UString("xyz").isEmpty)
  }

  test("UString#nonEmpty") {
    assert(!UString.empty.nonEmpty)
    assert(UString("xyz").nonEmpty)
  }

  test("UString#sizeAsString") {
    assertEquals(UString("").sizeAsString, 0)
    assertEquals(UString("xyz").sizeAsString, 3)
  }

  test("UString#getAt") {
    val s1 = UString("xyz")
    val s2 = UString("x🍣z")
    assertEquals(s1.getAt(0, false), Some(UChar('x')))
    assertEquals(s1.getAt(1, false), Some(UChar('y')))
    assertEquals(s1.getAt(2, false), Some(UChar('z')))
    assertEquals(s1.getAt(-1, false), None)
    assertEquals(s1.getAt(3, false), None)
    assertEquals(s2.getAt(1, false), Some(UChar(0xd83c)))
    assertEquals(s2.getAt(1, true), Some(UChar(0x1f363)))
  }

  test("UString#getBefore") {
    val s1 = UString("xyz")
    val s2 = UString("x🍣z")
    assertEquals(s1.getBefore(1, false), Some(UChar('x')))
    assertEquals(s1.getBefore(2, false), Some(UChar('y')))
    assertEquals(s1.getBefore(3, false), Some(UChar('z')))
    assertEquals(s1.getBefore(0, false), None)
    assertEquals(s1.getBefore(4, false), None)
    assertEquals(s2.getBefore(2, false), Some(UChar(0xd83c)))
    assertEquals(s2.getBefore(3, true), Some(UChar(0x1f363)))
  }

  test("UString#substring") {
    val s = UString("0123456789")
    assertEquals(s.substring(0, 0), UString(""))
    assertEquals(s.substring(0, 1), UString("0"))
    assertEquals(s.substring(1, 3), UString("12"))
    assertEquals(s.substring(2, 8), UString("234567"))
  }

  test("UString#insertAt") {
    val s = UString("012")
    assertEquals(s.insertAt(0, 'x'), UString("x012"))
    assertEquals(s.insertAt(1, 'x'), UString("0x12"))
    assertEquals(s.insertAt(2, 'x'), UString("01x2"))
    assertEquals(s.insertAt(3, 'x'), UString("012x"))
  }

  test("UString#insert") {
    val s = UString("012")
    assertEquals(s.insert(0, UString("xyz")), UString("xyz012"))
    assertEquals(s.insert(1, UString("xyz")), UString("0xyz12"))
    assertEquals(s.insert(2, UString("xyz")), UString("01xyz2"))
    assertEquals(s.insert(3, UString("xyz")), UString("012xyz"))
  }

  test("UString#replaceAt") {
    val s1 = UString("012")
    val s2 = UString("0🍣4")
    assertEquals(s1.replaceAt(0, 'x', false), UString("x12"))
    assertEquals(s1.replaceAt(1, 'x', false), UString("0x2"))
    assertEquals(s1.replaceAt(2, 'x', false), UString("01x"))
    assertEquals(s2.replaceAt(1, 'x', false), UString("0x\udf634"))
    assertEquals(s2.replaceAt(1, 'x', true), UString("0x4"))
  }

  test("UString#iterator") {
    val s = UString("0🍣4")
    assertEquals(s.iterator(false).toSeq, Seq(UChar('0'), UChar(0xd83c), UChar(0xdf63), UChar('4')))
    assertEquals(s.iterator(true).toSeq, Seq(UChar('0'), UChar(0x1f363), UChar('4')))
  }

  test("UString#asString") {
    assertEquals(UString("").asString, "")
    assertEquals(UString("foo").asString, "foo")
  }

  test("UString#toString") {
    assertEquals(UString("").toString, "''")
    assertEquals(UString("xyz").toString, "'xyz'")
    assertEquals(UString("a\nb").toString, "'a\\nb'")
    assertEquals(UString("a\\b").toString, "'a\\\\b'")
    assertEquals(UString("a'b").toString, "'a\\'b'")
  }
}
