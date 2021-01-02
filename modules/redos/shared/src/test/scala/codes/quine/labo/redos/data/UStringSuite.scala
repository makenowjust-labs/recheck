package codes.quine.labo.redos.data

class UStringSuite extends munit.FunSuite {
  test("UString,empty") {
    assertEquals(UString.empty, UString(IndexedSeq.empty))
  }

  test("UString.from") {
    assertEquals(UString.from("", false), UString(IndexedSeq.empty))
    assertEquals(UString.from("", true), UString(IndexedSeq.empty))
    assertEquals(UString.from("xyz", false), UString("xyz".map(UChar(_))))
    assertEquals(UString.from("xyz", true), UString("xyz".map(UChar(_))))
    assertEquals(UString.from("🍣", false), UString("🍣".map(UChar(_))))
    assertEquals(UString.from("🍣", true), UString(IndexedSeq(UChar(0x1f363))))
  }

  test("UString#isEmpty") {
    assert(UString.empty.isEmpty)
    assert(!UString.from("xyz", false).isEmpty)
  }

  test("UString#nonEmpty") {
    assert(!UString.empty.nonEmpty)
    assert(UString.from("xyz", false).nonEmpty)
  }

  test("UString#size") {
    assertEquals(UString.from("", false).size, 0)
    assertEquals(UString.from("xyz", false).size, 3)
  }

  test("UString#get") {
    val s = UString.from("xyz", false)
    assertEquals(s.get(0), Some(UChar('x')))
    assertEquals(s.get(1), Some(UChar('y')))
    assertEquals(s.get(2), Some(UChar('z')))
    assertEquals(s.get(-1), None)
    assertEquals(s.get(3), None)
  }

  test("UString#substring") {
    val s = UString.from("0123456789", false)
    assertEquals(s.substring(0, 0), UString(IndexedSeq.empty))
    assertEquals(s.substring(0, 1), UString.from("0", false))
    assertEquals(s.substring(1, 3), UString.from("12", false))
    assertEquals(s.substring(2, 8), UString.from("234567", false))
  }

  test("UString#insertAt") {
    val s = UString.from("012", false)
    assertEquals(s.insertAt(0, 'x'), UString.from("x012", false))
    assertEquals(s.insertAt(1, 'x'), UString.from("0x12", false))
    assertEquals(s.insertAt(2, 'x'), UString.from("01x2", false))
    assertEquals(s.insertAt(3, 'x'), UString.from("012x", false))
  }

  test("UString#insert") {
    val s = UString.from("012", false)
    assertEquals(s.insert(0, UString.from("xyz", false)), UString.from("xyz012", false))
    assertEquals(s.insert(1, UString.from("xyz", false)), UString.from("0xyz12", false))
    assertEquals(s.insert(2, UString.from("xyz", false)), UString.from("01xyz2", false))
    assertEquals(s.insert(3, UString.from("xyz", false)), UString.from("012xyz", false))
  }

  test("UString#delete") {
    val s = UString.from("012", false)
    assertEquals(s.delete(0, 3), UString.empty)
    assertEquals(s.delete(1, 1), UString.from("02", false))
  }

  test("UString#replaceAt") {
    val s = UString.from("012", false)
    assertEquals(s.replaceAt(0, 'x'), UString.from("x12", false))
    assertEquals(s.replaceAt(1, 'x'), UString.from("0x2", false))
    assertEquals(s.replaceAt(2, 'x'), UString.from("01x", false))
  }

  test("UString#replace") {
    val s = UString.from("012", false)
    assertEquals(s.replace(1, 1, UString.from("xyz", false)), UString.from("0xyz2", false))
    assertEquals(s.replace(0, 2, UString.from("xyz", false)), UString.from("xyz2", false))
    assertEquals(s.replace(1, 2, UString.from("xyz", false)), UString.from("0xyz", false))
  }

  test("UString#asString") {
    assertEquals(UString(IndexedSeq.empty).asString, "")
    assertEquals(UString.from("foo", false).asString, "foo")
  }

  test("UString#toString") {
    assertEquals(UString(IndexedSeq.empty).toString, "''")
    assertEquals(UString.from("xyz", false).toString, "'xyz'")
    assertEquals(UString.from("a\nb", false).toString, "'a\\nb'")
    assertEquals(UString.from("a'b", false).toString, "'a\\'b'")
  }
}
