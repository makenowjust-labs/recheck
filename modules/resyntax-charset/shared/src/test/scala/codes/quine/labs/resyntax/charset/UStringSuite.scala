package codes.quine.labs.resyntax.charset

class UStringSuite extends munit.FunSuite:
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
    val s = UnitMode.UTF16.decode("012")
    assertEquals(s.insert(0, UnitMode.UTF16.decode("xyz")), UnitMode.UTF16.decode("xyz012"))
    assertEquals(s.insert(1, UnitMode.UTF16.decode("xyz")), UnitMode.UTF16.decode("0xyz12"))
    assertEquals(s.insert(2, UnitMode.UTF16.decode("xyz")), UnitMode.UTF16.decode("01xyz2"))
    assertEquals(s.insert(3, UnitMode.UTF16.decode("xyz")), UnitMode.UTF16.decode("012xyz"))
