package codes.quine.labo.redos
package backtrack

import data.UString

class CaptureListSuite extends munit.FunSuite {
  test("CaptureList#size") {
    val s = UString.from("fizzbuzz", false)
    val caps = CaptureList(s, Map.empty, IndexedSeq(0, 4, 0, 2, 2, 4))
    assertEquals(caps.size, 2)
  }

  test("CaptureList#get") {
    val s = UString.from("fizzbuzz", false)
    val caps = CaptureList(s, Map("x" -> 1), IndexedSeq(0, 4, 0, 2, -1, -1, 2, 4))
    assertEquals(caps.get(0).map(_.asString), Some("fizz"))
    assertEquals(caps.get(1).map(_.asString), Some("fi"))
    assertEquals(caps.get(2).map(_.asString), None)
    assertEquals(caps.get(3).map(_.asString), Some("zz"))
    assertEquals(caps.get(-1), None)
    assertEquals(caps.get(4), None)
    assertEquals(caps.get("x").map(_.asString), Some("fi"))
    assertEquals(caps.get("y"), None)
  }

  test("CaptureList#toString") {
    val s = UString.from("fizzbuzz", false)
    val caps = CaptureList(s, Map("x" -> 1), IndexedSeq(0, 4, 0, 2, -1, -1, 2, 4))
    assertEquals(caps.toString, "['fizz', 'x': 'fi', undefined, 'zz']")
  }
}
