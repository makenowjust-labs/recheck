package codes.quine.labo.redos
package backtrack

import data.UString

class MatchSuite extends munit.FunSuite {
  test("Match.apply") {
    val s = UString.from("fizzbuzz", false)
    val caps = Match(s, Map("x" -> 1), IndexedSeq(0, 4, 0, 2, 2, 4))
    assertEquals(caps.input, s)
    assertEquals(caps.names, Map("x" -> 1))
  }

  test("Match#size") {
    val s = UString.from("fizzbuzz", false)
    val caps = Match(s, Map.empty, IndexedSeq(0, 4, 0, 2, 2, 4))
    assertEquals(caps.size, 2)
  }

  test("Match#capture") {
    val s = UString.from("fizzbuzz", false)
    val caps = Match(s, Map("x" -> 1), IndexedSeq(0, 4, 0, 2, -1, -1, 2, 4))
    assertEquals(caps.capture(0).map(_.asString), Some("fizz"))
    assertEquals(caps.capture(1).map(_.asString), Some("fi"))
    assertEquals(caps.capture(2).map(_.asString), None)
    assertEquals(caps.capture(3).map(_.asString), Some("zz"))
    assertEquals(caps.capture(-1), None)
    assertEquals(caps.capture(4), None)
    assertEquals(caps.capture("x").map(_.asString), Some("fi"))
    assertEquals(caps.capture("y"), None)
  }

  test("Match#toString") {
    val s = UString.from("fizzbuzz", false)
    val caps = Match(s, Map("x" -> 1), IndexedSeq(0, 4, 0, 2, -1, -1, 2, 4))
    assertEquals(caps.toString, "['fizz', 'x': 'fi', undefined, 'zz']")
  }
}
