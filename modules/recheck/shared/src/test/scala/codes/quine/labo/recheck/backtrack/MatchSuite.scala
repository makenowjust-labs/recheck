package codes.quine.labo.recheck
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

  test("Match#position") {
    val s = UString.from("fizzbuzz", false)
    val caps = Match(s, Map("x" -> 1), IndexedSeq(0, 4, 0, 2, -1, -1, 2, 4))
    assertEquals(caps.position, (0, 4))
    assertEquals(caps.position(0), Some((0, 4)))
    assertEquals(caps.position(1), Some((0, 2)))
    assertEquals(caps.position(2), None)
    assertEquals(caps.position(3), Some((2, 4)))
    assertEquals(caps.position(-1), None)
    assertEquals(caps.position(4), None)
    assertEquals(caps.position("x"), Some((0, 2)))
    assertEquals(caps.position("y"), None)
  }

  test("Match#positions") {
    val s = UString.from("fizzbuzz", false)
    val caps = Match(s, Map("x" -> 1), IndexedSeq(0, 4, 0, 2, -1, -1, 2, 4))
    assertEquals(caps.positions, Seq(Some((0, 4)), Some((0, 2)), None, Some((2, 4))))
  }

  test("Match#captures") {
    val s = UString.from("fizzbuzz", false)
    val caps = Match(s, Map("x" -> 1), IndexedSeq(0, 4, 0, 2, -1, -1, 2, 4))
    assertEquals(caps.captures.map(_.map(_.asString)), Seq(Some("fizz"), Some("fi"), None, Some("zz")))
  }

  test("Match#toString") {
    val s = UString.from("fizzbuzz", false)
    val caps = Match(s, Map("x" -> 1), IndexedSeq(0, 4, 0, 2, -1, -1, 2, 4))
    assertEquals(caps.toString, "['fizz', 'x': 'fi', undefined, 'zz']")
  }
}
