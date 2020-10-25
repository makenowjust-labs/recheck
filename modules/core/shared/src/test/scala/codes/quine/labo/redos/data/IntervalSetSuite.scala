package codes.quine.labo.redos.data

import IntervalSet._

class IntervalSetSuite extends munit.FunSuite {
  test("IntervalSet.apply") {
    assertEquals(IntervalSet((1, 2)).intervals, Vector((1, 2)))
    assertEquals(IntervalSet((1, 2), (3, 4)).intervals, Vector((1, 2), (3, 4)))
    assertEquals(IntervalSet((1, 2), (2, 3)).intervals, Vector((1, 3)))
  }

  test("IntervalSet.empty") {
    assertEquals(IntervalSet.empty.intervals, Vector.empty)
  }

  test("IntervalSet.from") {
    assertEquals(IntervalSet.from(Seq((1, 2))).intervals, Vector((1, 2)))
    assertEquals(IntervalSet.from(Seq((1, 2), (3, 4))).intervals, Vector((1, 2), (3, 4)))
    assertEquals(IntervalSet.from(Seq((1, 2), (2, 3))).intervals, Vector((1, 3)))
  }

  test("IntervalSet#isEmpty") {
    assert(IntervalSet.empty[Int].isEmpty)
    assert(!IntervalSet((1, 3)).isEmpty)
  }

  test("IntervalSet#nonEmpty") {
    assert(!IntervalSet.empty[Int].nonEmpty)
    assert(IntervalSet((1, 3)).nonEmpty)
  }

  test("IntervalSet#union") {
    assertEquals(IntervalSet.empty[Int] union IntervalSet.empty, IntervalSet.empty[Int])
    assertEquals(IntervalSet((1, 2)) union IntervalSet.empty, IntervalSet((1, 2)))
    assertEquals(IntervalSet((1, 2)) union IntervalSet((3, 4)), IntervalSet((1, 2), (3, 4)))
    assertEquals(IntervalSet((1, 2)) union IntervalSet((2, 3)), IntervalSet((1, 3)))
  }

  test("IntervalSet#intersection") {
    assertEquals(IntervalSet.empty[Int] intersection IntervalSet.empty, IntervalSet.empty[Int])
    assertEquals(IntervalSet((1, 5)) intersection IntervalSet.empty, IntervalSet.empty[Int])
    assertEquals(IntervalSet.empty intersection IntervalSet((1, 5)), IntervalSet.empty[Int])
    assertEquals(IntervalSet((1, 5)) intersection IntervalSet((2, 4)), IntervalSet((2, 4)))
    assertEquals(IntervalSet((2, 4)) intersection IntervalSet((1, 5)), IntervalSet((2, 4)))
    assertEquals(IntervalSet((1, 5)) intersection IntervalSet((1, 2), (4, 5)), IntervalSet((1, 2), (4, 5)))
    assertEquals(IntervalSet((1, 5)) intersection IntervalSet((0, 2), (4, 6)), IntervalSet((1, 2), (4, 5)))
  }

  test("IntervalSet#diff") {
    assertEquals(IntervalSet((1, 5)) diff IntervalSet.empty, IntervalSet((1, 5)))
    assertEquals(IntervalSet((1, 5)) diff IntervalSet((2, 4)), IntervalSet((1, 2), (4, 5)))
    assertEquals(IntervalSet((1, 5)) diff IntervalSet((0, 2), (4, 6)), IntervalSet((2, 4)))
  }

  test("IntervalSet#partition") {
    assertEquals(
      IntervalSet((1, 5)) partition IntervalSet((0, 2), (4, 6)),
      Partition(IntervalSet((1, 2), (4, 5)), IntervalSet((2, 4)), IntervalSet((0, 1), (5, 6)))
    )
  }

  test("IntervalSet#contains") {
    val set = IntervalSet((1, 3), (4, 6))
    for (x <- Seq(1, 2, 4, 5)) assert(set.contains(x))
    for (x <- Seq(0, 3, 6)) assert(!set.contains(x))
  }

  test("IntervalSet#map") {
    assertEquals(IntervalSet((1, 3), (4, 6)).map(_ + 10), IntervalSet((11, 13), (14, 16)))
  }

  test("IntervalSet#mapIntersection") {
    assertEquals(
      IntervalSet((1, 3), (4, 6)).mapIntersection(IntervalSet((2, 5)))(_ + 10),
      IntervalSet((1, 2), (5, 6), (12, 13), (14, 15))
    )
  }

  test("IntervalSet#toString") {
    assertEquals(IntervalSet((1, 3), (4, 6)).toString, "IntervalSet((1, 3), (4, 6))")
  }
}
