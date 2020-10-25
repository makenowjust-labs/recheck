package codes.quine.labo.redos.data

class MultiSetSuite extends munit.FunSuite {
  test("MultiSet.empty") {
    assertEquals(MultiSet.empty.map, Map.empty)
  }

  test("MultiSet.from") {
    assertEquals(MultiSet.from(Seq(1, 1, 2)).map, Map(1 -> 2, 2 -> 1))
  }

  test("MultiSet.apply") {
    assertEquals(MultiSet(1, 1, 2).map, Map(1 -> 2, 2 -> 1))
  }

  test("MultiSet#iterator") {
    assertEquals(MultiSet(1, 1, 2).iterator.toSeq, Seq(1, 1, 2))
  }

  test("MultiSet#++") {
    assertEquals(MultiSet(1, 1, 2) ++ MultiSet(2, 3, 3), MultiSet(1, 1, 2, 2, 3, 3))
  }
}
