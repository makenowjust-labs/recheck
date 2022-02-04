package codes.quine.labs.recheck
package automaton

class WitnessSuite extends munit.FunSuite {
  test("Witness#map") {
    assertEquals(
      Witness(Seq((Seq(1), Seq(2))), Seq(3)).map(_ + 1),
      Witness(Seq((Seq(2), Seq(3))), Seq(4))
    )
  }

  test("Witness#buildAttack") {
    assertEquals(Witness(Seq((Seq(1), Seq(2))), Seq(3)).buildAttack(0), Seq(1, 3))
    assertEquals(Witness(Seq((Seq(1), Seq(2))), Seq(3)).buildAttack(1), Seq(1, 2, 3))
    assertEquals(Witness(Seq((Seq(1), Seq(2))), Seq(3)).buildAttack(2), Seq(1, 2, 2, 3))
  }

  test("Witness#toLazyList") {
    assertEquals(
      Witness(Seq((Seq(1), Seq(2))), Seq(3)).toLazyList.take(3),
      LazyList(
        Seq(1, 3),
        Seq(1, 2, 3),
        Seq(1, 2, 2, 3)
      )
    )
  }

  test("Witness#repeatSize") {
    assertEquals(Witness(Seq((Seq(1), Seq(2))), Seq(3)).repeatSize, 1)
  }

  test("Witness#fixedSize") {
    assertEquals(Witness(Seq((Seq(1), Seq(2))), Seq(3)).fixedSize, 2)
  }
}
