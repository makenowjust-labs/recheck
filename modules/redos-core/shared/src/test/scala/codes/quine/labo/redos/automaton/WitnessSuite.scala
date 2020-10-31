package codes.quine.labo.redos
package automaton

class WitnessSuite extends munit.FunSuite {
  test("Witness#map") {
    assertEquals(
      Witness(Seq((Seq(1), Seq(2))), Seq(3)).map(_ + 1),
      Witness(Seq((Seq(2), Seq(3))), Seq(4))
    )
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
}
