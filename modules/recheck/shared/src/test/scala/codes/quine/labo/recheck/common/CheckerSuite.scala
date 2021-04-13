package codes.quine.labo.recheck.common

class CheckerSuite extends munit.FunSuite {
  test("Checker#toString") {
    assertEquals(
      Seq(Checker.Automaton, Checker.Fuzz, Checker.Hybrid).map(_.toString),
      Seq("automaton", "fuzz", "hybrid")
    )
  }
}
