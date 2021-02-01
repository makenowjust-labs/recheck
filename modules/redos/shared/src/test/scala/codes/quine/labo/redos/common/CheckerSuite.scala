package codes.quine.labo.redos.common

class CheckerSuite extends munit.FunSuite {
  test("Checker#toString") {
    assertEquals(Checker.Automaton.toString, "automaton")
    assertEquals(Checker.Fuzz.toString, "fuzz")
    assertEquals(Checker.Hybrid.toString, "hybrid")
  }
}
