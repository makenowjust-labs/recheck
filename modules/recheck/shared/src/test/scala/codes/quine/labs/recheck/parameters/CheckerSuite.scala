package codes.quine.labs.recheck.parameters

class CheckerSuite extends munit.FunSuite:

  test("Checker#toString"):
    assertEquals(
      Seq(Checker.Automaton, Checker.Fuzz, Checker.Auto).map(_.toString),
      Seq("automaton", "fuzz", "auto")
    )
