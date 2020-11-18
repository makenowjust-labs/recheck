package codes.quine.labo.redos
import automaton.Complexity

class ReDoSSuite extends munit.FunSuite {
  test("ReDoS.check") {
    assertEquals(ReDoS.check("^foo$", ""), Diagnostics.Safe(Some(Complexity.Constant)))
    assertEquals(ReDoS.check("", "x"), Diagnostics.Unknown(Diagnostics.ErrorKind.InvalidRegExp("unknown flag")))
  }
}
