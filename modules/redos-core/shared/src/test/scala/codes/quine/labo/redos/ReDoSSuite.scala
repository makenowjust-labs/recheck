package codes.quine.labo.redos

import scala.concurrent.duration._

import automaton.Complexity

class ReDoSSuite extends munit.FunSuite {
  test("ReDoS.check") {
    assertEquals(ReDoS.check("^foo$", ""), Diagnostics.Safe(Some(Complexity.Constant)))
    assertEquals(ReDoS.check("(a?){50}a{50}", "", 0.second), Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout))
  }
}
