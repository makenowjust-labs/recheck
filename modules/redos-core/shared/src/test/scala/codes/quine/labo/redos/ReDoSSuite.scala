package codes.quine.labo.redos

import scala.concurrent.duration._

import automaton.Complexity
import util.Timeout

class ReDoSSuite extends munit.FunSuite {
  test("ReDoS.check") {
    assertEquals(ReDoS.check("^foo$", ""), Diagnostics.Safe(None, Some(Checker.Automaton)))
    assertEquals(ReDoS.check("^.*$", ""), Diagnostics.Safe(Some(Complexity.Linear), Some(Checker.Automaton)))
    assertEquals(ReDoS.check("", "x"), Diagnostics.Unknown(Diagnostics.ErrorKind.InvalidRegExp("unknown flag"), None))
    assertEquals(
      ReDoS.check("^foo$", "", Config(timeout = Timeout.from(-1.second))),
      Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout, None)
    )
  }
}
