package codes.quine.labo.redos

import scala.concurrent.duration._

import automaton.Complexity
import util.Timeout

class ReDoSSuite extends munit.FunSuite {
  test("ReDoS.check") {
    assertEquals(ReDoS.check("^foo$", ""), Diagnostics.Safe(None))
    assertEquals(ReDoS.check("^.*$", ""), Diagnostics.Safe(Some(Complexity.Linear)))
    assertEquals(ReDoS.check("", "x"), Diagnostics.Unknown(Diagnostics.ErrorKind.InvalidRegExp("unknown flag")))
    assertEquals(
      ReDoS.check("^foo$", "", Config(timeout = Timeout.from(0.second))),
      Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout)
    )
  }
}
