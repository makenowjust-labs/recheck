package codes.quine.labo.redos

import java.util.concurrent.TimeoutException

import scala.concurrent.duration._
import scala.util.Success

import Checker._

class ReDoSSuite extends munit.FunSuite {
  test("ReDoS.check") {
    assertEquals(ReDoS.check("^foo$", ""), Success(Complexity.Constant))
    intercept[TimeoutException](ReDoS.check("(a?){50}a{50}", "", 0.second).get)
  }
}
