package codes.quine.labs.recheck.recall

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.unicode.UString

class RecallValidatorJVMSuite extends munit.FunSuite {
  test("RecallValidator.validate: JVM") {
    val pattern = AttackPattern(Seq((UString("x"), UString("y"), 0)), UString("z"), 2)

    intercept[TimeoutException] {
      val ctx50ms = Context(timeout = Duration(50, MILLISECONDS))
      RecallValidator.validate("foo", "1", pattern, Duration(50, MILLISECONDS))((_, _) => {
        Thread.sleep(50)
        None
      })(ctx50ms)
    }
  }
}
