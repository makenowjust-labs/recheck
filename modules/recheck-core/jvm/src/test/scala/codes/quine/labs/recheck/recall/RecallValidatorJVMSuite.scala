package codes.quine.labs.recheck.recall

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.util.UStringUtil

class RecallValidatorJVMSuite extends munit.FunSuite:
  test("RecallValidator.validate: JVM"):
    val pattern =
      AttackPattern(Seq((UStringUtil.fromString("x"), UStringUtil.fromString("y"), 0)), UStringUtil.fromString("z"), 2)

    intercept[TimeoutException]:
      val ctx50ms = Context(timeout = Duration(50, MILLISECONDS))
      RecallValidator.validate("foo", "1", pattern, Duration(50, MILLISECONDS))((_, _) =>
        Thread.sleep(100)
        None
      )(using ctx50ms)
