package codes.quine.labo.recheck.recall

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.MILLISECONDS
import scala.concurrent.duration.SECONDS

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.TimeoutException
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.unicode.UString

class RecallValidatorSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("RecallValidator.checks") {
    val pattern = AttackPattern(Seq((UString("x"), UString("y"), 0)), UString("z"), 2)
    assertEquals(
      RecallValidator.checks("foo", "i", pattern, Duration.MinusInf)((_, _) => fail("unreachable")),
      true
    )

    assertEquals(
      RecallValidator.checks("foo", "i", pattern, Duration.Inf)((_, _) => None),
      true
    )

    assertEquals(
      RecallValidator.checks("foo", "i", pattern, Duration.Inf)((_, _) => Some((0, "1", ""))),
      false
    )
  }

  test("RecallValidator.validate") {
    val pattern = AttackPattern(Seq((UString("x"), UString("y"), 0)), UString("z"), 2)
    assertEquals(
      RecallValidator.validate("foo", "i", pattern, Duration.MinusInf)((_, _) => fail("unreachable")),
      RecallResult.Timeout
    )

    var timeout1: Option[FiniteDuration] = null
    assertEquals(
      RecallValidator.validate("foo", "i", pattern, Duration.Inf)((_, t) => { timeout1 = t; None }),
      RecallResult.Timeout
    )
    assertEquals(timeout1, None)

    var timeout2: Option[FiniteDuration] = null
    assertEquals(
      RecallValidator.validate("foo", "i", pattern, Duration(100, SECONDS))((_, t) => { timeout2 = t; None }),
      RecallResult.Timeout
    )
    assertEquals(timeout2, Some(FiniteDuration(100, SECONDS)))

    var timeout3: Option[FiniteDuration] = null
    val ctx50s = Context(timeout = Duration(50, SECONDS))
    assertEquals(
      RecallValidator.validate("foo", "i", pattern, Duration(100, SECONDS))((_, t) => { timeout3 = t; None })(ctx50s),
      RecallResult.Timeout
    )
    assertEquals(timeout3.exists(_ <= FiniteDuration(50, SECONDS)), true)

    intercept[TimeoutException] {
      val ctx50ms = Context(timeout = Duration(50, MILLISECONDS))
      RecallValidator.validate("foo", "1", pattern, Duration(50, MILLISECONDS))((_, t) => {
        Thread.sleep(t.get.toMillis)
        None
      })(ctx50ms)
    }
  }

  test("RecallValidator.generate") {
    assertEquals(
      RecallValidator.generate("foo", "i", AttackPattern(Seq((UString("x"), UString("y"), 0)), UString("z"), 2)),
      """|const re = new RegExp('foo', 'i');
         |const input = 'x' + 'y'.repeat(2) + 'z';
         |const start = Date.now();
         |re.exec(input);
         |const end = Date.now();
         |console.log(Number(end - start).toString());
         |""".stripMargin
    )
  }

  test("RecallValidator.result") {
    assertEquals(RecallValidator.result(Some((0, "1", ""))), RecallResult.Finish(FiniteDuration(1L, MILLISECONDS)))
    assertEquals(RecallValidator.result(Some((1, "", "error"))), RecallResult.Error("error"))
    assertEquals(RecallValidator.result(None), RecallResult.Timeout)
  }
}
