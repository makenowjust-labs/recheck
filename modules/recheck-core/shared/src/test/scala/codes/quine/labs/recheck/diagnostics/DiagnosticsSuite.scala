package codes.quine.labs.recheck
package diagnostics

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import scala.concurrent.duration.SECONDS

import codes.quine.labs.recheck.common.CancelException
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.util.UStringUtil

class DiagnosticsSuite extends munit.FunSuite:
  test("Diagnostics#toString"):
    assertEquals(
      Diagnostics.Safe("", "", AttackComplexity.Constant, Checker.Automaton).toString,
      s"""|Input     : //
          |Status    : safe
          |Complexity: constant
          |Checker   : automaton""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Vulnerable(
          "abbc",
          "",
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UStringUtil.fromString("a"), UStringUtil.fromString("b"), 0)),
            UStringUtil.fromString("c"),
            2
          ),
          Hotspot.empty,
          Checker.Automaton
        )
        .toString,
      s"""|Input        : /abbc/
          |Status       : vulnerable
          |Complexity   : exponential
          |Attack string: 'a' + 'b'.repeat(2) + 'c'
          |Hotspot      : /abbc/
          |Checker      : automaton""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Vulnerable(
          "a*a*$",
          "",
          AttackComplexity.Polynomial(2, false),
          AttackPattern(
            Seq((UStringUtil.fromString(""), UStringUtil.fromString("a"), 0)),
            UStringUtil.fromString("!"),
            2
          ),
          Hotspot.empty,
          Checker.Automaton,
          Some(RecallValidation("ruby 4.0.7", Duration(1, SECONDS), RecallResult.Finish(Duration(3, MILLISECONDS))))
        )
        .toString,
      s"""|Input        : /a*a*$$/
          |Status       : vulnerable
          |Complexity   : 2nd degree polynomial
          |Attack string: 'a'.repeat(2) + '!'
          |Hotspot      : /a*a*$$/
          |Checker      : automaton
          |Recall       : failed (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)""".stripMargin
    )
    assertEquals(
      Diagnostics.Unknown("", "", Diagnostics.ErrorKind.Timeout, Some(Checker.Automaton)).toString,
      s"""|Input  : //
          |Status : unknown
          |Error  : timeout
          |Checker: automaton""".stripMargin
    )
    assertEquals(
      Diagnostics.Unknown("", "", Diagnostics.ErrorKind.Timeout, None).toString,
      s"""|Input  : //
          |Status : unknown
          |Error  : timeout
          |Checker: (none)""".stripMargin
    )

  test("RecallValidation#toString"):
    assertEquals(
      RecallValidation("python 3.15.0", Duration.Inf, RecallResult.Timeout).toString,
      "succeeded (timed out on python 3.15.0, timeout: none)"
    )
    assertEquals(
      RecallValidation(
        "php 8.5.12",
        Duration(1, SECONDS),
        RecallResult.EngineLimit("Backtrack limit exhausted")
      ).toString,
      "succeeded (engine limit: Backtrack limit exhausted on php 8.5.12, timeout: 1000 ms)"
    )
    assertEquals(
      RecallValidation("java 25", Duration(1, SECONDS), RecallResult.EngineError("StackOverflowError")).toString,
      "failed (engine error: StackOverflowError on java 25, timeout: 1000 ms)"
    )

  test("Diagnostics.Unknown.from"):
    assertEquals(
      Diagnostics.Unknown.from("", "", new TimeoutException("foo")),
      Diagnostics.Unknown("", "", Diagnostics.ErrorKind.Timeout, None)
    )
    assertEquals(
      Diagnostics.Unknown.from("", "", new CancelException("foo")),
      Diagnostics.Unknown("", "", Diagnostics.ErrorKind.Cancel, None)
    )
    assertEquals(
      Diagnostics.Unknown.from("", "", new InvalidRegExpException("foo")),
      Diagnostics.Unknown("", "", Diagnostics.ErrorKind.InvalidRegExp("foo"), None)
    )
    assertEquals(
      Diagnostics.Unknown.from("", "", new UnsupportedException("foo")),
      Diagnostics.Unknown("", "", Diagnostics.ErrorKind.Unsupported("foo"), None)
    )
    assertEquals(
      Diagnostics.Unknown.from("", "", new UnexpectedException("foo")),
      Diagnostics.Unknown("", "", Diagnostics.ErrorKind.Unexpected("foo"), None)
    )

  test("Diagnostics.ErrorKind#toString"):
    assertEquals(Diagnostics.ErrorKind.Timeout.toString, "timeout")
    assertEquals(Diagnostics.ErrorKind.Cancel.toString, "cancel")
    assertEquals(Diagnostics.ErrorKind.Unsupported("foo").toString, "unsupported (foo)")
    assertEquals(Diagnostics.ErrorKind.InvalidRegExp("foo").toString, "invalid RegExp (foo)")
