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
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.UnitMode

class DiagnosticsSuite extends munit.FunSuite:

  val refutedRecall: RefutedRecall = RefutedRecall(
    AttackComplexity.Polynomial(2, false),
    AttackPattern(Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)), UnitMode.UTF16.decode("!"), 2),
    Hotspot.empty,
    RecallValidation("ruby 4.0.7", Duration(1, SECONDS), RecallResult.Finish(Duration(3, MILLISECONDS)))
  )

  test("Diagnostics#toString"):
    assertEquals(
      Diagnostics
        .Safe("", "", Dialect.JavaScript, UnitMode.UTF16, AttackComplexity.Constant, Seq.empty, Checker.Automaton)
        .toString,
      s"""|Input     : //
          |Dialect   : javascript
          |Status    : safe
          |Complexity: constant
          |Checker   : automaton""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Vulnerable(
          "abbc",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UnitMode.UTF16.decode("a"), UnitMode.UTF16.decode("b"), 0)),
            UnitMode.UTF16.decode("c"),
            2
          ),
          Hotspot.empty,
          None,
          Seq.empty,
          Checker.Automaton
        )
        .toString,
      s"""|Input        : /abbc/
          |Dialect      : javascript
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
          Dialect.Ruby(),
          UnitMode.CodePoint,
          AttackComplexity.Polynomial(2, false),
          AttackPattern(
            Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)),
            UnitMode.UTF16.decode("!"),
            2
          ),
          Hotspot.empty,
          Some(RecallValidation("ruby 4.0.7", Duration(1, SECONDS), RecallResult.Timeout)),
          Seq(refutedRecall, refutedRecall),
          Checker.Automaton
        )
        .toString,
      s"""|Input        : /a*a*$$/
          |Dialect      : ruby (encoding: UTF-8)
          |Status       : vulnerable
          |Complexity   : 2nd degree polynomial
          |Attack string: "a" * 2 + "!"
          |Hotspot      : /a*a*$$/
          |Recall       : confirmed (timed out on ruby 4.0.7, timeout: 1000 ms)
          |Checker      : automaton
          |
          |Refuted recalls:
          |- Complexity   : 2nd degree polynomial
          |  Attack string: "a" * 2 + "!"
          |  Recall       : refuted (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)
          |- Complexity   : 2nd degree polynomial
          |  Attack string: "a" * 2 + "!"
          |  Recall       : refuted (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Safe(
          "a*a*$",
          "",
          Dialect.Ruby(),
          UnitMode.CodePoint,
          AttackComplexity.Linear,
          Seq(refutedRecall),
          Checker.Fuzz
        )
        .toString,
      s"""|Input     : /a*a*$$/
          |Dialect   : ruby (encoding: UTF-8)
          |Status    : safe
          |Complexity: linear
          |Checker   : fuzz
          |
          |Refuted recalls:
          |- Complexity   : 2nd degree polynomial
          |  Attack string: "a" * 2 + "!"
          |  Recall       : refuted (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Unknown(
          "",
          "",
          Dialect.JavaScript,
          Some(UnitMode.UTF16),
          Diagnostics.ErrorKind.Timeout,
          Seq.empty,
          Some(Checker.Automaton)
        )
        .toString,
      s"""|Input  : //
          |Dialect: javascript
          |Status : unknown
          |Error  : timeout
          |Checker: automaton""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Unknown(
          "",
          "",
          Dialect.Perl(utf8 = false, source = PerlSource.String),
          None,
          Diagnostics.ErrorKind.Timeout,
          Seq.empty,
          None
        )
        .toString,
      s"""|Input  : //
          |Dialect: perl (utf8: false, source: string)
          |Status : unknown
          |Error  : timeout
          |Checker: (none)""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Unknown(
          "a*a*$",
          "",
          Dialect.Ruby(),
          Some(UnitMode.CodePoint),
          Diagnostics.ErrorKind.RecallRefutationLimit,
          Seq(refutedRecall),
          Some(Checker.Fuzz)
        )
        .toString,
      s"""|Input  : /a*a*$$/
          |Dialect: ruby (encoding: UTF-8)
          |Status : unknown
          |Error  : recall refutation limit
          |Checker: fuzz
          |
          |Refuted recalls:
          |- Complexity   : 2nd degree polynomial
          |  Attack string: "a" * 2 + "!"
          |  Recall       : refuted (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)""".stripMargin
    )

  test("Diagnostics.Unknown: refuted recalls require the unit mode"):
    intercept[IllegalArgumentException]:
      Diagnostics.Unknown("", "", Dialect.JavaScript, None, Diagnostics.ErrorKind.Timeout, Seq(refutedRecall), None)

  test("Diagnostics.Unknown.from"):
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedRecall), new TimeoutException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.Timeout,
        Seq(refutedRecall),
        None
      )
    )
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedRecall), new CancelException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.Cancel,
        Seq(refutedRecall),
        None
      )
    )
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedRecall), new InvalidRegExpException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.InvalidRegExp("foo"),
        Seq(refutedRecall),
        None
      )
    )
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedRecall), new UnsupportedException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.Unsupported("foo"),
        Seq(refutedRecall),
        None
      )
    )
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedRecall), new UnexpectedException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.Unexpected("foo"),
        Seq(refutedRecall),
        None
      )
    )

  test("Diagnostics.ErrorKind#toString"):
    assertEquals(Diagnostics.ErrorKind.Timeout.toString, "timeout")
    assertEquals(Diagnostics.ErrorKind.Cancel.toString, "cancel")
    assertEquals(Diagnostics.ErrorKind.Unsupported("foo").toString, "unsupported (foo)")
    assertEquals(Diagnostics.ErrorKind.InvalidRegExp("foo").toString, "invalid RegExp (foo)")
    assertEquals(Diagnostics.ErrorKind.RecallRefutationLimit.toString, "recall refutation limit")
