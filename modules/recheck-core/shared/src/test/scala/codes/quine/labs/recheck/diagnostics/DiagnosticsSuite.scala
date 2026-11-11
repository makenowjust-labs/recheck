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
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.PerlSource

class DiagnosticsSuite extends munit.FunSuite:

  val refutedAttack: RefutedAttack = RefutedAttack(
    AttackComplexity.Polynomial(2, false),
    AttackPattern(Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)), UnitMode.UTF16.decode("!"), 2),
    Hotspot.empty,
    RuntimeAttack("ruby 4.0.7", Duration(1, SECONDS), RuntimeAttackResult.Finish(Duration(3, MILLISECONDS)))
  )

  val upperBound: UpperBound =
    UpperBound(AttackComplexity.Exponential(false), Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Hot))))

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
      s"""|Input         : /abbc/
          |Dialect       : javascript
          |Status        : vulnerable
          |Complexity    : exponential
          |Attack string : 'a' + 'b'.repeat(2) + 'c'
          |Hotspot       : /abbc/
          |Checker       : automaton""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Vulnerable(
          "a*a*$",
          "",
          Dialect.Ruby(),
          UnitMode.ScalarValue,
          AttackComplexity.Polynomial(2, false),
          AttackPattern(
            Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)),
            UnitMode.UTF16.decode("!"),
            2
          ),
          Hotspot.empty,
          Some(RuntimeAttack("ruby 4.0.7", Duration(1, SECONDS), RuntimeAttackResult.Timeout)),
          Seq(refutedAttack, refutedAttack),
          Checker.Automaton
        )
        .toString,
      s"""|Input         : /a*a*$$/
          |Dialect       : ruby (encoding: UTF-8)
          |Status        : vulnerable
          |Complexity    : 2nd degree polynomial
          |Attack string : "a" * 2 + "!"
          |Hotspot       : /a*a*$$/
          |Runtime attack: confirmed (timed out on ruby 4.0.7, timeout: 1000 ms)
          |Checker       : automaton
          |
          |Refuted attacks:
          |- Complexity    : 2nd degree polynomial
          |  Attack string : "a" * 2 + "!"
          |  Runtime attack: refuted (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)
          |- Complexity    : 2nd degree polynomial
          |  Attack string : "a" * 2 + "!"
          |  Runtime attack: refuted (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Safe(
          "a*a*$",
          "",
          Dialect.Ruby(),
          UnitMode.ScalarValue,
          AttackComplexity.Linear,
          Seq(refutedAttack),
          Checker.Fuzz
        )
        .toString,
      s"""|Input     : /a*a*$$/
          |Dialect   : ruby (encoding: UTF-8)
          |Status    : safe
          |Complexity: linear
          |Checker   : fuzz
          |
          |Refuted attacks:
          |- Complexity    : 2nd degree polynomial
          |  Attack string : "a" * 2 + "!"
          |  Runtime attack: refuted (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)""".stripMargin
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
        .Unknown("", "", Dialect.Python(), None, Diagnostics.ErrorKind.Cancel, Seq.empty, None)
        .toString,
      s"""|Input  : //
          |Dialect: python (type: str)
          |Status : unknown
          |Error  : cancel
          |Checker: (none)""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Unknown(
          "a*a*$",
          "",
          Dialect.Ruby(),
          Some(UnitMode.ScalarValue),
          Diagnostics.ErrorKind.RuntimeAttacksRefuted,
          Seq(refutedAttack),
          Some(Checker.Fuzz)
        )
        .toString,
      s"""|Input  : /a*a*$$/
          |Dialect: ruby (encoding: UTF-8)
          |Status : unknown
          |Error  : runtime attacks refuted
          |Checker: fuzz
          |
          |Refuted attacks:
          |- Complexity    : 2nd degree polynomial
          |  Attack string : "a" * 2 + "!"
          |  Runtime attack: refuted (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)""".stripMargin
    )
    // The upper bound comes before the refuted attacks, with its hotspot if it is not linear.
    assertEquals(
      Diagnostics
        .Safe(
          "(a|aa)+",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Safe(true),
          Seq(refutedAttack),
          Checker.Fuzz,
          Some(upperBound)
        )
        .toString,
      s"""|Input     : /(a|aa)+/
          |Dialect   : javascript
          |Status    : safe
          |Complexity: safe (fuzz)
          |Checker   : fuzz
          |
          |Upper bound:
          |- Complexity: exponential
          |  Hotspot   : /${upperBound.hotspot.highlight("(a|aa)+")}/
          |
          |Refuted attacks:
          |- Complexity    : 2nd degree polynomial
          |  Attack string : 'a'.repeat(2) + '!'
          |  Runtime attack: refuted (finished in 3 ms on ruby 4.0.7, timeout: 1000 ms)""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Unknown(
          "a+",
          "",
          Dialect.JavaScript,
          Some(UnitMode.UTF16),
          Diagnostics.ErrorKind.Timeout,
          Seq.empty,
          Some(Checker.Fuzz),
          Some(UpperBound(AttackComplexity.Linear, Hotspot.empty))
        )
        .toString,
      s"""|Input  : /a+/
          |Dialect: javascript
          |Status : unknown
          |Error  : timeout
          |Checker: fuzz
          |
          |Upper bound:
          |- Complexity: linear""".stripMargin
    )

  test("Diagnostics.Unknown: refuted attacks require the unit mode"):
    intercept[IllegalArgumentException]:
      Diagnostics.Unknown("", "", Dialect.JavaScript, None, Diagnostics.ErrorKind.Timeout, Seq(refutedAttack), None)

  test("Diagnostics.Unknown.from"):
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedAttack), new TimeoutException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.Timeout,
        Seq(refutedAttack),
        None
      )
    )
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedAttack), new CancelException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.Cancel,
        Seq(refutedAttack),
        None
      )
    )
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedAttack), new InvalidRegExpException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.InvalidRegExp("foo"),
        Seq(refutedAttack),
        None
      )
    )
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedAttack), new UnsupportedException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.Unsupported("foo"),
        Seq(refutedAttack),
        None
      )
    )
    assertEquals(
      Diagnostics.Unknown
        .from("", "", Dialect.JavaScript, Some(UnitMode.UTF16), Seq(refutedAttack), new UnexpectedException("foo")),
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.Unexpected("foo"),
        Seq(refutedAttack),
        None
      )
    )

  test("Diagnostics#withUpperBound"):
    val bound = Some(upperBound)
    val safe =
      Diagnostics.Safe("", "", Dialect.JavaScript, UnitMode.UTF16, AttackComplexity.Safe(true), Seq.empty, Checker.Fuzz)
    assertEquals(safe.withUpperBound(bound), safe.copy(upperBound = bound))
    val vulnerable = Diagnostics.Vulnerable(
      "",
      "",
      Dialect.JavaScript,
      UnitMode.UTF16,
      AttackComplexity.Exponential(true),
      AttackPattern(Seq.empty, UnitMode.UTF16.decode("a"), 0),
      Hotspot.empty,
      None,
      Seq.empty,
      Checker.Fuzz
    )
    assertEquals(vulnerable.withUpperBound(bound), vulnerable.copy(upperBound = bound))
    val unknown =
      Diagnostics.Unknown(
        "",
        "",
        Dialect.JavaScript,
        None,
        Diagnostics.ErrorKind.Timeout,
        Seq.empty,
        Some(Checker.Fuzz)
      )
    assertEquals(unknown.withUpperBound(bound), unknown.copy(upperBound = bound))

  test("Diagnostics.ErrorKind#toString"):
    assertEquals(Diagnostics.ErrorKind.Timeout.toString, "timeout")
    assertEquals(Diagnostics.ErrorKind.Cancel.toString, "cancel")
    assertEquals(Diagnostics.ErrorKind.Unsupported("foo").toString, "unsupported (foo)")
    assertEquals(Diagnostics.ErrorKind.InvalidRegExp("foo").toString, "invalid RegExp (foo)")
    assertEquals(Diagnostics.ErrorKind.RuntimeAttacksRefuted.toString, "runtime attacks refuted")
    assertEquals(Diagnostics.ErrorKind.Unexpected("foo").toString, "unexpected (foo)")
