package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.common.CancelException
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.unicode.UString

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
          AttackPattern(Seq((UString("a"), UString("b"), 0)), UString("c"), 2),
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
