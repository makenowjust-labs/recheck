package codes.quine.labo.recheck
package diagnostics

import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.common.TimeoutException
import codes.quine.labo.recheck.common.UnsupportedException
import codes.quine.labo.recheck.data.UString

class DiagnosticsSuite extends munit.FunSuite {
  test("Diagnostics#toString") {
    assertEquals(
      Diagnostics.Safe(AttackComplexity.Constant, Checker.Automaton).toString,
      s"""|Status    : safe
          |Complexity: constant
          |Checker   : automaton""".stripMargin
    )
    assertEquals(
      Diagnostics
        .Vulnerable(
          AttackComplexity.Exponential(false),
          AttackPattern(Seq((UString.from("a", false), UString.from("b", false), 0)), UString.from("c", false), 2),
          Checker.Automaton
        )
        .toString,
      s"""|Status       : vulnerable
          |Complexity   : exponential
          |Attack string: 'a' 'b'² 'c'
          |Checker      : automaton""".stripMargin
    )
    assertEquals(
      Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout, Some(Checker.Automaton)).toString,
      s"""|Status : unknown
          |Error  : timeout
          |Checker: automaton""".stripMargin
    )
    assertEquals(
      Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout, None).toString,
      s"""|Status : unknown
          |Error  : timeout
          |Checker: (none)""".stripMargin
    )
  }

  test("Diagnostics.Unknown.from") {
    assertEquals(
      Diagnostics.Unknown.from(new TimeoutException("foo")),
      Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout, None)
    )
    assertEquals(
      Diagnostics.Unknown.from(new InvalidRegExpException("foo")),
      Diagnostics.Unknown(Diagnostics.ErrorKind.InvalidRegExp("foo"), None)
    )
    assertEquals(
      Diagnostics.Unknown.from(new UnsupportedException("foo")),
      Diagnostics.Unknown(Diagnostics.ErrorKind.Unsupported("foo"), None)
    )
  }

  test("Diagnostics.ErrorKind#toString") {
    assertEquals(Diagnostics.ErrorKind.Timeout.toString, "timeout")
    assertEquals(Diagnostics.ErrorKind.Unsupported("foo").toString, "unsupported (foo)")
    assertEquals(Diagnostics.ErrorKind.InvalidRegExp("foo").toString, "invalid RegExp (foo)")
  }
}
