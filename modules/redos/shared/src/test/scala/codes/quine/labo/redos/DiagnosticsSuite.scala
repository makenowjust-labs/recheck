package codes.quine.labo.redos

import automaton.Complexity
import automaton.Witness
import common.InvalidRegExpException
import common.TimeoutException
import common.UnsupportedException
import data.UChar
import data.UString

class DiagnosticsSuite extends munit.FunSuite {
  test("Diagnostics#complexity") {
    assertEquals(Diagnostics.Safe(Some(Complexity.Constant), None).complexity, Some(Complexity.Constant))
    assertEquals(Diagnostics.Safe(None, None).complexity, None)
    val w = Witness(Seq((Seq(UChar('a')), Seq(UChar('a')))), Seq(UChar('a')))
    assertEquals(
      Diagnostics.Vulnerable(UString.empty, Some(Complexity.Exponential(w)), None).complexity,
      Some(Complexity.Exponential(w))
    )
    assertEquals(Diagnostics.Vulnerable(UString.empty, None, None).complexity, None)
    assertEquals(Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout, None).complexity, None)
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
