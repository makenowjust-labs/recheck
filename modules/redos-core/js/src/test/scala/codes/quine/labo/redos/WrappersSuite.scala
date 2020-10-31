package codes.quine.labo.redos

import scalajs.js
import scalajs.js.JSON
import automaton.Complexity
import automaton.Witness
import data.IChar
import data.UChar

class WrappersSuite extends munit.FunSuite {
  test("DiagnosticsJS.from") {
    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Safe(Some(Complexity.Linear)))),
      JSON.stringify(js.Dynamic.literal(status = "safe", complexity = js.Dynamic.literal(`type` = "linear")))
    )
    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Safe(None))),
      JSON.stringify(js.Dynamic.literal(status = "safe", complexity = ()))
    )

    val w = Witness(Seq((Seq(IChar('a')), Seq(IChar('b')))), Seq(IChar('c')))
    val j = js.Dynamic.literal(pumps = js.Array(js.Dynamic.literal(prefix = "a", pump = "b")), suffix = "c")
    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Vulnerable(Seq(IChar('a')), Some(Complexity.Exponential(w))))),
      JSON.stringify(
        js.Dynamic.literal(
          status = "vulnerable",
          attack = "a",
          complexity = js.Dynamic.literal(`type` = "exponential", witness = j)
        )
      )
    )

    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout))),
      JSON.stringify(js.Dynamic.literal(status = "unknown", error = js.Dynamic.literal(kind = "timeout")))
    )
  }

  test("ComplexityJS.from") {
    assertEquals(
      JSON.stringify(ComplexityJS.from(Complexity.Constant)),
      JSON.stringify(js.Dynamic.literal(`type` = "constant"))
    )
    assertEquals(
      JSON.stringify(ComplexityJS.from(Complexity.Linear)),
      JSON.stringify(js.Dynamic.literal(`type` = "linear"))
    )

    val w = Witness(Seq((Seq(IChar('a')), Seq(IChar('b')))), Seq(IChar('c')))
    val j = js.Dynamic.literal(pumps = js.Array(js.Dynamic.literal(prefix = "a", pump = "b")), suffix = "c")
    assertEquals(
      JSON.stringify(ComplexityJS.from(Complexity.Exponential(w))),
      JSON.stringify(js.Dynamic.literal(`type` = "exponential", witness = j))
    )
    assertEquals(
      JSON.stringify(ComplexityJS.from(Complexity.Polynomial(2, w))),
      JSON.stringify(js.Dynamic.literal(`type` = "polynomial", degree = 2, witness = j))
    )
  }

  test("WitnessJS.from") {
    assertEquals(
      JSON.stringify(WitnessJS.from(Witness(Seq((Seq(IChar('a')), Seq(IChar('b')))), Seq(IChar('c'))))),
      JSON.stringify(js.Dynamic.literal(pumps = js.Array(js.Dynamic.literal(prefix = "a", pump = "b")), suffix = "c"))
    )
  }

  test("PumpJS.from") {
    assertEquals(
      JSON.stringify(PumpJS.from((Seq(IChar('a')), Seq(IChar('a'))))),
      JSON.stringify(js.Dynamic.literal(prefix = "a", pump = "a"))
    )
  }

  test("IStringJS.from") {
    assertEquals(IStringJS.from(Seq(IChar('a'))), "a")
    assertEquals(IStringJS.from(Seq(IChar('a'), IChar.range(UChar('b'), UChar('c')))), "ab")
  }

  test("ErrorKindJS.from") {
    assertEquals(
      JSON.stringify(ErrorKindJS.from(Diagnostics.ErrorKind.Timeout)),
      JSON.stringify(js.Dynamic.literal(kind = "timeout"))
    )
    assertEquals(
      JSON.stringify(ErrorKindJS.from(Diagnostics.ErrorKind.Unsupported("foo"))),
      JSON.stringify(js.Dynamic.literal(kind = "unsupported", message = "foo"))
    )
    assertEquals(
      JSON.stringify(ErrorKindJS.from(Diagnostics.ErrorKind.InvalidRegExp("foo"))),
      JSON.stringify(js.Dynamic.literal(kind = "invalid", message = "foo"))
    )
  }
}
