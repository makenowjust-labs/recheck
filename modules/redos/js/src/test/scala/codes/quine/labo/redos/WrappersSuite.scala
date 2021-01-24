package codes.quine.labo.redos

import scalajs.js
import scalajs.js.JSON
import automaton.Complexity
import automaton.Witness
import common.Checker
import data.UChar
import data.UString

class WrappersSuite extends munit.FunSuite {
  test("DiagnosticsJS.from") {
    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Safe(Some(Complexity.Linear), None))),
      JSON.stringify(js.Dynamic.literal(status = "safe", used = (), complexity = js.Dynamic.literal(`type` = "linear")))
    )
    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Safe(None, None))),
      JSON.stringify(js.Dynamic.literal(status = "safe", used = (), complexity = ()))
    )

    val w = Witness(Seq((Seq(UChar('a')), Seq(UChar('b')))), Seq(UChar('c')))
    val j = js.Dynamic.literal(pumps = js.Array(js.Dynamic.literal(prefix = "a", pump = "b")), suffix = "c")
    assertEquals(
      JSON.stringify(
        DiagnosticsJS.from(Diagnostics.Vulnerable(UString.from("a", false), Some(Complexity.Exponential(w)), None))
      ),
      JSON.stringify(
        js.Dynamic.literal(
          status = "vulnerable",
          used = (),
          attack = "a",
          complexity = js.Dynamic.literal(`type` = "exponential", witness = j)
        )
      )
    )

    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout, None))),
      JSON.stringify(js.Dynamic.literal(status = "unknown", used = (), error = js.Dynamic.literal(kind = "timeout")))
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

    val w = Witness(Seq((Seq(UChar('a')), Seq(UChar('b')))), Seq(UChar('c')))
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
      JSON.stringify(WitnessJS.from(Witness(Seq((Seq(UChar('a')), Seq(UChar('b')))), Seq(UChar('c'))))),
      JSON.stringify(js.Dynamic.literal(pumps = js.Array(js.Dynamic.literal(prefix = "a", pump = "b")), suffix = "c"))
    )
  }

  test("PumpJS.from") {
    assertEquals(
      JSON.stringify(PumpJS.from((Seq(UChar('a')), Seq(UChar('a'))))),
      JSON.stringify(js.Dynamic.literal(prefix = "a", pump = "a"))
    )
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

  test("ConfigJS.from") {
    assertEquals(ConfigJS.from(js.Dynamic.literal().asInstanceOf[ConfigJS]), Config())

    assertEquals(ConfigJS.from(js.Dynamic.literal(checker = "hybrid").asInstanceOf[ConfigJS]).checker, Checker.Hybrid)
    assertEquals(
      ConfigJS.from(js.Dynamic.literal(checker = "automaton").asInstanceOf[ConfigJS]).checker,
      Checker.Automaton
    )
    assertEquals(ConfigJS.from(js.Dynamic.literal(checker = "fuzz").asInstanceOf[ConfigJS]).checker, Checker.Fuzz)
  }
}
