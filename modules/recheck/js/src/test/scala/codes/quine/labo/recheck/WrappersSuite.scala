package codes.quine.labo.recheck

import scala.scalajs.js
import scala.scalajs.js.JSON

import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.data.UString
import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.diagnostics.Diagnostics

class WrappersSuite extends munit.FunSuite {
  test("DiagnosticsJS.from") {
    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Safe(AttackComplexity.Linear, Checker.Automaton))),
      JSON.stringify(
        js.Dynamic.literal(
          status = "safe",
          checker = "automaton",
          complexity = js.Dynamic.literal(`type` = "linear", isFuzz = false)
        )
      )
    )
    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Safe(AttackComplexity.Safe(true), Checker.Fuzz))),
      JSON.stringify(
        js.Dynamic
          .literal(status = "safe", checker = "fuzz", complexity = js.Dynamic.literal(`type` = "safe", isFuzz = true))
      )
    )

    val attack =
      AttackPattern(Seq((UString.from("a", false), UString.from("b", false), 1)), UString.from("c", false), 2)
    val attackJS = js.Dynamic.literal(
      pumps = js.Array(js.Dynamic.literal(prefix = "a", pump = "b", bias = 1)),
      suffix = "c",
      base = 2,
      string = "abbbc"
    )
    assertEquals(
      JSON.stringify(
        DiagnosticsJS.from(Diagnostics.Vulnerable(AttackComplexity.Exponential(false), attack, Checker.Automaton))
      ),
      JSON.stringify(
        js.Dynamic.literal(
          status = "vulnerable",
          checker = "automaton",
          attack = attackJS,
          complexity = js.Dynamic.literal(`type` = "exponential", isFuzz = false)
        )
      )
    )

    assertEquals(
      JSON.stringify(DiagnosticsJS.from(Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout, None))),
      JSON.stringify(js.Dynamic.literal(status = "unknown", checker = (), error = js.Dynamic.literal(kind = "timeout")))
    )
  }

  test("AttackComplexityJS.from") {
    assertEquals(
      JSON.stringify(AttackComplexityJS.from(AttackComplexity.Constant)),
      JSON.stringify(js.Dynamic.literal(`type` = "constant", isFuzz = false))
    )
    assertEquals(
      JSON.stringify(AttackComplexityJS.from(AttackComplexity.Linear)),
      JSON.stringify(js.Dynamic.literal(`type` = "linear", isFuzz = false))
    )
    assertEquals(
      JSON.stringify(AttackComplexityJS.from(AttackComplexity.Safe(true))),
      JSON.stringify(js.Dynamic.literal(`type` = "safe", isFuzz = true))
    )
    assertEquals(
      JSON.stringify(AttackComplexityJS.from(AttackComplexity.Exponential(false))),
      JSON.stringify(js.Dynamic.literal(`type` = "exponential", isFuzz = false))
    )
    assertEquals(
      JSON.stringify(AttackComplexityJS.from(AttackComplexity.Polynomial(2, false))),
      JSON.stringify(js.Dynamic.literal(`type` = "polynomial", degree = 2, isFuzz = false))
    )
  }

  test("AttackPatternJS.from") {
    val attack =
      AttackPattern(Seq((UString.from("a", false), UString.from("b", false), 1)), UString.from("c", false), 2)
    assertEquals(
      JSON.stringify(AttackPatternJS.from(attack)),
      JSON.stringify(
        js.Dynamic.literal(
          pumps = js.Array(js.Dynamic.literal(prefix = "a", pump = "b", bias = 1)),
          suffix = "c",
          base = 2,
          string = "abbbc"
        )
      )
    )
  }

  test("PumpJS.from") {
    assertEquals(
      JSON.stringify(PumpJS.from((UString.from("a", false), UString.from("b", false), 1))),
      JSON.stringify(js.Dynamic.literal(prefix = "a", pump = "b", bias = 1))
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
    val config = Config()
    // Replaces `context` with config's one because it is reference and it has an issue on equality.
    assertEquals(ConfigJS.from(js.Dynamic.literal().asInstanceOf[ConfigJS]).copy(context = config.context), config)

    assertEquals(ConfigJS.from(js.Dynamic.literal(checker = "hybrid").asInstanceOf[ConfigJS]).checker, Checker.Hybrid)
    assertEquals(
      ConfigJS.from(js.Dynamic.literal(checker = "automaton").asInstanceOf[ConfigJS]).checker,
      Checker.Automaton
    )
    assertEquals(ConfigJS.from(js.Dynamic.literal(checker = "fuzz").asInstanceOf[ConfigJS]).checker, Checker.Fuzz)
  }
}
