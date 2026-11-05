package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Diagnostics.ErrorKind
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

class RxSuite extends munit.FunSuite:

  test("Rx.Safe#toString"):
    assertEquals(
      Rx.Safe("^a+$", "", Dialect.JavaScript).toString,
      """|Input  : /^a+$/
         |Dialect: javascript
         |Status : safe""".stripMargin
    )

  test("Rx.Repaired#toString"):
    val repaired = Rx.Repaired(
      "^(a+)+$",
      "",
      Dialect.JavaScript,
      UnitMode.UTF16,
      "^(a+)$",
      AttackComplexity.Linear,
      Seq(0, 1),
      Rx.Comparison.Equivalent
    )
    assertEquals(
      repaired.toString,
      """|Input     : /^(a+)+$/
         |Dialect   : javascript
         |Status    : repaired
         |Repaired  : /^(a+)$/
         |Complexity: linear
         |Kept      : match, 1
         |Comparison: equivalent""".stripMargin
    )
    assert(repaired.copy(kept = Seq.empty).toString.contains("Kept      : (none)"))

  test("Rx.Unknown#toString"):
    assertEquals(
      Rx.Unknown("^(a|aa)+$", "", Dialect.JavaScript, None).toString,
      """|Input  : /^(a|aa)+$/
         |Dialect: javascript
         |Status : unknown
         |Error  : no repair found""".stripMargin
    )
    assert(Rx.Unknown("(", "", Dialect.JavaScript, Some(ErrorKind.Timeout)).toString.endsWith("Error  : timeout"))

  test("Rx.Comparison#toString"):
    val counterexample = Rx.Counterexample(UString.from("ab".map(_.toInt)), Some(Seq(Some((0, 1)))), None)
    assertEquals(Rx.Comparison.Equivalent.toString(UnitMode.UTF16), "equivalent")
    assertEquals(
      Rx.Comparison.Different(counterexample).toString(UnitMode.UTF16),
      "different on \"ab\" (original: [\"a\"], repaired: no match)"
    )
    assertEquals(Rx.Comparison.Unknown.toString(UnitMode.UTF16), "unknown")

  test("Rx.Counterexample#toString"):
    val input = UString.from("a\"\\".map(_.toInt))
    val counterexample = Rx.Counterexample(input, Some(Seq(Some((0, 3)), None)), Some(Seq(Some((1, 2)), Some((0, 0)))))
    assertEquals(
      counterexample.toString(UnitMode.UTF16),
      "\"a\\\"\\\\\" (original: [\"a\\\"\\\\\", unset], repaired: [\"\\\"\", \"\"])"
    )
