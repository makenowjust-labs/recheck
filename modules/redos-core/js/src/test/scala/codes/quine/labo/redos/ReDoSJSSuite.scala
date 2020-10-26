package codes.quine.labo.redos

import scala.util.Failure
import scala.util.Success

import scalajs.js
import Checker._
import Checker.Complexity._
import data.IChar
import data.UChar

class ReDoSJSSuite extends munit.FunSuite {
  test("ReDoSJS.convertTry") {
    assertEquals(ReDoSJS.convertTry(Success(Constant)).toMap, Map[String, js.Any]("complexity" -> "constant"))
    intercept[js.JavaScriptException](ReDoSJS.convertTry(Failure(new UnsupportedException("foo"))))
  }

  test("ReDoSJS.convertComplexity") {
    assertEquals(ReDoSJS.convertComplexity(Constant).toMap, Map[String, js.Any]("complexity" -> "constant"))
    assertEquals(ReDoSJS.convertComplexity(Linear).toMap, Map[String, js.Any]("complexity" -> "linear"))
    assertEquals(
      js.JSON.stringify(
        ReDoSJS.convertComplexity(Exponential(Witness(Seq((Seq(IChar('a')), Seq(IChar('a')))), Seq(IChar('a')))))
      ),
      js.JSON.stringify(
        js.Dictionary[js.Any](
          "complexity" -> "exponential",
          "witness" -> js.Dictionary[js.Any](
            "pumps" -> js.Array(js.Dictionary("prefix" -> "a", "pump" -> "a")),
            "suffix" -> "a"
          )
        )
      )
    )
    assertEquals(
      js.JSON.stringify(
        ReDoSJS.convertComplexity(Polynomial(2, Witness(Seq((Seq(IChar('a')), Seq(IChar('a')))), Seq(IChar('a')))))
      ),
      js.JSON.stringify(
        js.Dictionary[js.Any](
          "complexity" -> "polynomial",
          "degree" -> 2,
          "witness" -> js.Dictionary[js.Any](
            "pumps" -> js.Array(js.Dictionary("prefix" -> "a", "pump" -> "a")),
            "suffix" -> "a"
          )
        )
      )
    )
  }

  test("ReDoSJS.convertWitness") {
    assertEquals(
      js.JSON.stringify(ReDoSJS.convertWitness(Witness(Seq((Seq(IChar('a')), Seq(IChar('a')))), Seq(IChar('a'))))),
      js.JSON.stringify(
        js.Dictionary[js.Any](
          "pumps" -> js.Array(js.Dictionary("prefix" -> "a", "pump" -> "a")),
          "suffix" -> "a"
        )
      )
    )
  }

  test("ReDoSJS.converICharSeq") {
    assertEquals(ReDoSJS.convertICharSeq(Seq.empty), "")
    assertEquals(ReDoSJS.convertICharSeq(Seq(IChar('a'), IChar('b'))), "ab")
    assertEquals(ReDoSJS.convertICharSeq(Seq(IChar.range(UChar('A'), UChar('Z')))), "A")
  }

  test("ReDoSJS.check") {
    assertEquals(ReDoSJS.check("^foo$", "").toMap, Map[String, js.Any]("complexity" -> "constant"))
    intercept[js.JavaScriptException](ReDoSJS.check("^foo$", "", 0))
  }
}
