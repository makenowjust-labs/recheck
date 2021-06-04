package codes.quine.labo.recheck.cli

import io.circe.DecodingFailure
import io.circe.Json
import io.circe.syntax._

import codes.quine.labo.recheck.cli.codecs._
import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.diagnostics.Diagnostics
import codes.quine.labo.recheck.diagnostics.Diagnostics.ErrorKind
import codes.quine.labo.recheck.diagnostics.Hotspot
import codes.quine.labo.recheck.unicode.UString

class CodecsSuite extends munit.FunSuite {
  test("codecs.encodeDiagnostics") {
    assertEquals(
      encodeDiagnostics(Diagnostics.Safe("a", "", AttackComplexity.Linear, Checker.Fuzz)),
      Json.obj(
        "source" := "a",
        "flags" := "",
        "status" := "safe",
        "checker" := Checker.Fuzz.asInstanceOf[Checker],
        "complexity" := AttackComplexity.Linear.asInstanceOf[AttackComplexity]
      )
    )
    assertEquals(
      encodeDiagnostics(
        Diagnostics.Vulnerable(
          "(a|a)*$",
          "",
          AttackComplexity.Exponential(false),
          AttackPattern(Seq((UString(""), UString("a"), 0)), UString("\u0000"), 17),
          Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Heat), Hotspot.Spot(3, 4, Hotspot.Heat))),
          Checker.Automaton
        )
      ),
      Json.obj(
        "source" := "(a|a)*$",
        "flags" := "",
        "status" := "vulnerable",
        "checker" := Checker.Automaton.asInstanceOf[Checker],
        "complexity" := AttackComplexity.Exponential(false).asInstanceOf[AttackComplexity],
        "attack" := AttackPattern(Seq((UString(""), UString("a"), 0)), UString("\u0000"), 17),
        "hotspot" := Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Heat), Hotspot.Spot(3, 4, Hotspot.Heat)))
      )
    )
    assertEquals(
      encodeDiagnostics(Diagnostics.Unknown("", "#", ErrorKind.InvalidRegExp("unknown flag"), None)),
      Json.obj(
        "source" := "",
        "flags" := "#",
        "status" := "unknown",
        "checker" := None,
        "error" := ErrorKind.InvalidRegExp("unknown flag").asInstanceOf[ErrorKind]
      )
    )
  }

  test("codecs.encodeChecker") {
    assertEquals(encodeChecker(Checker.Hybrid), "hybrid".asJson)
    assertEquals(encodeChecker(Checker.Fuzz), "fuzz".asJson)
    assertEquals(encodeChecker(Checker.Automaton), "automaton".asJson)
  }

  test("codecs.encodeAttackComplexity") {
    assertEquals(
      encodeAttackComplexity(AttackComplexity.Constant),
      Json.obj("type" := "constant", "summary" := "constant", "isFuzz" := false)
    )
    assertEquals(
      encodeAttackComplexity(AttackComplexity.Linear),
      Json.obj("type" := "linear", "summary" := "linear", "isFuzz" := false)
    )
    assertEquals(
      encodeAttackComplexity(AttackComplexity.Safe(true)),
      Json.obj("type" := "safe", "summary" := "safe (fuzz)", "isFuzz" := true)
    )
    assertEquals(
      encodeAttackComplexity(AttackComplexity.Polynomial(2, false)),
      Json.obj("type" := "polynomial", "degree" := 2, "summary" := "2nd degree polynomial", "isFuzz" := false)
    )
    assertEquals(
      encodeAttackComplexity(AttackComplexity.Exponential(false)),
      Json.obj("type" := "exponential", "summary" := "exponential", "isFuzz" := false)
    )
  }

  test("codecs.encodeAttackPattern") {
    assertEquals(
      encodeAttackPattern(AttackPattern(Seq((UString("a"), UString("b"), 1)), UString("c"), 0)),
      Json.obj(
        "pumps" := Json.arr(Json.obj("prefix" := "a", "pump" := "b", "bias" := 1)),
        "suffix" := "c",
        "base" := 0,
        "string" := "abc",
        "pattern" := "'a' 'b'¹ 'c'"
      )
    )
  }

  test("codecs.encodeHotspot") {
    assertEquals(
      encodeHotspot(Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Heat)))),
      Json.arr(Json.obj("start" := 1, "end" := 2, "temperature" := "heat"))
    )
  }

  test("codecs.encodeErrorKind") {
    assertEquals(encodeErrorKind(ErrorKind.Timeout), Json.obj("kind" -> "timeout".asJson))
    assertEquals(encodeErrorKind(ErrorKind.Unsupported("foo")), Json.obj("kind" := "unsupported", "message" := "foo"))
    assertEquals(encodeErrorKind(ErrorKind.InvalidRegExp("foo")), Json.obj("kind" := "invalid", "message" := "foo"))
  }

  test("codecs.encodeUString") {
    assertEquals(encodeUString(UString("foo")), Json.fromString("foo"))
  }

  test("codecs.decodeChecker") {
    assertEquals(decodeChecker.decodeJson("hybrid".asJson), Right(Checker.Hybrid))
    assertEquals(decodeChecker.decodeJson("automaton".asJson), Right(Checker.Automaton))
    assertEquals(decodeChecker.decodeJson("fuzz".asJson), Right(Checker.Fuzz))
    assertEquals(decodeChecker.decodeJson("xxx".asJson), Left(DecodingFailure("Unknown checker: xxx", List.empty)))
  }
}
