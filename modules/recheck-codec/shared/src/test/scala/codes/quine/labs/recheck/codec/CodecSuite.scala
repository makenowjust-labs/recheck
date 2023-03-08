package codes.quine.labs.recheck.codec

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Json
import io.circe.syntax._

import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.Seeder
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.Diagnostics.ErrorKind
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.unicode.UString

class CodecSuite extends munit.FunSuite {
  test("codec.encodeDiagnostics") {
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

  test("codec.encodeChecker") {
    assertEquals(encodeChecker(Checker.Auto), "auto".asJson)
    assertEquals(encodeChecker(Checker.Fuzz), "fuzz".asJson)
    assertEquals(encodeChecker(Checker.Automaton), "automaton".asJson)
  }

  test("codec.encodeAttackComplexity") {
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

  test("codec.encodeAttackPattern") {
    assertEquals(
      encodeAttackPattern(AttackPattern(Seq((UString("a"), UString("b"), 2)), UString("c"), 0)),
      Json.obj(
        "pumps" := Json.arr(Json.obj("prefix" := "a", "pump" := "b", "bias" := 2)),
        "suffix" := "c",
        "base" := 0,
        "string" := "abbc",
        "pattern" := "'a' + 'b'.repeat(2) + 'c'"
      )
    )
  }

  test("codec.encodeHotspot") {
    assertEquals(
      encodeHotspot(Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Heat)))),
      Json.arr(Json.obj("start" := 1, "end" := 2, "temperature" := "heat"))
    )
  }

  test("codec.encodeErrorKind") {
    assertEquals(encodeErrorKind(ErrorKind.Timeout), Json.obj("kind" := "timeout"))
    assertEquals(encodeErrorKind(ErrorKind.Cancel), Json.obj("kind" := "cancel"))
    assertEquals(encodeErrorKind(ErrorKind.Unsupported("foo")), Json.obj("kind" := "unsupported", "message" := "foo"))
    assertEquals(encodeErrorKind(ErrorKind.InvalidRegExp("foo")), Json.obj("kind" := "invalid", "message" := "foo"))
    assertEquals(encodeErrorKind(ErrorKind.Unexpected("foo")), Json.obj("kind" := "unexpected", "message" := "foo"))
  }

  test("codec.encodeUString") {
    assertEquals(encodeUString(UString("foo")), Json.fromString("foo"))
  }

  test("codec.decodeParameters") {

    implicit val decodeLogger: Decoder[Context.Logger] =
      Decoder.decodeUnit.map(_ => null.asInstanceOf[Context.Logger])

    assertEquals(decodeParameters.decodeJson(Json.obj()), Right(Parameters()))
    assertEquals(
      decodeParameters.decodeJson(
        Json.obj(
          "attackTimeout" := Json.Null,
          "incubationTimeout" := Json.Null,
          "recallTimeout" := Json.Null,
          "seedingTimeout" := Json.Null
        )
      ),
      Right(
        Parameters(
          attackTimeout = Duration.Inf,
          incubationTimeout = Duration.Inf,
          recallTimeout = Duration.Inf,
          seedingTimeout = Duration.Inf
        )
      )
    )
    assertEquals(
      decodeParameters.decodeJson(
        Json.obj(
          "checker" := "fuzz",
          "timeout" := 123,
          "logger" := Json.arr(),
          "maxAttackStringSize" := 123,
          "attackLimit" := 123,
          "randomSeed" := 123,
          "maxIteration" := 123,
          "seeder" := "dynamic",
          "maxSimpleRepeatCount" := 123,
          "seedingLimit" := 123,
          "seedingTimeout" := 123,
          "maxInitialGenerationSize" := 123,
          "incubationLimit" := 123,
          "incubationTimeout" := 123,
          "maxGeneStringSize" := 123,
          "maxGenerationSize" := 123,
          "crossoverSize" := 123,
          "mutationSize" := 123,
          "attackTimeout" := 123,
          "maxDegree" := 123,
          "heatRatio" := 0.123,
          "accelerationMode" := "off",
          "maxRepeatCount" := 123,
          "maxNFASize" := 123,
          "maxPatternSize" := 123
        )
      ),
      Right(
        Parameters(
          checker = Checker.Fuzz,
          timeout = Duration(123, MILLISECONDS),
          logger = Some(null),
          maxAttackStringSize = 123,
          attackLimit = 123,
          randomSeed = 123,
          maxIteration = 123,
          seeder = Seeder.Dynamic,
          maxSimpleRepeatCount = 123,
          seedingLimit = 123,
          seedingTimeout = Duration(123, MILLISECONDS),
          maxInitialGenerationSize = 123,
          incubationLimit = 123,
          incubationTimeout = Duration(123, MILLISECONDS),
          maxGeneStringSize = 123,
          maxGenerationSize = 123,
          crossoverSize = 123,
          mutationSize = 123,
          attackTimeout = Duration(123, MILLISECONDS),
          maxDegree = 123,
          heatRatio = 0.123,
          accelerationMode = AccelerationMode.Off,
          maxRepeatCount = 123,
          maxNFASize = 123,
          maxPatternSize = 123
        )
      )
    )
  }

  test("codec.decodeDuration") {
    assertEquals(decodeDuration.decodeJson(Json.Null), Right(Duration.Inf))
    assertEquals(decodeDuration.decodeJson(100.asJson), Right(Duration(100, MILLISECONDS)))
    assertEquals(decodeDuration.decodeJson("100".asJson), Right(Duration(100, MILLISECONDS)))
    assertEquals(decodeDuration.decodeJson(Json.True), Left(DecodingFailure("Duration", List.empty)))
    assertEquals(decodeDuration.decodeJson(123.456.asJson), Left(DecodingFailure("Duration", List.empty)))
    assertEquals(decodeDuration.decodeJson("123.456".asJson), Left(DecodingFailure("Duration", List.empty)))
  }

  test("codec.decodeChecker") {
    assertEquals(decodeChecker.decodeJson("auto".asJson), Right(Checker.Auto))
    assertEquals(decodeChecker.decodeJson("automaton".asJson), Right(Checker.Automaton))
    assertEquals(decodeChecker.decodeJson("fuzz".asJson), Right(Checker.Fuzz))
    assertEquals(decodeChecker.decodeJson("xxx".asJson), Left(DecodingFailure("Unknown checker: xxx", List.empty)))
  }

  test("codec.decodeAccelerationMode") {
    assertEquals(decodeAccelerationMode.decodeJson("auto".asJson), Right(AccelerationMode.Auto))
    assertEquals(decodeAccelerationMode.decodeJson("on".asJson), Right(AccelerationMode.On))
    assertEquals(decodeAccelerationMode.decodeJson("off".asJson), Right(AccelerationMode.Off))
    assertEquals(
      decodeAccelerationMode.decodeJson("xxx".asJson),
      Left(DecodingFailure("Unknown acceleration mode: xxx", List.empty))
    )
  }

  test("codec.decodeSeeder") {
    assertEquals(decodeSeeder.decodeJson("static".asJson), Right(Seeder.Static))
    assertEquals(decodeSeeder.decodeJson("dynamic".asJson), Right(Seeder.Dynamic))
    assertEquals(decodeSeeder.decodeJson("xxx".asJson), Left(DecodingFailure("Unknown seeder: xxx", List.empty)))
  }
}
