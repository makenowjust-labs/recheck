package codes.quine.labs.recheck.codec

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Json
import io.circe.syntax.*

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
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class CodecSuite extends munit.FunSuite:
  test("codec.encodeDiagnostics"):
    assertEquals(
      encodeDiagnostics(Diagnostics.Safe("a", "", Dialect.JavaScript, AttackComplexity.Linear, Checker.Fuzz)),
      Json.obj(
        "source" := "a",
        "flags" := "",
        "dialect" := Json.obj("name" := "javascript"),
        "status" := "safe",
        "complexity" := AttackComplexity.Linear.asInstanceOf[AttackComplexity],
        "checker" := Checker.Fuzz.asInstanceOf[Checker]
      )
    )
    assertEquals(
      encodeDiagnostics(
        Diagnostics.Vulnerable(
          "(a|a)*$",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)),
            UnitMode.UTF16.decode("\u0000"),
            17
          ),
          Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Heat), Hotspot.Spot(3, 4, Hotspot.Heat))),
          None,
          Checker.Automaton
        )
      ),
      Json.obj(
        "source" := "(a|a)*$",
        "flags" := "",
        "dialect" := Json.obj("name" := "javascript"),
        "unitMode" := "utf16",
        "status" := "vulnerable",
        "complexity" := AttackComplexity.Exponential(false).asInstanceOf[AttackComplexity],
        "attack" -> encodeAttackPattern(Dialect.JavaScript, UnitMode.UTF16)(
          AttackPattern(
            Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)),
            UnitMode.UTF16.decode("\u0000"),
            17
          )
        ),
        "hotspot" := Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Heat), Hotspot.Spot(3, 4, Hotspot.Heat))),
        "recallValidation" := Json.Null,
        "checker" := Checker.Automaton.asInstanceOf[Checker]
      )
    )
    val recall = RecallValidation("python 3.15.0", Duration(1000, MILLISECONDS), RecallResult.Timeout)
    assertEquals(
      encodeDiagnostics(
        Diagnostics.Vulnerable(
          "(a|a)*$",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Exponential(false),
          AttackPattern(Seq.empty, UnitMode.UTF16.decode(""), 0),
          Hotspot(Seq.empty),
          Some(recall),
          Checker.Automaton
        )
      ).hcursor.downField("recallValidation").focus,
      Some(encodeRecallValidation(recall))
    )
    assertEquals(
      encodeDiagnostics(
        Diagnostics.Unknown("", "#", Dialect.JavaScript, ErrorKind.InvalidRegExp("unknown flag"), None)
      ),
      Json.obj(
        "source" := "",
        "flags" := "#",
        "dialect" := Json.obj("name" := "javascript"),
        "status" := "unknown",
        "error" := ErrorKind.InvalidRegExp("unknown flag").asInstanceOf[ErrorKind],
        "checker" := None
      )
    )

  test("codec.encodeUnitMode"):
    assertEquals(encodeUnitMode(UnitMode.UTF16), "utf16".asJson)
    assertEquals(encodeUnitMode(UnitMode.CodePoint), "code-point".asJson)
    assertEquals(encodeUnitMode(UnitMode.Byte), "byte".asJson)

  test("codec.decodeUnitMode"):
    for mode <- UnitMode.values do assertEquals(decodeUnitMode.decodeJson(encodeUnitMode(mode)), Right(mode))
    assertEquals(decodeUnitMode.decodeJson("ucs2".asJson).left.map(_.message), Left("Unknown unit mode: ucs2"))

  test("codec.encodeChecker"):
    assertEquals(encodeChecker(Checker.Auto), "auto".asJson)
    assertEquals(encodeChecker(Checker.Fuzz), "fuzz".asJson)
    assertEquals(encodeChecker(Checker.Automaton), "automaton".asJson)

  test("codec.encodeAttackComplexity"):
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

  test("codec.encodeAttackPattern"):
    val attack = AttackPattern(
      Seq((UnitMode.UTF16.decode("a"), UnitMode.UTF16.decode("b"), 2)),
      UnitMode.UTF16.decode("c"),
      0
    )
    assertEquals(
      encodeAttackPattern(Dialect.JavaScript, UnitMode.UTF16)(attack),
      Json.obj(
        "pumps" := Json.arr(Json.obj("prefix" := "a", "pump" := "b", "bias" := 2)),
        "suffix" := "c",
        "base" := 0,
        "string" := "abbc",
        "pattern" := "'a' + 'b'.repeat(2) + 'c'"
      )
    )
    assertEquals(
      encodeAttackPattern(Dialect.Python(), UnitMode.CodePoint)(attack).hcursor.get[String]("pattern"),
      Right("'a' + 'b' * 2 + 'c'")
    )
    // The units of the mode are written in the string of UTF-16.
    val astral = AttackPattern(Seq.empty, UString(0x1f600), 0)
    assertEquals(
      encodeAttackPattern(Dialect.JavaScript, UnitMode.CodePoint)(astral).hcursor.get[String]("string"),
      Right("😀")
    )
    val utf16 = AttackPattern(Seq.empty, UString(0xd83d, 0xde00), 0)
    assertEquals(
      encodeAttackPattern(Dialect.JavaScript, UnitMode.UTF16)(utf16).hcursor.get[String]("string"),
      Right("😀")
    )

  test("codec.encodeHotspot"):
    assertEquals(
      encodeHotspot(Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Heat)))),
      Json.arr(Json.obj("start" := 1, "end" := 2, "temperature" := "heat"))
    )

  test("codec.encodeRecallValidation"):
    assertEquals(
      encodeRecallValidation(RecallValidation("node v26.11.1", Duration(1000, MILLISECONDS), RecallResult.Timeout)),
      Json.obj(
        "runtime" := "node v26.11.1",
        "timeout" := 1000,
        "result" := Json.obj("kind" := "timeout"),
        "isSucceeded" := true
      )
    )
    assertEquals(
      encodeRecallValidation(
        RecallValidation("ruby 4.0.7", Duration.Inf, RecallResult.Finish(Duration(3, MILLISECONDS)))
      ),
      Json.obj(
        "runtime" := "ruby 4.0.7",
        "timeout" := Json.Null,
        "result" := Json.obj("kind" := "finish", "time" := 3),
        "isSucceeded" := false
      )
    )
    assertEquals(
      encodeRecallResult(RecallResult.EngineLimit("Backtrack limit exhausted")),
      Json.obj("kind" := "engine-limit", "message" := "Backtrack limit exhausted")
    )
    assertEquals(
      encodeRecallResult(RecallResult.EngineError("StackOverflowError")),
      Json.obj("kind" := "engine-error", "message" := "StackOverflowError")
    )

  test("codec.encodeErrorKind"):
    assertEquals(encodeErrorKind(ErrorKind.Timeout), Json.obj("kind" := "timeout"))
    assertEquals(encodeErrorKind(ErrorKind.Cancel), Json.obj("kind" := "cancel"))
    assertEquals(encodeErrorKind(ErrorKind.Unsupported("foo")), Json.obj("kind" := "unsupported", "message" := "foo"))
    assertEquals(encodeErrorKind(ErrorKind.InvalidRegExp("foo")), Json.obj("kind" := "invalid", "message" := "foo"))
    assertEquals(encodeErrorKind(ErrorKind.RecallFailureLimit), Json.obj("kind" := "recall-failure-limit"))
    assertEquals(encodeErrorKind(ErrorKind.Unexpected("foo")), Json.obj("kind" := "unexpected", "message" := "foo"))

  test("codec.encodeUString"):
    assertEquals(encodeUString(UnitMode.UTF16)(UString(0x61, 0xd83d, 0xde00)), Json.fromString("a😀"))
    assertEquals(encodeUString(UnitMode.CodePoint)(UString(0x61, 0x1f600)), Json.fromString("a😀"))
    assertEquals(encodeUString(UnitMode.Byte)(UString(0x61, 0xc3, 0xa9)), Json.fromString("a\u00c3\u00a9"))

  test("codec.decodeUString"):
    // Each mode decodes what `encodeUString` gives, including a lone surrogate.
    for (mode, s) <- Seq(
        UnitMode.UTF16 -> UString(0x61, 0xd83d, 0xde00, 0xd800),
        UnitMode.CodePoint -> UString(0x61, 0x1f600, 0xd800),
        UnitMode.Byte -> UString(0x61, 0xc3, 0xa9, 0xff)
      )
    do assertEquals(decodeUString(mode).decodeJson(encodeUString(mode)(s)), Right(s))
    assert(clue(decodeUString(UnitMode.Byte).decodeJson("é".asJson)).isRight)
    assertEquals(
      decodeUString(UnitMode.Byte).decodeJson("\u0100".asJson).left.map(_.message),
      Left("Invalid byte string: \u0100")
    )

  test("codec.decodeAttackPattern"):
    val attack = AttackPattern(
      Seq((UnitMode.CodePoint.decode("x"), UString(0x1f600), -1), (UString.empty, UnitMode.CodePoint.decode("y"), 2)),
      UString(0),
      17
    )
    val decoder = decodeAttackPattern(UnitMode.CodePoint)
    assertEquals(
      decoder.decodeJson(encodeAttackPattern(Dialect.Python(), UnitMode.CodePoint)(attack)),
      Right(attack)
    )
    assertEquals(
      decoder.decodeJson(Json.obj("pumps" := Json.arr(), "suffix" := "a")).left.map(_.pathToRootString),
      Left(Some(".base"))
    )

  test("codec.decodeParameters"):

    given decodeLogger: Decoder[Context.Logger] =
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
          "maxPatternSize" := 123,
          "dialect" := Json.obj("name" := "perl", "utf8" := false, "source" := "string"),
          "maxRecallFailures" := 123
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
          maxPatternSize = 123,
          dialect = Dialect.Perl(utf8 = false, source = PerlSource.String),
          maxRecallFailures = 123
        )
      )
    )
    assertEquals(
      decodeParameters.decodeJson(Json.obj("dialect" := Json.obj("name" := "perl", "source" := Json.Null))),
      Right(Parameters(dialect = Dialect.Perl()))
    )
    assert(decodeParameters.decodeJson(Json.obj("dialect" := "xxx")).isLeft)

  test("codec.decodeDuration"):
    assertEquals(decodeDuration.decodeJson(Json.Null), Right(Duration.Inf))
    assertEquals(decodeDuration.decodeJson(100.asJson), Right(Duration(100, MILLISECONDS)))
    assertEquals(decodeDuration.decodeJson("100".asJson), Right(Duration(100, MILLISECONDS)))
    assertEquals(decodeDuration.decodeJson(Json.True), Left(DecodingFailure("Duration", List.empty)))
    assertEquals(decodeDuration.decodeJson(123.456.asJson), Left(DecodingFailure("Duration", List.empty)))
    assertEquals(decodeDuration.decodeJson("123.456".asJson), Left(DecodingFailure("Duration", List.empty)))

  test("codec.decodeChecker"):
    assertEquals(decodeChecker.decodeJson("auto".asJson), Right(Checker.Auto))
    assertEquals(decodeChecker.decodeJson("automaton".asJson), Right(Checker.Automaton))
    assertEquals(decodeChecker.decodeJson("fuzz".asJson), Right(Checker.Fuzz))
    assertEquals(decodeChecker.decodeJson("xxx".asJson), Left(DecodingFailure("Unknown checker: xxx", List.empty)))

  test("codec.decodeAccelerationMode"):
    assertEquals(decodeAccelerationMode.decodeJson("auto".asJson), Right(AccelerationMode.Auto))
    assertEquals(decodeAccelerationMode.decodeJson("on".asJson), Right(AccelerationMode.On))
    assertEquals(decodeAccelerationMode.decodeJson("off".asJson), Right(AccelerationMode.Off))
    assertEquals(
      decodeAccelerationMode.decodeJson("xxx".asJson),
      Left(DecodingFailure("Unknown acceleration mode: xxx", List.empty))
    )

  test("codec.decodeSeeder"):
    assertEquals(decodeSeeder.decodeJson("static".asJson), Right(Seeder.Static))
    assertEquals(decodeSeeder.decodeJson("dynamic".asJson), Right(Seeder.Dynamic))
    assertEquals(decodeSeeder.decodeJson("xxx".asJson), Left(DecodingFailure("Unknown seeder: xxx", List.empty)))

  test("codec.decodeDialect"):
    assertEquals(decodeDialect.decodeJson("javascript".asJson), Right(Dialect.JavaScript))
    assertEquals(decodeDialect.decodeJson("dotnet".asJson), Right(Dialect.DotNet))
    assertEquals(decodeDialect.decodeJson("pcre".asJson), Right(Dialect.PCRE))
    assertEquals(decodeDialect.decodeJson("python".asJson), Right(Dialect.Python()))
    assertEquals(decodeDialect.decodeJson("ruby".asJson), Right(Dialect.Ruby()))
    assertEquals(decodeDialect.decodeJson("perl".asJson), Right(Dialect.Perl()))
    assertEquals(decodeDialect.decodeJson("xxx".asJson), Left(DecodingFailure("Unknown dialect: xxx", List.empty)))
    assertEquals(decodeDialect.decodeJson(Json.obj("name" := "java")), Right(Dialect.Java))
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "python", "type" := "bytes")),
      Right(Dialect.Python(Dialect.Python.Type.Bytes))
    )
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "ruby", "encoding" := "ASCII-8BIT")),
      Right(Dialect.Ruby(Dialect.Ruby.Encoding.ASCII8Bit))
    )
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "perl", "source" := "string", "utf8" := false)),
      Right(Dialect.Perl(utf8 = false, source = PerlSource.String))
    )
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "perl", "utf8" := Json.Null)),
      Right(Dialect.Perl())
    )
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "xxx")),
      Left(DecodingFailure("Unknown dialect: xxx", List.empty))
    )
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "ruby", "encoding" := "str")),
      Left(DecodingFailure("Unknown encoding of ruby: str", List.empty))
    )
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "perl", "source" := "xxx")),
      Left(DecodingFailure("Unknown source of perl: xxx", List.empty))
    )
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "javascript", "encoding" := "str")),
      Left(DecodingFailure("Unknown setting of javascript: encoding", List.empty))
    )
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "python", "encoding" := "str")),
      Left(DecodingFailure("Unknown setting of python: encoding", List.empty))
    )
    assertEquals(
      decodeDialect.decodeJson(Json.obj("name" := "perl", "encoding" := "bytes")),
      Left(DecodingFailure("Unknown setting of perl: encoding", List.empty))
    )
    assert(decodeDialect.decodeJson(Json.obj("encoding" := "str")).isLeft)
    assert(decodeDialect.decodeJson(Json.obj("name" := "perl", "utf8" := "yes")).isLeft)

  test("codec.encodeDialect"):
    assertEquals(encodeDialect(Dialect.JavaScript), Json.obj("name" := "javascript"))
    assertEquals(encodeDialect(Dialect.DotNet), Json.obj("name" := "dotnet"))
    assertEquals(encodeDialect(Dialect.Python()), Json.obj("name" := "python", "type" := "str"))
    assertEquals(
      encodeDialect(Dialect.Ruby(Dialect.Ruby.Encoding.ASCII8Bit)),
      Json.obj("name" := "ruby", "encoding" := "ASCII-8BIT")
    )
    assertEquals(
      encodeDialect(Dialect.Perl(utf8 = false)),
      Json.obj("name" := "perl", "utf8" := false, "source" := "literal")
    )
