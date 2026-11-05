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
import codes.quine.labs.recheck.diagnostics.RefutedAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.recheck.diagnostics.UpperBound
import codes.quine.labs.recheck.repair.Keep
import codes.quine.labs.recheck.repair.Rx
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.PerlSource

class CodecSuite extends munit.FunSuite:

  val refutedAttack: RefutedAttack = RefutedAttack(
    AttackComplexity.Polynomial(2, false),
    AttackPattern(Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)), UnitMode.UTF16.decode("!"), 2),
    Hotspot(Seq(Hotspot.Spot(0, 2, Hotspot.Hot))),
    RuntimeAttack("python 3.15.0", Duration(1000, MILLISECONDS), RuntimeAttackResult.Finish(Duration(3, MILLISECONDS)))
  )

  test("codec.encodeDiagnostics"):
    assertEquals(
      encodeDiagnostics(
        Diagnostics.Safe("a", "", Dialect.JavaScript, UnitMode.UTF16, AttackComplexity.Linear, Seq.empty, Checker.Fuzz)
      ),
      Json.obj(
        "source" := "a",
        "flags" := "",
        "dialect" := Json.obj("name" := "javascript"),
        "unitMode" := "utf16",
        "status" := "safe",
        "complexity" := AttackComplexity.Linear.asInstanceOf[AttackComplexity],
        "upperBound" := Json.Null,
        "refutedAttacks" := Json.arr(),
        "checker" := Checker.Fuzz.asInstanceOf[Checker]
      )
    )
    val upperBound = UpperBound(AttackComplexity.Polynomial(2, false), Hotspot(Seq(Hotspot.Spot(0, 1, Hotspot.Hot))))
    assertEquals(
      encodeDiagnostics(
        Diagnostics.Safe(
          "a*a*!",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Safe(true),
          Seq.empty,
          Checker.Fuzz,
          Some(upperBound)
        )
      ).hcursor.downField("upperBound").focus,
      Some(
        Json.obj(
          "complexity" := Json.obj(
            "type" := "polynomial",
            "degree" := 2,
            "summary" := "2nd degree polynomial",
            "isFuzz" := false
          ),
          "hotspot" := Json.arr(Json.obj("start" := 0, "end" := 1, "temperature" := "hot"))
        )
      )
    )
    assertEquals(
      encodeDiagnostics(
        Diagnostics.Safe(
          "a*a*!",
          "",
          Dialect.Python(),
          UnitMode.CodePoint,
          AttackComplexity.Safe(true),
          Seq(refutedAttack),
          Checker.Fuzz
        )
      ).hcursor.downField("refutedAttacks").focus,
      Some(Json.arr(encodeRefutedAttack(Dialect.Python(), UnitMode.CodePoint)(refutedAttack)))
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
          Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Hot), Hotspot.Spot(3, 4, Hotspot.Hot))),
          None,
          Seq.empty,
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
        "hotspot" := Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Hot), Hotspot.Spot(3, 4, Hotspot.Hot))),
        "runtimeAttack" := Json.Null,
        "upperBound" := Json.Null,
        "refutedAttacks" := Json.arr(),
        "checker" := Checker.Automaton.asInstanceOf[Checker]
      )
    )
    val runtimeAttack = RuntimeAttack("python 3.15.0", Duration(1000, MILLISECONDS), RuntimeAttackResult.Timeout)
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
          Some(runtimeAttack),
          Seq.empty,
          Checker.Automaton
        )
      ).hcursor.downField("runtimeAttack").focus,
      Some(encodeRuntimeAttack(runtimeAttack))
    )
    assertEquals(
      encodeDiagnostics(
        Diagnostics.Unknown("", "#", Dialect.JavaScript, None, ErrorKind.InvalidRegExp("unknown flag"), Seq.empty, None)
      ),
      Json.obj(
        "source" := "",
        "flags" := "#",
        "dialect" := Json.obj("name" := "javascript"),
        "unitMode" := Json.Null,
        "status" := "unknown",
        "error" := ErrorKind.InvalidRegExp("unknown flag").asInstanceOf[ErrorKind],
        "upperBound" := Json.Null,
        "refutedAttacks" := Json.arr(),
        "checker" := None
      )
    )
    assertEquals(
      encodeDiagnostics(
        Diagnostics.Unknown(
          "a*a*!",
          "",
          Dialect.JavaScript,
          Some(UnitMode.UTF16),
          ErrorKind.RuntimeAttacksRefuted,
          Seq(refutedAttack),
          Some(Checker.Fuzz),
          Some(upperBound)
        )
      ),
      Json.obj(
        "source" := "a*a*!",
        "flags" := "",
        "dialect" := Json.obj("name" := "javascript"),
        "unitMode" := "utf16",
        "status" := "unknown",
        "error" := Json.obj("kind" := "runtime-attacks-refuted"),
        "upperBound" := encodeUpperBound(upperBound),
        "refutedAttacks" := Json.arr(encodeRefutedAttack(Dialect.JavaScript, UnitMode.UTF16)(refutedAttack)),
        "checker" := "fuzz"
      )
    )

  test("codec.encodeRefutedAttack"):
    assertEquals(
      encodeRefutedAttack(Dialect.Python(), UnitMode.CodePoint)(refutedAttack),
      Json.obj(
        "complexity" := Json.obj(
          "type" := "polynomial",
          "degree" := 2,
          "summary" := "2nd degree polynomial",
          "isFuzz" := false
        ),
        "attack" := Json.obj(
          "pumps" := Json.arr(Json.obj("prefix" := "", "pump" := "a", "bias" := 0)),
          "suffix" := "!",
          "base" := 2,
          "string" := "aa!",
          "pattern" := "'a' * 2 + '!'"
        ),
        "hotspot" := Json.arr(Json.obj("start" := 0, "end" := 2, "temperature" := "hot")),
        "runtimeAttack" := Json.obj(
          "runtime" := "python 3.15.0",
          "timeout" := 1000,
          "result" := Json.obj("kind" := "finish", "time" := 3),
          "isConfirmed" := false
        )
      )
    )

  test("codec.encodeUnitMode"):
    assertEquals(encodeUnitMode(UnitMode.UTF16), "utf16".asJson)
    assertEquals(encodeUnitMode(UnitMode.UTF16CodePoint), "utf16-code-point".asJson)
    assertEquals(encodeUnitMode(UnitMode.CodePoint), "code-point".asJson)
    assertEquals(encodeUnitMode(UnitMode.ScalarValue), "scalar-value".asJson)
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
      encodeAttackPattern(Dialect.JavaScript, UnitMode.UTF16CodePoint)(astral).hcursor.get[String]("string"),
      Right("😀")
    )
    val utf16 = AttackPattern(Seq.empty, UString(0xd83d, 0xde00), 0)
    assertEquals(
      encodeAttackPattern(Dialect.JavaScript, UnitMode.UTF16)(utf16).hcursor.get[String]("string"),
      Right("😀")
    )

  test("codec.encodeHotspot"):
    assertEquals(
      encodeHotspot(Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Hot)))),
      Json.arr(Json.obj("start" := 1, "end" := 2, "temperature" := "hot"))
    )

  test("codec.encodeRuntimeAttack"):
    assertEquals(
      encodeRuntimeAttack(RuntimeAttack("node v26.11.1", Duration(1000, MILLISECONDS), RuntimeAttackResult.Timeout)),
      Json.obj(
        "runtime" := "node v26.11.1",
        "timeout" := 1000,
        "result" := Json.obj("kind" := "timeout"),
        "isConfirmed" := true
      )
    )
    assertEquals(
      encodeRuntimeAttack(
        RuntimeAttack("ruby 4.0.7", Duration.Inf, RuntimeAttackResult.Finish(Duration(3, MILLISECONDS)))
      ),
      Json.obj(
        "runtime" := "ruby 4.0.7",
        "timeout" := Json.Null,
        "result" := Json.obj("kind" := "finish", "time" := 3),
        "isConfirmed" := false
      )
    )
    assertEquals(
      encodeRuntimeAttackResult(RuntimeAttackResult.EngineLimit("Backtrack limit exhausted")),
      Json.obj("kind" := "engine-limit", "message" := "Backtrack limit exhausted")
    )
    assertEquals(
      encodeRuntimeAttackResult(RuntimeAttackResult.EngineError("StackOverflowError")),
      Json.obj("kind" := "engine-error", "message" := "StackOverflowError")
    )

  test("codec.encodeRx"):
    val js = Dialect.JavaScript
    assertEquals(
      encodeRx(Rx.Safe("a", "", js)),
      Json.obj("source" := "a", "flags" := "", "dialect" := js, "status" := "safe")
    )
    assertEquals(
      encodeRx(Rx.Unknown("(", "", js, Some(ErrorKind.InvalidRegExp("foo")))),
      Json.obj(
        "source" := "(",
        "flags" := "",
        "dialect" := js,
        "status" := "unknown",
        "error" := Json.obj("kind" := "invalid", "message" := "foo")
      )
    )
    assertEquals(encodeRx(Rx.Unknown("a", "", js, None)).hcursor.downField("error").focus, Some(Json.Null))
    def repaired(comparison: Rx.Comparison): Json =
      encodeRx(Rx.Repaired("^(a+)+$", "", js, UnitMode.UTF16, "^(a+)$", AttackComplexity.Linear, Seq(0, 1), comparison))
    assertEquals(
      repaired(Rx.Comparison.Equivalent),
      Json.obj(
        "source" := "^(a+)+$",
        "flags" := "",
        "dialect" := js,
        "unitMode" := "utf16",
        "status" := "repaired",
        "repaired" := "^(a+)$",
        "complexity" := (AttackComplexity.Linear: AttackComplexity),
        "kept" := Seq(0, 1),
        "comparison" := Json.obj("kind" := "equivalent")
      )
    )
    val counterexample = Rx.Counterexample(UString(0x61, 0x62), Some(Seq(Some((0, 2)), None)), None)
    assertEquals(
      repaired(Rx.Comparison.Different(counterexample)).hcursor.downField("comparison").focus,
      Some(
        Json.obj(
          "kind" := "different",
          "input" := "ab",
          "original" := Json.arr(Json.arr(0.asJson, 2.asJson), Json.Null),
          "repaired" := Json.Null
        )
      )
    )
    assertEquals(
      repaired(Rx.Comparison.Unknown).hcursor.downField("comparison").focus,
      Some(Json.obj("kind" := "unknown"))
    )

  test("codec.decodeKeep"):
    assertEquals(decodeKeep.decodeJson("all".asJson), Right(Keep.All))
    assertEquals(decodeKeep.decodeJson("named".asJson), Right(Keep.Named))
    assertEquals(decodeKeep.decodeJson("match-range".asJson), Right(Keep.MatchRange))
    assertEquals(decodeKeep.decodeJson("match".asJson), Right(Keep.Match))
    assertEquals(decodeKeep.decodeJson(Json.arr(1.asJson, "year".asJson)), Right(Keep.Captures(Seq(1, "year"))))
    assertEquals(decodeKeep.decodeJson("foo".asJson), Left(DecodingFailure("Unknown keep: foo", List.empty)))
    assertEquals(
      decodeKeep.decodeJson(Json.arr(true.asJson)),
      Left(DecodingFailure("Keep must be capture numbers and names", List.empty))
    )
    assertEquals(
      decodeKeep.decodeJson(1.asJson),
      Left(DecodingFailure("Keep must be a string or an array", List.empty))
    )

  test("codec.encodeErrorKind"):
    assertEquals(encodeErrorKind(ErrorKind.Timeout), Json.obj("kind" := "timeout"))
    assertEquals(encodeErrorKind(ErrorKind.Cancel), Json.obj("kind" := "cancel"))
    assertEquals(encodeErrorKind(ErrorKind.Unsupported("foo")), Json.obj("kind" := "unsupported", "message" := "foo"))
    assertEquals(encodeErrorKind(ErrorKind.InvalidRegExp("foo")), Json.obj("kind" := "invalid", "message" := "foo"))
    assertEquals(
      encodeErrorKind(ErrorKind.InvalidKeep("foo")),
      Json.obj("kind" := "invalid-keep", "message" := "foo")
    )
    assertEquals(encodeErrorKind(ErrorKind.RuntimeAttacksRefuted), Json.obj("kind" := "runtime-attacks-refuted"))
    assertEquals(encodeErrorKind(ErrorKind.Unexpected("foo")), Json.obj("kind" := "unexpected", "message" := "foo"))

  test("codec.encodeUString"):
    assertEquals(encodeUString(UnitMode.UTF16)(UString(0x61, 0xd83d, 0xde00)), Json.fromString("a😀"))
    assertEquals(encodeUString(UnitMode.CodePoint)(UString(0x61, 0x1f600)), Json.fromString("a😀"))
    assertEquals(encodeUString(UnitMode.Byte)(UString(0x61, 0x7f)), Json.fromString("a\u007f"))

    // A string that does not read back as the units is an array of them.
    assertEquals(
      encodeUString(UnitMode.UTF16)(UString(0x61, 0xd800)),
      Json.arr(Json.fromInt(0x61), Json.fromInt(0xd800))
    )
    for mode <- Seq(UnitMode.UTF16CodePoint, UnitMode.CodePoint) do
      assertEquals(encodeUString(mode)(UString(0xdc00)), Json.arr(Json.fromInt(0xdc00)))
    assertEquals(encodeUString(UnitMode.ScalarValue)(UString(0x61, 0x1f600)), Json.fromString("a😀"))
    assertEquals(encodeUString(UnitMode.Byte)(UString(0x61, 0x80)), Json.arr(Json.fromInt(0x61), Json.fromInt(0x80)))
    assertEquals(
      encodeUString(UnitMode.CodePoint)(UString(0xd800, 0xdc00)),
      Json.arr(Json.fromInt(0xd800), Json.fromInt(0xdc00))
    )

  test("codec.decodeUString"):
    // Each mode decodes what `encodeUString` gives, including a lone surrogate.
    for (mode, s) <- Seq(
        UnitMode.UTF16 -> UString(0x61, 0xd83d, 0xde00, 0xd800),
        UnitMode.UTF16CodePoint -> UString(0x61, 0x1f600, 0xd800),
        UnitMode.CodePoint -> UString(0x61, 0x1f600, 0xd800),
        UnitMode.ScalarValue -> UString(0x61, 0x1f600),
        UnitMode.Byte -> UString(0x61, 0xc3, 0xa9, 0xff)
      )
    do assertEquals(decodeUString(mode).decodeJson(encodeUString(mode)(s)), Right(s))
    assert(clue(decodeUString(UnitMode.Byte).decodeJson("é".asJson)).isRight)
    assertEquals(
      decodeUString(UnitMode.Byte).decodeJson("\u0100".asJson).left.map(_.message),
      Left("Invalid byte string: \u0100")
    )

    assertEquals(
      decodeUString(UnitMode.CodePoint).decodeJson(Json.arr(Json.fromInt(0xd800), Json.fromInt(0xdc00))),
      Right(UString(0xd800, 0xdc00))
    )
    assertEquals(
      decodeUString(UnitMode.Byte).decodeJson(Json.arr(Json.fromInt(0x100))).left.map(_.message),
      Left("Invalid unit in Byte mode: 100")
    )
    assertEquals(
      decodeUString(UnitMode.ScalarValue).decodeJson("\ud800".asJson).left.map(_.message),
      Left("Invalid unit in ScalarValue mode: D800")
    )
    assertEquals(
      decodeUString(UnitMode.ScalarValue).decodeJson(Json.arr(Json.fromInt(0xd800))).left.map(_.message),
      Left("Invalid unit in ScalarValue mode: D800")
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

    // A high surrogate followed by a low surrogate, which some repeat count gives, is not a string of `UTF16CodePoint`.
    val paired = AttackPattern(Seq((UString(0xd800), UString(0xdc00), 0)), UString.empty, 1)
    val json = encodeAttackPattern(Dialect.JavaScript, UnitMode.CodePoint)(paired)
    assertEquals(decodeAttackPattern(UnitMode.CodePoint).decodeJson(json), Right(paired))
    assertEquals(
      decodeAttackPattern(UnitMode.UTF16CodePoint).decodeJson(json).left.map(_.message),
      Left("Invalid attack pattern in UTF16CodePoint mode: a high surrogate followed by a low surrogate")
    )
    // The failure points to the attack pattern in the JSON.
    assertEquals(
      decodeAttackPattern(UnitMode.UTF16CodePoint)
        .at("attack")
        .decodeJson(Json.obj("attack" := json))
        .left
        .map(_.pathToRootString),
      Left(Some(".attack"))
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
          "runtimeAttackTimeout" := Json.Null,
          "seedingTimeout" := Json.Null
        )
      ),
      Right(
        Parameters(
          attackTimeout = Duration.Inf,
          incubationTimeout = Duration.Inf,
          runtimeAttackTimeout = Duration.Inf,
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
          "maxAttackCandidates" := 123,
          "crossoverSize" := 123,
          "mutationSize" := 123,
          "attackTimeout" := 123,
          "maxDegree" := 123,
          "heatRatio" := 0.123,
          "accelerationMode" := "off",
          "maxRepeatCount" := 123,
          "maxNFASize" := 123,
          "maxDFASize" := 123,
          "maxPatternSize" := 123,
          "dialect" := Json.obj("name" := "perl", "utf8" := false, "source" := "string"),
          "maxRefutedAttacks" := 123
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
          maxAttackCandidates = 123,
          crossoverSize = 123,
          mutationSize = 123,
          attackTimeout = Duration(123, MILLISECONDS),
          maxDegree = 123,
          heatRatio = 0.123,
          accelerationMode = AccelerationMode.Off,
          maxRepeatCount = 123,
          maxNFASize = 123,
          maxDFASize = 123,
          maxPatternSize = 123,
          dialect = Dialect.Perl(utf8 = false, source = PerlSource.String),
          maxRefutedAttacks = 123
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
