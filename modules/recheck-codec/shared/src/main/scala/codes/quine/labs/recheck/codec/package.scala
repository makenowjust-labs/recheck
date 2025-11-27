package codes.quine.labs.recheck
package codec

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.DecodingFailure.Reason.MissingField
import io.circe.Encoder
import io.circe.HCursor
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
import codes.quine.labs.recheck.unicode.UString

/** Package `codec` provides recheck types `Decoder`/`Encoder` of circe. */
package object codec

/** An `Encoder` for `Diagnostics`. */
given encodeDiagnostics: Encoder[Diagnostics] with
  def apply(d: Diagnostics): Json = d match
    case Diagnostics.Safe(source, flags, complexity, checker) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "status" := "safe",
        "checker" := checker,
        "complexity" := complexity.asInstanceOf[AttackComplexity]
      )
    case Diagnostics.Vulnerable(source, flags, complexity, attack, hotspot, checker) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "status" := "vulnerable",
        "checker" := checker,
        "complexity" := complexity.asInstanceOf[AttackComplexity],
        "attack" := attack,
        "hotspot" := hotspot
      )
    case Diagnostics.Unknown(source, flags, error, checker) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "status" := "unknown",
        "checker" := checker,
        "error" := error
      )

/** An `Encoder` for `Checker`. */
given encodeChecker: Encoder[Checker] with
  def apply(c: Checker): Json = c match
    case Checker.Auto      => "auto".asJson
    case Checker.Automaton => "automaton".asJson
    case Checker.Fuzz      => "fuzz".asJson

/** An `Encoder` for `AttackComplexity`. */
given encodeAttackComplexity: Encoder[AttackComplexity] with
  def apply(c: AttackComplexity): Json = c match
    case c @ AttackComplexity.Constant =>
      Json.obj("type" := "constant", "summary" := c.toString, "isFuzz" := false)
    case c @ AttackComplexity.Linear =>
      Json.obj("type" := "linear", "summary" := c.toString, "isFuzz" := false)
    case c: AttackComplexity.Safe =>
      Json.obj("type" := "safe", "summary" := c.toString, "isFuzz" := c.isFuzz)
    case c: AttackComplexity.Polynomial =>
      Json.obj("type" := "polynomial", "degree" := c.degree, "summary" := c.toString, "isFuzz" := c.isFuzz)
    case c: AttackComplexity.Exponential =>
      Json.obj("type" := "exponential", "summary" := c.toString, "isFuzz" := c.isFuzz)

/** An `Encoder` for `AttackPattern`. */
given encodeAttackPattern: Encoder[AttackPattern] with
  def apply(p: AttackPattern): Json =
    val pumps = p.pumps.map:
      case (p, s, n) => Json.obj("prefix" := p, "pump" := s, "bias" := n)
    Json.obj(
      "pumps" := Json.arr(pumps*),
      "suffix" := p.suffix,
      "base" := p.n,
      "string" := p.asUString,
      "pattern" := p.toString
    )

/** An `Encoder` for `Hotspot`. */
given encodeHotspot: Encoder[Hotspot] with
  def apply(h: Hotspot): Json =
    val spots = h.spots.map:
      case Hotspot.Spot(s, e, t) => Json.obj("start" := s, "end" := e, "temperature" := t.toString)
    Json.arr(spots*)

/** An `Encoder` for `ErrorKind`. */
given encodeErrorKind: Encoder[ErrorKind] with
  def apply(e: ErrorKind): Json = e match
    case ErrorKind.Timeout =>
      Json.obj("kind" := "timeout")
    case ErrorKind.Cancel =>
      Json.obj("kind" := "cancel")
    case ErrorKind.Unsupported(message) =>
      Json.obj("kind" := "unsupported", "message" := message)
    case ErrorKind.InvalidRegExp(message) =>
      Json.obj("kind" := "invalid", "message" := message)
    case ErrorKind.Unexpected(message) =>
      Json.obj("kind" := "unexpected", "message" := message)

/** An `Encoder` for `UString`. */
given encodeUString: Encoder[UString] = _.asString.asJson

/** A `Decoder` for `Parameters`. */
given decodeParameters(using decodeLogger: Decoder[Context.Logger]): Decoder[Parameters] with

  def apply(c: HCursor): Decoder.Result[Parameters] =

    /** Returns a decoded result if `key` is found, or returns the given fallback value as a result if key is missing.
      *
      * It is almost similar to `HCursor#getOrElse`. However, it only falls back on missing key (not on `null` case).
      */
    def getOrElse[A: Decoder](key: String)(fallback: A): Decoder.Result[A] =
      c.get[A](key) match
        case Left(failure) if failure.reason == MissingField => Right(fallback)
        case result                                          => result

    for
      accelerationMode <- getOrElse[AccelerationMode]("accelerationMode")(Parameters.DefaultAccelerationMode)
      attackLimit <- getOrElse[Int]("attackLimit")(Parameters.DefaultAttackLimit)
      attackTimeout <- getOrElse[Duration]("attackTimeout")(Parameters.DefaultAttackTimeout)
      checker <- getOrElse[Checker]("checker")(Parameters.DefaultChecker)
      crossoverSize <- getOrElse[Int]("crossoverSize")(Parameters.DefaultCrossoverSize)
      heatRatio <- getOrElse[Double]("heatRatio")(Parameters.DefaultHeatRatio)
      incubationLimit <- getOrElse[Int]("incubationLimit")(Parameters.DefaultIncubationLimit)
      incubationTimeout <- getOrElse[Duration]("incubationTimeout")(Parameters.DefaultIncubationTimeout)
      logger <- getOrElse[Option[Context.Logger]]("logger")(Parameters.DefaultLogger)
      maxAttackStringSize <- getOrElse[Int]("maxAttackStringSize")(Parameters.DefaultMaxAttackStringSize)
      maxDegree <- getOrElse[Int]("maxDegree")(Parameters.DefaultMaxDegree)
      maxGeneStringSize <- getOrElse[Int]("maxGeneStringSize")(Parameters.DefaultMaxGeneStringSize)
      maxGenerationSize <- getOrElse[Int]("maxGenerationSize")(Parameters.DefaultMaxGenerationSize)
      maxInitialGenerationSize <- getOrElse[Int]("maxInitialGenerationSize")(Parameters.DefaultMaxInitialGenerationSize)
      maxIteration <- getOrElse[Int]("maxIteration")(Parameters.DefaultMaxIteration)
      maxNFASize <- getOrElse[Int]("maxNFASize")(Parameters.DefaultMaxNFASize)
      maxPatternSize <- getOrElse[Int]("maxPatternSize")(Parameters.DefaultMaxPatternSize)
      maxRecallStringSize <- getOrElse[Int]("maxRecallStringSize")(Parameters.DefaultMaxRecallStringSize)
      maxRepeatCount <- getOrElse[Int]("maxRepeatCount")(Parameters.DefaultMaxRepeatCount)
      maxSimpleRepeatCount <- getOrElse[Int]("maxSimpleRepeatCount")(Parameters.DefaultMaxSimpleRepeatCount)
      mutationSize <- getOrElse[Int]("mutationSize")(Parameters.DefaultMutationSize)
      randomSeed <- getOrElse[Long]("randomSeed")(Parameters.DefaultRandomSeed)
      recallLimit <- getOrElse[Int]("recallLimit")(Parameters.DefaultRecallLimit)
      recallTimeout <- getOrElse[Duration]("recallTimeout")(Parameters.DefaultRecallTimeout)
      seeder <- getOrElse[Seeder]("seeder")(Parameters.DefaultSeeder)
      seedingLimit <- getOrElse[Int]("seedingLimit")(Parameters.DefaultSeedingLimit)
      seedingTimeout <- getOrElse[Duration]("seedingTimeout")(Parameters.DefaultSeedingTimeout)
      timeout <- getOrElse[Duration]("timeout")(Parameters.DefaultTimeout)
    yield Parameters(
      accelerationMode,
      attackLimit,
      attackTimeout,
      checker,
      crossoverSize,
      heatRatio,
      incubationLimit,
      incubationTimeout,
      logger,
      maxAttackStringSize,
      maxDegree,
      maxGeneStringSize,
      maxGenerationSize,
      maxInitialGenerationSize,
      maxIteration,
      maxNFASize,
      maxPatternSize,
      maxRecallStringSize,
      maxRepeatCount,
      maxSimpleRepeatCount,
      mutationSize,
      randomSeed,
      recallLimit,
      recallTimeout,
      seeder,
      seedingLimit,
      seedingTimeout,
      timeout
    )

/** A `Decoder` for `Duration`. */
given decodeDuration: Decoder[Duration] = (c: HCursor) =>
  if c.value.isNull then Right(Duration.Inf)
  else
    Decoder[Int].tryDecode(c).map(t => Duration(t, MILLISECONDS)).orElse(Left(DecodingFailure("Duration", c.history)))

/** A `Decoder` for `Checker`. */
given decodeChecker: Decoder[Checker] =
  Decoder[String].emap:
    case "auto"      => Right(Checker.Auto)
    case "automaton" => Right(Checker.Automaton)
    case "fuzz"      => Right(Checker.Fuzz)
    case s           => Left(s"Unknown checker: $s")

/** A `Decoder` for `Seeder`. */
given decodeSeeder: Decoder[Seeder] =
  Decoder[String].emap:
    case "static"  => Right(Seeder.Static)
    case "dynamic" => Right(Seeder.Dynamic)
    case s         => Left(s"Unknown seeder: $s")

/** A `Decoder` for `AccelerationMode`. */
given decodeAccelerationMode: Decoder[AccelerationMode] =
  Decoder[String].emap:
    case "auto" => Right(AccelerationMode.Auto)
    case "on"   => Right(AccelerationMode.On)
    case "off"  => Right(AccelerationMode.Off)
    case s      => Left(s"Unknown acceleration mode: $s")
