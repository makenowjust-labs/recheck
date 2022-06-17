package codes.quine.labs.recheck

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.DecodingFailure.Reason.MissingField
import io.circe.Encoder
import io.circe.HCursor
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

/** Package `codec` provides recheck types `Decoder`/`Encoder` of circe. */
package object codec {

  /** An `Encoder` for `Diagnostics`. */
  implicit def encodeDiagnostics: Encoder[Diagnostics] = {
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
  }

  /** An `Encoder` for `Checker`. */
  implicit def encodeChecker: Encoder[Checker] = {
    case Checker.Auto      => "auto".asJson
    case Checker.Automaton => "automaton".asJson
    case Checker.Fuzz      => "fuzz".asJson
  }

  /** An `Encoder` for `AttackComplexity`. */
  implicit def encodeAttackComplexity: Encoder[AttackComplexity] = {
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
  }

  /** An `Encoder` for `AttackPattern`. */
  implicit def encodeAttackPattern: Encoder[AttackPattern] = (p: AttackPattern) =>
    Json.obj(
      "pumps" := Json.arr(p.pumps.map { case (p, s, n) => Json.obj("prefix" := p, "pump" := s, "bias" := n) }: _*),
      "suffix" := p.suffix,
      "base" := p.n,
      "string" := p.asUString,
      "pattern" := p.toString
    )

  /** An `Encoder` for `Hotspot`. */
  implicit def encodeHotspot: Encoder[Hotspot] = (h: Hotspot) =>
    Json.arr(h.spots.map { case Hotspot.Spot(s, e, t) =>
      Json.obj("start" := s, "end" := e, "temperature" := t.toString)
    }: _*)

  /** An `Encoder` for `ErrorKind`. */
  implicit def encodeErrorKind: Encoder[ErrorKind] = {
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
  }

  /** An `Encoder` for `UString`. */
  implicit def encodeUString: Encoder[UString] = _.asString.asJson

  /** A `Decoder` for `Parameters`. */
  implicit def decodeParameters(implicit decodeLogger: Decoder[Context.Logger]): Decoder[Parameters] = (c: HCursor) => {

    /** Returns a decoded result if `key` is found, or returns the given fallback value as a result if key is missing.
      *
      * It is almost similar to `HCursor#getOrElse`. However, it only falls back on missing key (not on `null` case).
      */
    def getOrElse[A: Decoder](key: String)(fallback: A): Decoder.Result[A] =
      c.get[A](key) match {
        case Left(failure) if failure.reason == MissingField => Right(fallback)
        case result                                          => result
      }

    for {
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
    } yield Parameters(
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
  }

  /** A `Decoder` for `Duration`. */
  implicit def decodeDuration: Decoder[Duration] = (c: HCursor) =>
    if (c.value.isNull) Right(Duration.Inf)
    else if (c.value.isNumber) c.value.asNumber.flatMap(_.toInt) match {
      case Some(d) => Right(Duration(d, MILLISECONDS))
      case None    => Left(DecodingFailure("Duration", c.history))
    }
    else Left(DecodingFailure("Duration", c.history))

  /** A `Decoder` for `Checker`. */
  implicit def decodeChecker: Decoder[Checker] =
    Decoder[String].emap {
      case "auto"      => Right(Checker.Auto)
      case "automaton" => Right(Checker.Automaton)
      case "fuzz"      => Right(Checker.Fuzz)
      case s           => Left(s"Unknown checker: $s")
    }

  /** A `Decoder` for `AccelerationMode`. */
  implicit def decodeAccelerationMode: Decoder[AccelerationMode] =
    Decoder[String].emap {
      case "auto" => Right(AccelerationMode.Auto)
      case "on"   => Right(AccelerationMode.On)
      case "off"  => Right(AccelerationMode.Off)
      case s      => Left(s"Unknown acceleration mode: $s")
    }

  /** A `Decoder` for `Seeder`. */
  implicit def decodeSeeder: Decoder[Seeder] =
    Decoder[String].emap {
      case "static"  => Right(Seeder.Static)
      case "dynamic" => Right(Seeder.Dynamic)
      case s         => Left(s"Unknown seeder: $s")
    }
}
