package codes.quine.labs.recheck

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import io.circe.Decoder
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
  implicit def decodeParameters(implicit decodeLogger: Decoder[Context.Logger]): Decoder[Parameters] = (c: HCursor) =>
    for {
      checker <- c.getOrElse[Checker]("checker")(Parameters.DefaultChecker)
      timeout <- c.getOrElse[Duration]("timeout")(Parameters.DefaultTimeout)
      logger <- c.getOrElse[Option[Context.Logger]]("logger")(Parameters.DefaultLogger)
      randomSeed <- c.getOrElse[Long]("randomSeed")(Parameters.DefaultRandomSeed)
      maxIteration <- c.getOrElse[Int]("maxIteration")(Parameters.DefaultMaxIteration)
      seeder <- c.getOrElse[Seeder]("seeder")(Parameters.DefaultSeeder)
      maxSimpleRepeatCount <- c.getOrElse[Int]("maxSimpleRepeatCount")(Parameters.DefaultMaxSimpleRepeatCount)
      seedingLimit <- c.getOrElse[Int]("seedingLimit")(Parameters.DefaultSeedingLimit)
      seedingTimeout <- c.getOrElse[Duration]("seedingTimeout")(Parameters.DefaultSeedingTimeout)
      maxInitialGenerationSize <- c.getOrElse[Int]("maxInitialGenerationSize")(
        Parameters.DefaultMaxInitialGenerationSize
      )
      incubationLimit <- c.getOrElse[Int]("incubationLimit")(Parameters.DefaultIncubationLimit)
      incubationTimeout <- c.getOrElse[Duration]("incubationTimeout")(Parameters.DefaultIncubationTimeout)
      maxGeneStringSize <- c.getOrElse[Int]("maxGeneStringSize")(Parameters.DefaultMaxGeneStringSize)
      maxGenerationSize <- c.getOrElse[Int]("maxGenerationSize")(Parameters.DefaultMaxGenerationSize)
      crossoverSize <- c.getOrElse[Int]("crossoverSize")(Parameters.DefaultCrossoverSize)
      mutationSize <- c.getOrElse[Int]("mutationSize")(Parameters.DefaultMutationSize)
      attackLimit <- c.getOrElse[Int]("attackLimit")(Parameters.DefaultAttackLimit)
      attackTimeout <- c.getOrElse[Duration]("attackTimeout")(Parameters.DefaultAttackTimeout)
      maxAttackStringSize <- c.getOrElse[Int]("maxAttackStringSize")(Parameters.DefaultMaxAttackStringSize)
      maxDegree <- c.getOrElse[Int]("maxDegree")(Parameters.DefaultMaxDegree)
      heatRatio <- c.getOrElse[Double]("heatRatio")(Parameters.DefaultHeatRatio)
      accelerationMode <- c.getOrElse[AccelerationMode]("accelerationMode")(Parameters.DefaultAccelerationMode)
      maxRepeatCount <- c.getOrElse[Int]("maxRepeatCount")(Parameters.DefaultMaxRepeatCount)
      maxPatternSize <- c.getOrElse[Int]("maxPatternSize")(Parameters.DefaultMaxPatternSize)
      maxNFASize <- c.getOrElse[Int]("maxNFASize")(Parameters.DefaultMaxNFASize)
      recallLimit <- c.getOrElse[Int]("recallLimit")(Parameters.DefaultRecallLimit)
      recallTimeout <- c.getOrElse[Duration]("recallTimeout")(Parameters.DefaultRecallTimeout)
      maxRecallStringSize <- c.getOrElse[Int]("maxRecallStringSize")(Parameters.DefaultMaxRecallStringSize)
    } yield Parameters(
      checker,
      timeout,
      logger,
      randomSeed,
      maxIteration,
      seeder,
      maxSimpleRepeatCount,
      seedingLimit,
      seedingTimeout,
      maxInitialGenerationSize,
      incubationLimit,
      incubationTimeout,
      maxGeneStringSize,
      maxGenerationSize,
      crossoverSize,
      mutationSize,
      attackLimit,
      attackTimeout,
      maxAttackStringSize,
      maxDegree,
      heatRatio,
      accelerationMode,
      maxRepeatCount,
      maxPatternSize,
      maxNFASize,
      recallLimit,
      recallTimeout,
      maxRecallStringSize
    )

  /** A `Decoder` for `Duration`. */
  implicit def decodeDuration: Decoder[Duration] = Decoder[Option[Int]].map {
    case Some(d) => Duration(d, MILLISECONDS)
    case None    => Duration.Inf
  }

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
