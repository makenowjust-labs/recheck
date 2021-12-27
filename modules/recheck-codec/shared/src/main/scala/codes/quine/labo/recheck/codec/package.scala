package codes.quine.labo.recheck

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax._

import codes.quine.labo.recheck.common.AccelerationMode
import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.Parameters
import codes.quine.labo.recheck.common.Seeder
import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.diagnostics.Diagnostics
import codes.quine.labo.recheck.diagnostics.Diagnostics.ErrorKind
import codes.quine.labo.recheck.diagnostics.Hotspot
import codes.quine.labo.recheck.unicode.UString

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
    case Checker.Hybrid    => "hybrid".asJson
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
      checker <- c.getOrElse[Checker]("checker")(Parameters.Checker)
      timeout <- c.getOrElse[Duration]("timeout")(Parameters.Timeout)
      logger <- c.getOrElse[Option[Context.Logger]]("logger")(Parameters.Logger)
      maxAttackStringSize <- c.getOrElse[Int]("maxAttackStringSize")(Parameters.MaxAttackStringSize)
      attackLimit <- c.getOrElse[Int]("attackLimit")(Parameters.AttackLimit)
      randomSeed <- c.getOrElse[Long]("randomSeed")(Parameters.RandomSeed)
      maxIteration <- c.getOrElse[Int]("maxIteration")(Parameters.MaxIteration)
      seeder <- c.getOrElse[Seeder]("seeder")(Parameters.Seeder)
      maxSimpleRepeatCount <- c.getOrElse[Int]("maxSimpleRepeatCount")(Parameters.MaxSimpleRepeatCount)
      seedingLimit <- c.getOrElse[Int]("seedingLimit")(Parameters.SeedingLimit)
      seedingTimeout <- c.getOrElse[Duration]("seedingTimeout")(Parameters.SeedingTimeout)
      maxInitialGenerationSize <- c.getOrElse[Int]("maxInitialGenerationSize")(Parameters.MaxInitialGenerationSize)
      incubationLimit <- c.getOrElse[Int]("incubationLimit")(Parameters.IncubationLimit)
      incubationTimeout <- c.getOrElse[Duration]("incubationTimeout")(Parameters.IncubationTimeout)
      maxGeneStringSize <- c.getOrElse[Int]("maxGeneStringSize")(Parameters.MaxGeneStringSize)
      maxGenerationSize <- c.getOrElse[Int]("maxGenerationSize")(Parameters.MaxGenerationSize)
      crossoverSize <- c.getOrElse[Int]("crossoverSize")(Parameters.CrossoverSize)
      mutationSize <- c.getOrElse[Int]("mutationSize")(Parameters.MutationSize)
      attackTimeout <- c.getOrElse[Duration]("attackTimeout")(Parameters.AttackTimeout)
      maxDegree <- c.getOrElse[Int]("maxDegree")(Parameters.MaxDegree)
      heatRatio <- c.getOrElse[Double]("heatRatio")(Parameters.HeatRatio)
      accelerationMode <- c.getOrElse[AccelerationMode]("accelerationMode")(Parameters.AccelerationMode)
      maxRecallStringSize <- c.getOrElse[Int]("maxRecallStringSize")(Parameters.MaxRecallStringSize)
      recallLimit <- c.getOrElse[Int]("recallLimit")(Parameters.RecallLimit)
      recallTimeout <- c.getOrElse[Duration]("recallTimeout")(Parameters.RecallTimeout)
      maxRepeatCount <- c.getOrElse[Int]("maxRepeatCount")(Parameters.MaxRepeatCount)
      maxNFASize <- c.getOrElse[Int]("maxNFASize")(Parameters.MaxNFASize)
      maxPatternSize <- c.getOrElse[Int]("maxPatternSize")(Parameters.MaxPatternSize)
    } yield Parameters(
      checker,
      timeout,
      logger,
      maxAttackStringSize,
      attackLimit,
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
      attackTimeout,
      maxDegree,
      heatRatio,
      accelerationMode,
      maxRecallStringSize,
      recallLimit,
      recallTimeout,
      maxRepeatCount,
      maxNFASize,
      maxPatternSize
    )

  /** A `Decoder` for `Duration`. */
  implicit def decodeDuration: Decoder[Duration] = Decoder[Option[Int]].map {
    case Some(d) => Duration(d, MILLISECONDS)
    case None    => Duration.Inf
  }

  /** A `Decoder` for `Checker`. */
  implicit def decodeChecker: Decoder[Checker] =
    Decoder[String].emap {
      case "hybrid"    => Right(Checker.Hybrid)
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
