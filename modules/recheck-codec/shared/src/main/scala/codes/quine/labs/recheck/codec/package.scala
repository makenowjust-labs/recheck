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
      accelerationMode <- c.get[AccelerationMode]("accelerationMode").orElse(Right(Parameters.DefaultAccelerationMode))
      attackLimit <- c.get[Int]("attackLimit").orElse(Right(Parameters.DefaultAttackLimit))
      attackTimeout <- c.get[Duration]("attackTimeout").orElse(Right(Parameters.DefaultAttackTimeout))
      checker <- c.get[Checker]("checker").orElse(Right(Parameters.DefaultChecker))
      crossoverSize <- c.get[Int]("crossoverSize").orElse(Right(Parameters.DefaultCrossoverSize))
      heatRatio <- c.get[Double]("heatRatio").orElse(Right(Parameters.DefaultHeatRatio))
      incubationLimit <- c.get[Int]("incubationLimit").orElse(Right(Parameters.DefaultIncubationLimit))
      incubationTimeout <- c.get[Duration]("incubationTimeout").orElse(Right(Parameters.DefaultIncubationTimeout))
      logger <- c.get[Option[Context.Logger]]("logger").orElse(Right(Parameters.DefaultLogger))
      maxAttackStringSize <- c.get[Int]("maxAttackStringSize").orElse(Right(Parameters.DefaultMaxAttackStringSize))
      maxDegree <- c.get[Int]("maxDegree").orElse(Right(Parameters.DefaultMaxDegree))
      maxGeneStringSize <- c.get[Int]("maxGeneStringSize").orElse(Right(Parameters.DefaultMaxGeneStringSize))
      maxGenerationSize <- c.get[Int]("maxGenerationSize").orElse(Right(Parameters.DefaultMaxGenerationSize))
      maxInitialGenerationSize <- c
        .get[Int]("maxInitialGenerationSize")
        .orElse(Right(Parameters.DefaultMaxInitialGenerationSize))
      maxIteration <- c.get[Int]("maxIteration").orElse(Right(Parameters.DefaultMaxIteration))
      maxNFASize <- c.get[Int]("maxNFASize").orElse(Right(Parameters.DefaultMaxNFASize))
      maxPatternSize <- c.get[Int]("maxPatternSize").orElse(Right(Parameters.DefaultMaxPatternSize))
      maxRecallStringSize <- c.get[Int]("maxRecallStringSize").orElse(Right(Parameters.DefaultMaxRecallStringSize))
      maxRepeatCount <- c.get[Int]("maxRepeatCount").orElse(Right(Parameters.DefaultMaxRepeatCount))
      maxSimpleRepeatCount <- c.get[Int]("maxSimpleRepeatCount").orElse(Right(Parameters.DefaultMaxSimpleRepeatCount))
      mutationSize <- c.get[Int]("mutationSize").orElse(Right(Parameters.DefaultMutationSize))
      randomSeed <- c.get[Long]("randomSeed").orElse(Right(Parameters.DefaultRandomSeed))
      recallLimit <- c.get[Int]("recallLimit").orElse(Right(Parameters.DefaultRecallLimit))
      recallTimeout <- c.get[Duration]("recallTimeout").orElse(Right(Parameters.DefaultRecallTimeout))
      seeder <- c.get[Seeder]("seeder").orElse(Right(Parameters.DefaultSeeder))
      seedingLimit <- c.get[Int]("seedingLimit").orElse(Right(Parameters.DefaultSeedingLimit))
      seedingTimeout <- c.get[Duration]("seedingTimeout").orElse(Right(Parameters.DefaultSeedingTimeout))
      timeout <- c.get[Duration]("timeout").orElse(Right(Parameters.DefaultTimeout))
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

  /** A `Decoder` for `Duration`. */
  implicit def decodeDuration: Decoder[Duration] = {
    import io.circe.disjunctionCodecs._

    Decoder[Either[Unit, Int]].map(_.fold(_ => Duration.Inf, Duration(_, MILLISECONDS)))
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
