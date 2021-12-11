package codes.quine.labo.recheck

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax._

import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.Parameters
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
  }

  /** An `Encoder` for `UString`. */
  implicit def encodeUString: Encoder[UString] = _.asString.asJson

  /** A `Decoder` for `Parameters`. */
  implicit def decodeParameters: Decoder[Parameters] = (c: HCursor) =>
    for {
      checker <- c.getOrElse[Checker]("checker")(Parameters.CHECKER)
      timeout <- c.getOrElse[Duration]("timeout")(Parameters.TIMEOUT)
      maxAttackStringSize <- c.getOrElse[Int]("maxAttackStringSize")(Parameters.MAX_ATTACK_STRING_SIZE)
      attackLimit <- c.getOrElse[Int]("attackLimit")(Parameters.ATTACK_LIMIT)
      randomSeed <- c.getOrElse[Long]("randomSeed")(Parameters.RANDOM_SEED)
      maxIteration <- c.getOrElse[Int]("maxIteration")(Parameters.MAX_ITERATION)
      seedingLimit <- c.getOrElse[Int]("seedingLimit")(Parameters.SEEDING_LIMIT)
      seedingTimeout <- c.getOrElse[Duration]("seedingTimeout")(Parameters.SEEDING_TIMEOUT)
      maxInitialGenerationSize <- c.getOrElse[Int]("maxInitialGenerationSize")(Parameters.MAX_INITIAL_GENERATION_SIZE)
      incubationLimit <- c.getOrElse[Int]("incubationLimit")(Parameters.INCUBATION_LIMIT)
      incubationTimeout <- c.getOrElse[Duration]("incubationTimeout")(Parameters.INCUBATION_TIMEOUT)
      maxGeneStringSize <- c.getOrElse[Int]("maxGeneStringSize")(Parameters.MAX_GENE_STRING_SIZE)
      maxGenerationSize <- c.getOrElse[Int]("maxGenerationSize")(Parameters.MAX_GENERATION_SIZE)
      crossoverSize <- c.getOrElse[Int]("crossoverSize")(Parameters.CROSSOVER_SIZE)
      mutationSize <- c.getOrElse[Int]("mutationSize")(Parameters.MUTATION_SIZE)
      attackTimeout <- c.getOrElse[Duration]("attackTimeout")(Parameters.ATTACK_TIMEOUT)
      maxDegree <- c.getOrElse[Int]("maxDegree")(Parameters.MAX_DEGREE)
      heatRatio <- c.getOrElse[Double]("heatRatio")(Parameters.HEAT_RATIO)
      usesAcceleration <- c.getOrElse[Boolean]("usesAcceleration")(Parameters.USES_ACCELERATION)
      maxRepeatCount <- c.getOrElse[Int]("maxRepeatCount")(Parameters.MAX_REPEAT_COUNT)
      maxNFASize <- c.getOrElse[Int]("maxNFASize")(Parameters.MAX_N_F_A_SIZE)
      maxPatternSize <- c.getOrElse[Int]("maxPatternSize")(Parameters.MAX_PATTERN_SIZE)
    } yield Parameters(
      checker,
      timeout,
      maxAttackStringSize,
      attackLimit,
      randomSeed,
      maxIteration,
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
      usesAcceleration,
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
}
