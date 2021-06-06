package codes.quine.labo.recheck

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._

import codes.quine.labo.recheck.common.Checker
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
    case ErrorKind.Unsupported(message) =>
      Json.obj("kind" := "unsupported", "message" := message)
    case ErrorKind.InvalidRegExp(message) =>
      Json.obj("kind" := "invalid", "message" := message)
  }

  /** An `Encoder` for `UString`. */
  implicit def encodeUString: Encoder[UString] = _.asString.asJson

  /** A `Decoder` for `Checker`. */
  implicit def decodeChecker: Decoder[Checker] =
    Decoder[String].emap {
      case "hybrid"    => Right(Checker.Hybrid)
      case "automaton" => Right(Checker.Automaton)
      case "fuzz"      => Right(Checker.Fuzz)
      case s           => Left(s"Unknown checker: $s")
    }
}
