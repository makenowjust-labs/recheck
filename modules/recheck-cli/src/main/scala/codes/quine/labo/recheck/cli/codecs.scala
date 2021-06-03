package codes.quine.labo.recheck.cli

import cats.syntax.contravariant._
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

object codecs {
  implicit def encodeDiagnostics: Encoder[Diagnostics] = {
    case Diagnostics.Safe(source, flags, complexity, checker) =>
      Json.obj(
        "source" -> source.asJson,
        "flags" -> flags.asJson,
        "status" -> "safe".asJson,
        "checker" -> checker.asJson,
        "complexity" -> complexity.asJson(encodeAttackComplexity.narrow)
      )
    case Diagnostics.Vulnerable(source, flags, complexity, attack, hotspot, checker) =>
      Json.obj(
        "source" -> source.asJson,
        "flags" -> flags.asJson,
        "status" -> "vulnerable".asJson,
        "checker" -> checker.asJson,
        "complexity" -> complexity.asJson(encodeAttackComplexity.narrow),
        "attack" -> attack.asJson,
        "hotspot" -> hotspot.asJson
      )
    case Diagnostics.Unknown(source, flags, error, checker) =>
      Json.obj(
        "source" -> source.asJson,
        "flags" -> flags.asJson,
        "status" -> "unknown".asJson,
        "checker" -> checker.asJson,
        "error" -> error.asJson
      )
  }

  implicit def encodeChecker: Encoder[Checker] = {
    case Checker.Hybrid    => "hybrid".asJson
    case Checker.Automaton => "automaton".asJson
    case Checker.Fuzz      => "fuzz".asJson
  }

  implicit def encodeAttackComplexity: Encoder[AttackComplexity] = {
    case c @ AttackComplexity.Constant =>
      Json.obj(
        "type" -> "constant".asJson,
        "summary" -> c.toString.asJson,
        "isFuzz" -> false.asJson
      )
    case c @ AttackComplexity.Linear =>
      Json.obj(
        "type" -> "linear".asJson,
        "summary" -> c.toString.asJson,
        "isFuzz" -> false.asJson
      )
    case c: AttackComplexity.Safe =>
      Json.obj(
        "type" -> "safe".asJson,
        "summary" -> c.toString.asJson,
        "isFuzz" -> c.isFuzz.asJson
      )
    case c: AttackComplexity.Polynomial =>
      Json.obj(
        "type" -> "polynomial".asJson,
        "degree" -> c.degree.asJson,
        "summary" -> c.toString.asJson,
        "isFuzz" -> c.isFuzz.asJson
      )
    case c: AttackComplexity.Exponential =>
      Json.obj(
        "type" -> "exponential".asJson,
        "summary" -> c.toString.asJson,
        "isFuzz" -> c.isFuzz.asJson
      )
  }

  implicit def encodeAttackPattern: Encoder[AttackPattern] = (p: AttackPattern) =>
    Json.obj(
      "pumps" ->
        Json.arr(p.pumps.map { case (p, s, n) =>
          Json.obj("prefix" -> p.asJson, "pump" -> s.asJson, "bias" -> n.asJson)
        }: _*),
      "suffix" -> p.suffix.asJson,
      "base" -> p.n.asJson,
      "string" -> p.asUString.asJson,
      "pattern" -> p.toString.asJson
    )

  implicit def encodeHotspot: Encoder[Hotspot] = (h: Hotspot) =>
    Json.arr(h.spots.map { case Hotspot.Spot(start, end, temperature) =>
      Json.obj("start" -> start.asJson, "end" -> end.asJson, "temperature" -> temperature.toString.asJson)
    }: _*)

  implicit def encodeErrorKind: Encoder[ErrorKind] = {
    case ErrorKind.Timeout =>
      Json.obj("kind" -> "timeout".asJson)
    case ErrorKind.Unsupported(message) =>
      Json.obj("kind" -> "unsupported".asJson, "message" -> message.asJson)
    case ErrorKind.InvalidRegExp(message) =>
      Json.obj("kind" -> "invalid".asJson, "message" -> message.asJson)
  }

  implicit def encodeUString: Encoder[UString] = _.asString.asJson

  implicit def decodeChecker: Decoder[Checker] =
    Decoder[String].emap {
      case "hybrid"    => Right(Checker.Hybrid)
      case "automaton" => Right(Checker.Automaton)
      case "fuzz"      => Right(Checker.Fuzz)
      case s           => Left(s"Unknown checker: $s")
    }
}
