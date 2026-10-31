package codes.quine.labs.recheck
package codec

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.MILLISECONDS

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax.*

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.Diagnostics.ErrorKind
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.resyntax.charset.UString

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
    case Diagnostics.Vulnerable(source, flags, complexity, attack, hotspot, checker, recallValidation) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "status" := "vulnerable",
        "checker" := checker,
        "complexity" := complexity.asInstanceOf[AttackComplexity],
        "attack" := attack,
        "hotspot" := hotspot,
        "recallValidation" := recallValidation
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

/** An `Encoder` for `RecallValidation`. */
given encodeRecallValidation: Encoder[RecallValidation] with
  def apply(r: RecallValidation): Json =
    val timeout = r.timeout match
      case d: FiniteDuration => d.toMillis.asJson
      case _                 => Json.Null
    Json.obj(
      "runtime" := r.runtime,
      "timeout" := timeout,
      "result" := r.result,
      "succeeded" := r.succeeded
    )

/** An `Encoder` for `RecallResult`. */
given encodeRecallResult: Encoder[RecallResult] with
  def apply(r: RecallResult): Json = r match
    case RecallResult.Finish(time)         => Json.obj("kind" := "finish", "time" := time.toMillis)
    case RecallResult.Timeout              => Json.obj("kind" := "timeout")
    case RecallResult.EngineError(message) => Json.obj("kind" := "engine-error", "message" := message)

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

/** A `Decoder` for `Duration`. */
given decodeDuration: Decoder[Duration] = (c: HCursor) =>
  if c.value.isNull then Right(Duration.Inf)
  else
    Decoder[Int].tryDecode(c).map(t => Duration(t, MILLISECONDS)).orElse(Left(DecodingFailure("Duration", c.history)))
