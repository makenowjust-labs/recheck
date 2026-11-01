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
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** Package `codec` provides recheck types `Decoder`/`Encoder` of circe. */
package object codec

/** An `Encoder` for `Diagnostics`. */
given encodeDiagnostics: Encoder[Diagnostics] with
  def apply(d: Diagnostics): Json = d match
    case Diagnostics.Safe(source, flags, dialect, complexity, checker) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "dialect" := dialect,
        "status" := "safe",
        "checker" := checker,
        "complexity" := complexity.asInstanceOf[AttackComplexity]
      )
    case Diagnostics.Vulnerable(source, flags, dialect, complexity, attack, hotspot, checker, recallValidation) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "dialect" := dialect,
        "status" := "vulnerable",
        "checker" := checker,
        "complexity" := complexity.asInstanceOf[AttackComplexity],
        "attack" := attack,
        "hotspot" := hotspot,
        "recallValidation" := recallValidation
      )
    case Diagnostics.Unknown(source, flags, dialect, error, checker) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "dialect" := dialect,
        "status" := "unknown",
        "checker" := checker,
        "error" := error
      )

/** An `Encoder` for `Dialect`, an object of the name and every setting. */
given encodeDialect: Encoder[Dialect] with
  def apply(d: Dialect): Json =
    val settings = d match
      case Dialect.Python(tpe)        => Seq("type" := tpe.name)
      case Dialect.Ruby(encoding)     => Seq("encoding" := encoding.name)
      case Dialect.Perl(utf8, source) => Seq("utf8" := utf8, "source" := source.name)
      case _                          => Seq.empty
    Json.obj(("name" := d.name) +: settings*)

/** A `Decoder` for `Dialect`, which is the name of the language, or an object of the name and the settings.
  *
  * An omitted or `null` setting takes the default.
  */
given decodeDialect: Decoder[Dialect] with
  def apply(c: HCursor): Decoder.Result[Dialect] =
    def fail(message: String): Decoder.Result[Dialect] = Left(DecodingFailure(message, c.history))
    def setting[A](dialect: Dialect, key: String, default: A)(of: String => Option[A]): Decoder.Result[A] =
      c.get[Option[String]](key)
        .flatMap:
          case None       => Right(default)
          case Some(name) => of(name).toRight(DecodingFailure(s"Unknown $key of ${dialect.name}: $name", c.history))
    c.value.asString match
      case Some(name) => Dialect.of(name).fold(fail(s"Unknown dialect: $name"))(Right(_))
      case None       =>
        c.get[String]("name")
          .flatMap: name =>
            Dialect.of(name) match
              case None          => fail(s"Unknown dialect: $name")
              case Some(dialect) =>
                val keys = c.keys.toSeq.flatten.filter(_ != "name")
                val allowed = dialect match
                  case _: Dialect.Python => Seq("type")
                  case _: Dialect.Ruby   => Seq("encoding")
                  case _: Dialect.Perl   => Seq("utf8", "source")
                  case _                 => Seq.empty
                keys.find(!allowed.contains(_)) match
                  case Some(key) => fail(s"Unknown setting of $name: $key")
                  case None      =>
                    dialect match
                      case python: Dialect.Python =>
                        setting(python, "type", python.`type`)(Dialect.Python.Type.of).map(Dialect.Python(_))
                      case ruby: Dialect.Ruby =>
                        setting(ruby, "encoding", ruby.encoding)(Dialect.Ruby.Encoding.of).map(Dialect.Ruby(_))
                      case perl: Dialect.Perl =>
                        for
                          utf8 <- c.get[Option[Boolean]]("utf8")
                          source <- setting(perl, "source", perl.source)(PerlSource.of)
                        yield Dialect.Perl(utf8.getOrElse(perl.utf8), source)
                      case dialect => Right(dialect)

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
    case RecallResult.EngineLimit(message) => Json.obj("kind" := "engine-limit", "message" := message)
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
    case ErrorKind.RecallFailureLimit =>
      Json.obj("kind" := "recall-failure-limit")
    case ErrorKind.Unexpected(message) =>
      Json.obj("kind" := "unexpected", "message" := message)

/** An `Encoder` for `UString`. */
given encodeUString: Encoder[UString] = _.asString.asJson

/** Returns a `Decoder` for `UString` of the unit mode, which is the inverse of [[encodeUString]]. */
def decodeUString(mode: UnitMode): Decoder[UString] = Decoder[String].emap: s =>
  mode match
    case UnitMode.UTF16     => Right(UString.fromUTF16Units(s))
    case UnitMode.CodePoint => Right(UString.fromString(s))
    case UnitMode.Byte      =>
      if s.forall(_ <= 0xff) then Right(UString.fromUTF16Units(s)) else Left(s"Invalid byte string: $s")

/** Returns a `Decoder` for `AttackPattern` of the unit mode in the style.
  *
  * It reads the fields of [[encodeAttackPattern]] except `string` and `pattern`, which are derived from the others.
  */
def decodeAttackPattern(mode: UnitMode, style: AttackPattern.Style): Decoder[AttackPattern] =
  given Decoder[UString] = decodeUString(mode)
  given Decoder[(UString, UString, Int)] = (c: HCursor) =>
    for
      prefix <- c.get[UString]("prefix")
      pump <- c.get[UString]("pump")
      bias <- c.get[Int]("bias")
    yield (prefix, pump, bias)
  (c: HCursor) =>
    for
      pumps <- c.get[Seq[(UString, UString, Int)]]("pumps")
      suffix <- c.get[UString]("suffix")
      n <- c.get[Int]("base")
    yield AttackPattern(pumps, suffix, n, style)

/** A `Decoder` for `Duration`. */
given decodeDuration: Decoder[Duration] = (c: HCursor) =>
  if c.value.isNull then Right(Duration.Inf)
  else
    Decoder[Int].tryDecode(c).map(t => Duration(t, MILLISECONDS)).orElse(Left(DecodingFailure("Duration", c.history)))
