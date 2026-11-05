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
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.Diagnostics.ErrorKind
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.RefutedAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** Package `codec` provides recheck types `Decoder`/`Encoder` of circe. */
package object codec

/** An `Encoder` for `Diagnostics`. */
given encodeDiagnostics: Encoder[Diagnostics] with
  def apply(d: Diagnostics): Json = d match
    case Diagnostics.Safe(source, flags, dialect, unitMode, complexity, refutedAttacks, checker) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "dialect" := dialect,
        "unitMode" := unitMode,
        "status" := "safe",
        "complexity" := complexity.asInstanceOf[AttackComplexity],
        "refutedAttacks" := refutedAttacks.map(encodeRefutedAttack(dialect, unitMode)(_)),
        "checker" := checker
      )
    case Diagnostics.Vulnerable(
          source,
          flags,
          dialect,
          unitMode,
          complexity,
          attack,
          hotspot,
          runtimeAttack,
          refutedAttacks,
          checker
        ) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "dialect" := dialect,
        "unitMode" := unitMode,
        "status" := "vulnerable",
        "complexity" := complexity.asInstanceOf[AttackComplexity],
        "attack" -> encodeAttackPattern(dialect, unitMode)(attack),
        "hotspot" := hotspot,
        "runtimeAttack" := runtimeAttack,
        "refutedAttacks" := refutedAttacks.map(encodeRefutedAttack(dialect, unitMode)(_)),
        "checker" := (checker: Checker)
      )
    case Diagnostics.Unknown(source, flags, dialect, unitMode, error, refutedAttacks, checker) =>
      Json.obj(
        "source" := source,
        "flags" := flags,
        "dialect" := dialect,
        "unitMode" := unitMode,
        "status" := "unknown",
        "error" := error,
        "refutedAttacks" := refutedAttacks.map(encodeRefutedAttack(dialect, unitMode.get)(_)),
        "checker" := checker
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

/** An `Encoder` for `UnitMode`. */
given encodeUnitMode: Encoder[UnitMode] with
  def apply(m: UnitMode): Json = m match
    case UnitMode.UTF16     => "utf16".asJson
    case UnitMode.CodePoint => "code-point".asJson
    case UnitMode.Byte      => "byte".asJson

/** A `Decoder` for `UnitMode`, which is the inverse of [[encodeUnitMode]]. */
given decodeUnitMode: Decoder[UnitMode] = Decoder[String].emap:
  case "utf16"      => Right(UnitMode.UTF16)
  case "code-point" => Right(UnitMode.CodePoint)
  case "byte"       => Right(UnitMode.Byte)
  case name         => Left(s"Unknown unit mode: $name")

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

/** Returns an `Encoder` for `AttackPattern` of the unit mode, whose `pattern` is in the style of the dialect. */
def encodeAttackPattern(dialect: Dialect, mode: UnitMode): Encoder[AttackPattern] =
  given Encoder[UString] = encodeUString(mode)
  val style = AttackPatternStyle.of(dialect, mode)
  (p: AttackPattern) =>
    val pumps = p.pumps.map:
      case (p, s, n) => Json.obj("prefix" := p, "pump" := s, "bias" := n)
    Json.obj(
      "pumps" := Json.arr(pumps*),
      "suffix" := p.suffix,
      "base" := p.n,
      "string" := p.asUString,
      "pattern" := p.toString(style)
    )

/** Returns an `Encoder` for `RefutedAttack` of the unit mode, whose `attack` is in the style of the dialect. */
def encodeRefutedAttack(dialect: Dialect, mode: UnitMode): Encoder[RefutedAttack] =
  given Encoder[AttackPattern] = encodeAttackPattern(dialect, mode)
  (r: RefutedAttack) =>
    Json.obj(
      "complexity" := r.complexity.asInstanceOf[AttackComplexity],
      "attack" := r.attack,
      "hotspot" := r.hotspot,
      "runtimeAttack" := r.runtimeAttack
    )

/** An `Encoder` for `Hotspot`. */
given encodeHotspot: Encoder[Hotspot] with
  def apply(h: Hotspot): Json =
    val spots = h.spots.map:
      case Hotspot.Spot(s, e, t) => Json.obj("start" := s, "end" := e, "temperature" := t.toString)
    Json.arr(spots*)

/** An `Encoder` for `RuntimeAttack`. */
given encodeRuntimeAttack: Encoder[RuntimeAttack] with
  def apply(r: RuntimeAttack): Json =
    val timeout = r.timeout match
      case d: FiniteDuration => d.toMillis.asJson
      case _                 => Json.Null
    Json.obj(
      "runtime" := r.runtime,
      "timeout" := timeout,
      "result" := r.result,
      "isConfirmed" := r.isConfirmed
    )

/** An `Encoder` for `RuntimeAttackResult`. */
given encodeRuntimeAttackResult: Encoder[RuntimeAttackResult] with
  def apply(r: RuntimeAttackResult): Json = r match
    case RuntimeAttackResult.Finish(time)         => Json.obj("kind" := "finish", "time" := time.toMillis)
    case RuntimeAttackResult.Timeout              => Json.obj("kind" := "timeout")
    case RuntimeAttackResult.EngineLimit(message) => Json.obj("kind" := "engine-limit", "message" := message)
    case RuntimeAttackResult.EngineError(message) => Json.obj("kind" := "engine-error", "message" := message)

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
    case ErrorKind.RuntimeAttacksRefuted =>
      Json.obj("kind" := "runtime-attacks-refuted")
    case ErrorKind.Unexpected(message) =>
      Json.obj("kind" := "unexpected", "message" := message)

/** Returns an `Encoder` for `UString` of the unit mode, which gives the string of the mode, or the array of the units
  * when the string does not read back as them.
  */
def encodeUString(mode: UnitMode): Encoder[UString] = (s: UString) =>
  val str = mode.encode(s)
  // A lone surrogate cannot be written in UTF-8, and a surrogate pair of code points reads back as one code point.
  if !hasLoneSurrogate(str) && mode.decode(str) == s then str.asJson else s.iterator.toSeq.asJson

/** Returns a `Decoder` for `UString` of the unit mode, which reads a string or an array of units. */
def decodeUString(mode: UnitMode): Decoder[UString] =
  val fromString = Decoder[String].emap: s =>
    Either.cond(mode != UnitMode.Byte || s.forall(_ <= 0xff), mode.decode(s), s"Invalid byte string: $s")
  val fromUnits = Decoder[Seq[Int]].emap: us =>
    us.find(!mode.contains(_)) match
      case Some(u) => Left(f"Invalid unit in $mode mode: $u%X")
      case None    => Right(UString.from(us))
  (c: HCursor) => if c.value.isArray then fromUnits(c) else fromString(c)

/** Tests whether the string has a surrogate that is not a part of a surrogate pair. */
private def hasLoneSurrogate(s: String): Boolean =
  var i = 0
  while i < s.length do
    val c = s.charAt(i)
    if Character.isHighSurrogate(c) && i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1)) then i += 2
    else if Character.isSurrogate(c) then return true
    else i += 1
  false

/** Returns a `Decoder` for `AttackPattern` of the unit mode.
  *
  * It reads the fields of [[encodeAttackPattern]] except `string` and `pattern`, which are derived from the others.
  */
def decodeAttackPattern(mode: UnitMode): Decoder[AttackPattern] =
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
    yield AttackPattern(pumps, suffix, n)

/** A `Decoder` for `Duration`. */
given decodeDuration: Decoder[Duration] = (c: HCursor) =>
  if c.value.isNull then Right(Duration.Inf)
  else
    Decoder[Int].tryDecode(c).map(t => Duration(t, MILLISECONDS)).orElse(Left(DecodingFailure("Duration", c.history)))
