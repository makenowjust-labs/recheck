package codes.quine.labo.recheck

import scala.concurrent.duration._
import scala.util.Random

import scalajs.js
import scalajs.js.JSConverters._
import common.Context
import common.Checker
import data.UString
import diagnostics.AttackComplexity
import diagnostics.AttackPattern
import diagnostics.Diagnostics

/** DiagnosticsJS is a JS wrapper for Diagnostics. */
trait DiagnosticsJS extends js.Object {

  /** A status of this diagnostics. One of `safe`, `vulnerable` and `unknown`. */
  def status: String

  /** A checker named to be used. */
  def checker: js.UndefOr[String]

  /** An attack string. It is available on `vulnerable` diagnostics. */
  def attack: js.UndefOr[AttackPatternJS]

  /** A matching-time complexity. It is available on `safe` or `vulnerable` diagnostics. */
  def complexity: js.UndefOr[AttackComplexityJS]

  /** An error kind. It is available on `unknown` diagnostics. */
  def error: js.UndefOr[ErrorKindJS]
}

/** DiagnosticsJS utilities. */
object DiagnosticsJS {

  /** Constructs a DiagnosticsJS from the actual Diagnostics. */
  def from(d: Diagnostics): DiagnosticsJS = d match {
    case Diagnostics.Safe(c, checker) =>
      js.Dynamic
        .literal(status = "safe", checker = checker.toString, complexity = AttackComplexityJS.from(c))
        .asInstanceOf[DiagnosticsJS]
    case Diagnostics.Vulnerable(c, a, checker) =>
      js.Dynamic
        .literal(
          status = "vulnerable",
          checker = checker.toString,
          attack = AttackPatternJS.from(a),
          complexity = AttackComplexityJS.from(c)
        )
        .asInstanceOf[DiagnosticsJS]
    case Diagnostics.Unknown(k, checker) =>
      js.Dynamic
        .literal(status = "unknown", checker = checker.map(_.toString).orUndefined, error = ErrorKindJS.from(k))
        .asInstanceOf[DiagnosticsJS]
  }
}

/** AttackComplexityJS is a JS wrapper for AttackComplexity. */
trait AttackComplexityJS extends js.Object {

  /** A type of this complexity. One of `constant`, `linear`, `exponential` and `polynomial`. */
  def `type`: String

  /** When it is `true`, this complexity maybe wrong. Otherwise, this comes from a precise analysis. */
  def isFuzz: Boolean

  /** A polynomial complexity's degree. It is available on `polynomial` complexity. */
  def degree: js.UndefOr[Int]
}

/** AttackComplexityJS utilities. */
object AttackComplexityJS {

  /** Constructs a AttackComplexityJS from the actual AttackComplexity. */
  def from(c: AttackComplexity): AttackComplexityJS = c match {
    case AttackComplexity.Constant =>
      js.Dynamic.literal(`type` = "constant", isFuzz = false).asInstanceOf[AttackComplexityJS]
    case AttackComplexity.Linear =>
      js.Dynamic.literal(`type` = "linear", isFuzz = false).asInstanceOf[AttackComplexityJS]
    case c: AttackComplexity.Safe =>
      js.Dynamic.literal(`type` = "safe", isFuzz = c.isFuzz).asInstanceOf[AttackComplexityJS]
    case AttackComplexity.Polynomial(d, fuzz) =>
      js.Dynamic.literal(`type` = "polynomial", degree = d, isFuzz = fuzz).asInstanceOf[AttackComplexityJS]
    case AttackComplexity.Exponential(fuzz) =>
      js.Dynamic.literal(`type` = "exponential", isFuzz = fuzz).asInstanceOf[AttackComplexityJS]
  }
}

/** AttackPatternJS is a JS wrapper for AttackPattern. */
trait AttackPatternJS extends js.Object {

  /** Pump objects. */
  def pumps: js.Array[PumpJS]

  /** A suffix string. */
  def suffix: String

  /** A repeat base. */
  def base: Int

  /** A string representation of this. */
  def string: String
}

/** AttackPatternJS utilities. */
object AttackPatternJS {

  /** Constructs a WitnessJS from the actual Witness. */
  def from(a: AttackPattern): AttackPatternJS =
    js.Dynamic
      .literal(
        pumps = a.pumps.map(PumpJS.from).toJSArray,
        suffix = a.suffix.asString,
        base = a.n,
        string = a.asUString.asString
      )
      .asInstanceOf[AttackPatternJS]
}

/** PumpJS is a JS wrapper for Pump. */
trait PumpJS extends js.Object {

  /** A prefix string. */
  def prefix: String

  /** A pump string. */
  def pump: String

  /** A repeat bias. */
  def bias: Int
}

/** PumpJS utilities. */
object PumpJS {

  /** Constructs a PumpJS from the actual Pump. */
  def from(p: (UString, UString, Int)): PumpJS =
    js.Dynamic
      .literal(prefix = p._1.asString, pump = p._2.asString, bias = p._3)
      .asInstanceOf[PumpJS]
}

/** ErrorKindJS is a JS wrapper for ErrorKind. */
trait ErrorKindJS extends js.Object {

  /** An error kind name. One of `timeout`, `unsupported` and `invalid`. */
  def kind: String

  /** An error message. It is available on `unsupported` and `invalid` error. */
  def message: js.UndefOr[String]
}

/** ErrorKindJS utilities. */
object ErrorKindJS {

  /** Constructs a ErrorKindJS from the actual ErrorKind. */
  def from(k: Diagnostics.ErrorKind): ErrorKindJS = k match {
    case Diagnostics.ErrorKind.Timeout => js.Dynamic.literal(kind = "timeout").asInstanceOf[ErrorKindJS]
    case Diagnostics.ErrorKind.Unsupported(msg) =>
      js.Dynamic.literal(kind = "unsupported", message = msg).asInstanceOf[ErrorKindJS]
    case Diagnostics.ErrorKind.InvalidRegExp(msg) =>
      js.Dynamic.literal(kind = "invalid", message = msg).asInstanceOf[ErrorKindJS]
  }
}

/** ConfigJS is a JS wrapper for Config. */
trait ConfigJS extends js.Object {

  /** A timeout duration in a check. */
  def timeout: js.UndefOr[Int]

  /** A checker to use. */
  def checker: js.UndefOr[String]

  /** A maximum size of an attack string. */
  def maxAttackSize: js.UndefOr[Int]

  /** A limit of VM execution steps on attack string construction. */
  def attackLimit: js.UndefOr[Int]

  /** A seed value for random instance. */
  def randomSeed: js.UndefOr[Int]

  /** A limit of VM execution steps on seeding. */
  def seedLimit: js.UndefOr[Int]

  /** A limit of VM execution steps on population. */
  def populationLimit: js.UndefOr[Int]

  /** A size to compute crossing. */
  def crossSize: js.UndefOr[Int]

  /** A size to compute mutation. */
  def mutateSize: js.UndefOr[Int]

  /** A maximum size of a initial seed set. */
  def maxSeedSize: js.UndefOr[Int]

  /** A maximum size of a population on a generation. */
  def maxGenerationSize: js.UndefOr[Int]

  /** A maximum iteration number of GA. */
  def maxIteration: js.UndefOr[Int]

  /** A maximum degree number to attempt on building attack string. * */
  def maxDegree: js.UndefOr[Int]

  /** A maximum number of sum of repeat counts like `/a{10}/`.
    * If this value is exceeded, it switches to use fuzzing based checker.
    */
  def maxRepeatCount: js.UndefOr[Int]

  /** A maximum transition size of NFA to use the automaton based checker.
    * If this value is exceeded, it switches to use fuzzing based checker.
    */
  def maxNFASize: js.UndefOr[Int]
}

/** ConfigJS utilities. */
object ConfigJS {

  /** Constructs a Config instance from ConfigJS object. */
  def from(config: ConfigJS): Config = {
    val context = Context(timeout = config.timeout.map(_.milli).getOrElse(Duration.Inf))

    val checker = config.checker.getOrElse("hybrid") match {
      case "hybrid"    => Checker.Hybrid
      case "automaton" => Checker.Automaton
      case "fuzz"      => Checker.Fuzz
    }
    val random = config.randomSeed.map(new Random(_)).getOrElse(Random)

    Config(
      context,
      checker,
      config.maxAttackSize.getOrElse(Config.MaxAttackSize),
      config.attackLimit.getOrElse(Config.AttackLimit),
      random,
      config.seedLimit.getOrElse(Config.SeedLimit),
      config.populationLimit.getOrElse(Config.PopulationLimit),
      config.crossSize.getOrElse(Config.CrossSize),
      config.mutateSize.getOrElse(Config.MutateSize),
      config.maxSeedSize.getOrElse(Config.MaxSeedSize),
      config.maxGenerationSize.getOrElse(Config.MaxGenerationSize),
      config.maxIteration.getOrElse(Config.MaxIteration),
      config.maxDegree.getOrElse(Config.MaxDegree),
      config.maxRepeatCount.getOrElse(Config.MaxRepeatCount),
      config.maxNFASize.getOrElse(Config.MaxNFASize)
    )
  }
}
