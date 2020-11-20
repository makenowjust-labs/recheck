package codes.quine.labo.redos

import scala.concurrent.duration._
import scala.util.Random

import scalajs.js
import scalajs.js.JSConverters._
import automaton.Complexity
import automaton.Witness
import data.IChar
import util.Timeout

/** DiagnosticsJS is a JS wrapper for Diagnostics. */
trait DiagnosticsJS extends js.Object {

  /** A status of this diagnostics. One of `safe`, `vulnerable` and `unknown`. */
  def status: String

  /** An attack string. It is available on `vulnerable` diagnostics. */
  def attack: js.UndefOr[String]

  /** A matching-time complexity. It is available on `safe` or `vulnerable` diagnostics. */
  def complexity: js.UndefOr[ComplexityJS]

  /** An error kind. It is available on `unknown` diagnostics. */
  def error: js.UndefOr[ErrorKindJS]
}

/** DiagnosticsJS utilities. */
object DiagnosticsJS {

  /** Constructs a DiagnosticsJS from the actual Diagnostics. */
  def from(d: Diagnostics): DiagnosticsJS = d match {
    case Diagnostics.Safe(c) =>
      js.Dynamic
        .literal(status = "safe", complexity = c.map(ComplexityJS.from(_)).orUndefined)
        .asInstanceOf[DiagnosticsJS]
    case Diagnostics.Vulnerable(a, c) =>
      js.Dynamic
        .literal(
          status = "vulnerable",
          attack = a.asString,
          complexity = c.map(ComplexityJS.from(_)).orUndefined
        )
        .asInstanceOf[DiagnosticsJS]
    case Diagnostics.Unknown(k) =>
      js.Dynamic.literal(status = "unknown", error = ErrorKindJS.from(k)).asInstanceOf[DiagnosticsJS]
  }
}

/** ComplexityJS is a JS wrapper for Complexity. */
trait ComplexityJS extends js.Object {

  /** A type of this complexity. One of `constant`, `linear`, `exponential` and `polynomial`. */
  def `type`: String

  /** A polynomial complexity's degree. It is available on `polynomial` complexity. */
  def degree: js.UndefOr[Int]

  /** A witness pattern. It is available on `exponential` or `polynomial` complexity. */
  def witness: js.UndefOr[WitnessJS]
}

/** ComplexityJS utilities. */
object ComplexityJS {

  /** Constructs a ComplexityJS from the actual Complexity. */
  def from(c: Complexity[IChar]): ComplexityJS = c match {
    case Complexity.Constant => js.Dynamic.literal(`type` = "constant").asInstanceOf[ComplexityJS]
    case Complexity.Linear   => js.Dynamic.literal(`type` = "linear").asInstanceOf[ComplexityJS]
    case Complexity.Polynomial(d, w) =>
      js.Dynamic.literal(`type` = "polynomial", degree = d, witness = WitnessJS.from(w)).asInstanceOf[ComplexityJS]
    case Complexity.Exponential(w) =>
      js.Dynamic.literal(`type` = "exponential", witness = WitnessJS.from(w)).asInstanceOf[ComplexityJS]
  }
}

/** WitnessJS is a JS wrapper for Witness. */
trait WitnessJS extends js.Object {

  /** Pump objects. */
  def pumps: js.Array[PumpJS]

  /** A suffix string. */
  def suffix: String
}

/** WitnessJS utilities. */
object WitnessJS {

  /** Constructs a WitnessJS from the actual Witness. */
  def from(w: Witness[IChar]): WitnessJS =
    js.Dynamic
      .literal(pumps = w.pumps.map(PumpJS.from(_)).toJSArray, suffix = IStringJS.from(w.suffix))
      .asInstanceOf[WitnessJS]
}

/** PumpJS is a JS wrapper for Pump. */
trait PumpJS extends js.Object {

  /** A prefix string. */
  def prefix: String

  /** A pump string. */
  def pump: String
}

/** PumpJS utilities. */
object PumpJS {

  /** Constructs a PumpJS from the actual Pump. */
  def from(p: (Seq[IChar], Seq[IChar])): PumpJS =
    js.Dynamic.literal(prefix = IStringJS.from(p._1), pump = IStringJS.from(p._2)).asInstanceOf[PumpJS]
}

/** IString (`Seq[IChar]`) utilities. */
object IStringJS {

  /** Constructs a String from the actual IString. */
  def from(seq: Seq[IChar]): String = seq.map(_.head.asString).mkString
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

  /** A ratio of a VM step and a NFA transition. */
  def stepRate: js.UndefOr[Double]

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
    val timeout = Timeout.from(config.timeout.map(_.milli).getOrElse(Duration.Inf))
    val checker = config.checker.getOrElse("hybrid") match {
      case "hybrid"    => Checker.Hybrid
      case "automaton" => Checker.Automaton
      case "fuzz"      => Checker.Fuzz
    }
    val random = config.randomSeed.map(new Random(_)).getOrElse(Random)

    Config(
      timeout,
      checker,
      config.maxAttackSize.getOrElse(Config.MaxAttackSize),
      config.attackLimit.getOrElse(Config.AttackLimit),
      config.stepRate.getOrElse(Config.StepRate),
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
