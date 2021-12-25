package codes.quine.labo.recheck
package common

import scala.concurrent.duration._

/** Parameters is an immutable representation of parameters.
  *
  * @param checker
  *   Checker Type of checker used for analysis.
  *
  * There are three checkers:
  *
  *   - `'automaton'`: A checker which works based on automaton theory. It can analyze ReDoS vulnerability of the RegExp
  *     without false positive, however, it needs some minutes against some RegExp and it does not support some syntax.
  *   - `'fuzz'`: A checker based on fuzzing. It can detect ReDoS vulnerability against the all RegExp syntax including
  *     back-references and look-around assertions. However, it needs some seconds on average and it may cause false
  *     negative.
  *   - `'hybrid'`: A checker which combines the automaton checker and the fuzzing checker. If the RegExp is supported
  *     by the automaton checker and some thresholds are passed, it uses the automaton checker. Otherwise, it falls back
  *     to the fuzzing checker.
  *
  * The hybrid checker performs better than others in many cases. (default: `common.Checker.Hybrid`)
  *
  * @param timeout
  *   Duration Upper limit of analysis time.
  *
  * If the analysis time exceeds this value, the result will be reported as a timeout. If the value is the positive
  * infinite duration, the result never become a timeout.
  *
  * (default: `Duration(10, SECONDS)`)
  *
  * @param logger
  *   Option[Context.Logger] Logger to log an analysis execution. (default: `None`)
  *
  * @param maxAttackStringSize
  *   Int Maximum length of an attack string. (default: `300000`)
  *
  * @param attackLimit
  *   Int Upper limit on the number of characters read by the VM during attack string construction. (default:
  *   `1500000000`)
  *
  * @param randomSeed
  *   Long Seed value for PRNG used by fuzzing. (default: `0`)
  *
  * @param maxIteration
  *   Int Maximum number of iterations of genetic algorithm. (default: `10`)
  *
  * @param seeder
  *   Seeder Type of seeder used for constructing the initial generation of fuzzing.
  *
  * There are two seeders:
  *
  *   - `'static'`: Seeder to construct the initial generation by using static analysis to the given pattern.
  *   - `'dynamic'`: Seeder to construct the initial generation by using dynamic analysis to the given pattern.
  *     (default: `common.Seeder.Static`)
  *
  * @param maxSimpleRepeatCount
  *   Int Maximum number of sum of repeat counts for static seeder. (default: `30`)
  *
  * @param seedingLimit
  *   Int Upper limit on the number of characters read by the VM during seeding. (default: `1000`)
  *
  * @param seedingTimeout
  *   Duration Upper limit of VM execution time during seeding.
  *
  * (default: `Duration(100, MILLISECONDS)`)
  *
  * @param maxInitialGenerationSize
  *   Int Maximum population at the initial generation. (default: `500`)
  *
  * @param incubationLimit
  *   Int Upper limit on the number of characters read by the VM during incubation. (default: `25000`)
  *
  * @param incubationTimeout
  *   Duration Upper limit of VM execution time during incubation.
  *
  * (default: `Duration(250, MILLISECONDS)`)
  *
  * @param maxGeneStringSize
  *   Int Maximum length of an attack string on genetic algorithm iterations. (default: `2400`)
  *
  * @param maxGenerationSize
  *   Int Maximum population at a single generation. (default: `100`)
  *
  * @param crossoverSize
  *   Int Number of crossovers in a single generation. (default: `25`)
  *
  * @param mutationSize
  *   Int Number of mutations in a single generation. (default: `50`)
  *
  * @param attackTimeout
  *   Duration The upper limit of the VM execution time when constructing a attack string.
  *
  * If the execution time exceeds this value, the result will be reported as a vulnerable.
  *
  * (default: `Duration(1000, MILLISECONDS)`)
  *
  * @param maxDegree
  *   Int Maximum degree for constructing attack string. (default: `4`)
  *
  * @param heatRatio
  *   Double Ratio of the number of characters read to the maximum number to be considered a hotspot. (default: `0.001`)
  *
  * @param accelerationMode
  *   AccelerationMode Mode of acceleration of VM execution.
  *
  * There are three mode:
  *
  *   - `'auto'`: The automatic mode. When it is specified, VM acceleration is used for regular expressions contains no
  *     back-reference, because back-reference makes VM acceleration slow sometimes.
  *   - `'on'`: The force **on** mode.
  *   - `'off'`: The force **off** mode. (default: `common.AccelerationMode.Auto`)
  *
  * @param maxRepeatCount
  *   Int Maximum number of sum of repeat counts.
  *
  * If this value is exceeded, it switches to use the fuzzing checker. (default: `30`)
  *
  * @param maxNFASize
  *   Int Maximum transition size of NFA to use the automaton checker.
  *
  * If transition size of NFA (and also DFA because it is larger in general) exceeds this value, it switches to use the
  * fuzzing checker. (default: `35000`)
  *
  * @param maxPatternSize
  *   Int Maximum pattern size to use the automaton checker.
  *
  * If this value is exceeded, it switches to use the fuzzing checker. (default: `1500`)
  */
final case class Parameters(
    checker: Checker = Parameters.Checker,
    timeout: Duration = Parameters.Timeout,
    logger: Option[Context.Logger] = Parameters.Logger,
    maxAttackStringSize: Int = Parameters.MaxAttackStringSize,
    attackLimit: Int = Parameters.AttackLimit,
    randomSeed: Long = Parameters.RandomSeed,
    maxIteration: Int = Parameters.MaxIteration,
    seeder: Seeder = Parameters.Seeder,
    maxSimpleRepeatCount: Int = Parameters.MaxSimpleRepeatCount,
    seedingLimit: Int = Parameters.SeedingLimit,
    seedingTimeout: Duration = Parameters.SeedingTimeout,
    maxInitialGenerationSize: Int = Parameters.MaxInitialGenerationSize,
    incubationLimit: Int = Parameters.IncubationLimit,
    incubationTimeout: Duration = Parameters.IncubationTimeout,
    maxGeneStringSize: Int = Parameters.MaxGeneStringSize,
    maxGenerationSize: Int = Parameters.MaxGenerationSize,
    crossoverSize: Int = Parameters.CrossoverSize,
    mutationSize: Int = Parameters.MutationSize,
    attackTimeout: Duration = Parameters.AttackTimeout,
    maxDegree: Int = Parameters.MaxDegree,
    heatRatio: Double = Parameters.HeatRatio,
    accelerationMode: AccelerationMode = Parameters.AccelerationMode,
    maxRepeatCount: Int = Parameters.MaxRepeatCount,
    maxNFASize: Int = Parameters.MaxNFASize,
    maxPatternSize: Int = Parameters.MaxPatternSize
)

object Parameters {
  // $COVERAGE-OFF$
  /** The default value of [[Parameters.checker]]. */
  val Checker: Checker = common.Checker.Hybrid

  /** The default value of [[Parameters.timeout]]. */
  val Timeout: Duration = Duration(10, SECONDS)

  /** The default value of [[Parameters.logger]]. */
  val Logger: Option[Context.Logger] = None

  /** The default value of [[Parameters.maxAttackStringSize]]. */
  val MaxAttackStringSize: Int = 300000

  /** The default value of [[Parameters.attackLimit]]. */
  val AttackLimit: Int = 1500000000

  /** The default value of [[Parameters.randomSeed]]. */
  val RandomSeed: Long = 0

  /** The default value of [[Parameters.maxIteration]]. */
  val MaxIteration: Int = 10

  /** The default value of [[Parameters.seeder]]. */
  val Seeder: Seeder = common.Seeder.Static

  /** The default value of [[Parameters.maxSimpleRepeatCount]]. */
  val MaxSimpleRepeatCount: Int = 30

  /** The default value of [[Parameters.seedingLimit]]. */
  val SeedingLimit: Int = 1000

  /** The default value of [[Parameters.seedingTimeout]]. */
  val SeedingTimeout: Duration = Duration(100, MILLISECONDS)

  /** The default value of [[Parameters.maxInitialGenerationSize]]. */
  val MaxInitialGenerationSize: Int = 500

  /** The default value of [[Parameters.incubationLimit]]. */
  val IncubationLimit: Int = 25000

  /** The default value of [[Parameters.incubationTimeout]]. */
  val IncubationTimeout: Duration = Duration(250, MILLISECONDS)

  /** The default value of [[Parameters.maxGeneStringSize]]. */
  val MaxGeneStringSize: Int = 2400

  /** The default value of [[Parameters.maxGenerationSize]]. */
  val MaxGenerationSize: Int = 100

  /** The default value of [[Parameters.crossoverSize]]. */
  val CrossoverSize: Int = 25

  /** The default value of [[Parameters.mutationSize]]. */
  val MutationSize: Int = 50

  /** The default value of [[Parameters.attackTimeout]]. */
  val AttackTimeout: Duration = Duration(1000, MILLISECONDS)

  /** The default value of [[Parameters.maxDegree]]. */
  val MaxDegree: Int = 4

  /** The default value of [[Parameters.heatRatio]]. */
  val HeatRatio: Double = 0.001

  /** The default value of [[Parameters.accelerationMode]]. */
  val AccelerationMode: AccelerationMode = common.AccelerationMode.Auto

  /** The default value of [[Parameters.maxRepeatCount]]. */
  val MaxRepeatCount: Int = 30

  /** The default value of [[Parameters.maxNFASize]]. */
  val MaxNFASize: Int = 35000

  /** The default value of [[Parameters.maxPatternSize]]. */
  val MaxPatternSize: Int = 1500
  // $COVERAGE-ON$
}
