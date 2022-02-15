package codes.quine.labs.recheck.common

import scala.concurrent.duration._

/** Parameters is an immutable representation of parameters.
  *
  * @param checker
  *
  * The type of checker to be used.
  *
  * There are three checker types.
  *
  *   - `auto` checker uses the criteria to decide which algorithm is better to use against a regular expression, the
  *     algorithm based on automata theory or the fuzzing algorithm.
  *   - `fuzz` checker uses the fuzzing algorithm with static analysis.
  *   - `automaton` checker uses the algorithm based on automata theory.
  *
  * (default: `Checker.Auto`)
  *
  * @param timeout
  *
  * The upper limit of checking time.
  *
  * If the checking time exceeds this limit, the result will be reported as `timeout`. If the value is positive infinite
  * in Scala or `null` in TypeScript, the result never becomes `timeout`.
  *
  * The `timeout` time begins to be measured as soon as the check starts. Note that the `timeout` does not occur while
  * the input is in the queue waiting to be checked.
  *
  * In TypeScript, a number value is treated as in milliseconds.
  *
  * (default: `Duration(10, SECONDS)`)
  *
  * @param logger
  *
  * The logger function to record execution traces.
  *
  * To disable the logging, `null` in TypeScript or `None` in Scala should be passed.
  *
  * (default: `None`)
  *
  * @param randomSeed
  *
  * The PRNG seed number.
  *
  * (default: `0`)
  *
  * @param maxIteration
  *
  * The maximum number of fuzzing iteration.
  *
  * (default: `10`)
  *
  * @param seeder
  *
  * The type of seeder to be used in fuzzing.
  *
  * There are two seeders.
  *
  *   - `static` seeder uses the seeding algorithm based on the automata theory.
  *   - `dynamic` seeder uses the seeding algorithm with dynamic analysis.
  *
  * (default: `Seeder.Static`)
  *
  * @param maxSimpleRepeatCount
  *
  * The maximum number of each repetition quantifier’s repeat count on `static` seeding.
  *
  * (default: `30`)
  *
  * @param seedingLimit
  *
  * The upper limit on the number of characters read by VM on `dynamic` seeding.
  *
  * (default: `1000`)
  *
  * @param seedingTimeout
  *
  * The upper limit of matching time on `dynamic` seeding.
  *
  * (default: `Duration(100, MILLISECONDS)`)
  *
  * @param maxInitialGenerationSize
  *
  * The maximum size of the initial generation on fuzzing.
  *
  * (default: `500`)
  *
  * @param incubationLimit
  *
  * The upper limit on the number of characters read by VM on incubation.
  *
  * (default: `25000`)
  *
  * @param incubationTimeout
  *
  * The upper limit of matching time on incubation.
  *
  * (default: `Duration(250, MILLISECONDS)`)
  *
  * @param maxGeneStringSize
  *
  * The maximum length of the gene string on fuzzing.
  *
  * (default: `2400`)
  *
  * @param maxGenerationSize
  *
  * The maximum size of each generation on fuzzing.
  *
  * (default: `100`)
  *
  * @param crossoverSize
  *
  * The number of crossover on each generation.
  *
  * (default: `25`)
  *
  * @param mutationSize
  *
  * The number of mutation on each generation.
  *
  * (default: `50`)
  *
  * @param attackLimit
  *
  * The upper limit on the number of characters read by VM on the attack.
  *
  * (default: `1500000000`)
  *
  * @param attackTimeout
  *
  * The upper limit of matching time on the attack.
  *
  * (default: `Duration(1, SECONDS)`)
  *
  * @param maxAttackStringSize
  *
  * The maximum length of the attack string on fuzzing.
  *
  * (default: `300000`)
  *
  * @param maxDegree
  *
  * The maximum degree to be considered in fuzzing.
  *
  * (default: `4`)
  *
  * @param heatRatio
  *
  * The ratio of the number of characters read to the maximum number to be considered as a hot spot.
  *
  * (default: `0.001`)
  *
  * @param accelerationMode
  *
  * The type of acceleration mode strategy on fuzzing.
  *
  * There are three acceleration mode strategies.
  *
  *   - `auto` uses acceleration mode as default. However, if the regular expression has backreferences, it turns off
  *     the acceleration mode.
  *   - `on` turns on the acceleration mode.
  *   - `off` turns off the acceleration mode.
  *
  * (default: `AccelerationMode.Auto`)
  *
  * @param maxRepeatCount
  *
  * The maximum number of sum of repetition quantifier’s repeat counts to determine which algorithm is used.
  *
  * (default: `30`)
  *
  * @param maxPatternSize
  *
  * The maximum size of the regular expression pattern to determine which algorithm is used.
  *
  * (default: `1500`)
  *
  * @param maxNFASize
  *
  * The maximum size of NFA to determine which algorithm is used.
  *
  * (default: `35000`)
  *
  * @param recallLimit
  *
  * The upper limit on the number of characters read by VM on the recall validation.
  *
  * (default: `1500000000`)
  *
  * @param recallTimeout
  *
  * The upper limit of matching time on the recall validation.
  *
  * If this value is negative, then the recall validation is skipped.
  *
  * (default: `Duration(-1, SECONDS)`)
  *
  * @param maxRecallStringSize
  *
  * The maximum length of the attack string on recall validation.
  *
  * (default: `300000`)
  */
final case class Parameters(
    checker: Checker = Parameters.DefaultChecker,
    timeout: Duration = Parameters.DefaultTimeout,
    logger: Option[Context.Logger] = Parameters.DefaultLogger,
    randomSeed: Long = Parameters.DefaultRandomSeed,
    maxIteration: Int = Parameters.DefaultMaxIteration,
    seeder: Seeder = Parameters.DefaultSeeder,
    maxSimpleRepeatCount: Int = Parameters.DefaultMaxSimpleRepeatCount,
    seedingLimit: Int = Parameters.DefaultSeedingLimit,
    seedingTimeout: Duration = Parameters.DefaultSeedingTimeout,
    maxInitialGenerationSize: Int = Parameters.DefaultMaxInitialGenerationSize,
    incubationLimit: Int = Parameters.DefaultIncubationLimit,
    incubationTimeout: Duration = Parameters.DefaultIncubationTimeout,
    maxGeneStringSize: Int = Parameters.DefaultMaxGeneStringSize,
    maxGenerationSize: Int = Parameters.DefaultMaxGenerationSize,
    crossoverSize: Int = Parameters.DefaultCrossoverSize,
    mutationSize: Int = Parameters.DefaultMutationSize,
    attackLimit: Int = Parameters.DefaultAttackLimit,
    attackTimeout: Duration = Parameters.DefaultAttackTimeout,
    maxAttackStringSize: Int = Parameters.DefaultMaxAttackStringSize,
    maxDegree: Int = Parameters.DefaultMaxDegree,
    heatRatio: Double = Parameters.DefaultHeatRatio,
    accelerationMode: AccelerationMode = Parameters.DefaultAccelerationMode,
    maxRepeatCount: Int = Parameters.DefaultMaxRepeatCount,
    maxPatternSize: Int = Parameters.DefaultMaxPatternSize,
    maxNFASize: Int = Parameters.DefaultMaxNFASize,
    recallLimit: Int = Parameters.DefaultRecallLimit,
    recallTimeout: Duration = Parameters.DefaultRecallTimeout,
    maxRecallStringSize: Int = Parameters.DefaultMaxRecallStringSize
)

object Parameters {
  // $COVERAGE-OFF$

  /** The default value of [[Parameters.checker]]. */
  val DefaultChecker: Checker = Checker.Auto

  /** The default value of [[Parameters.timeout]]. */
  val DefaultTimeout: Duration = Duration(10, SECONDS)

  /** The default value of [[Parameters.logger]]. */
  val DefaultLogger: Option[Context.Logger] = None

  /** The default value of [[Parameters.randomSeed]]. */
  val DefaultRandomSeed: Long = 0

  /** The default value of [[Parameters.maxIteration]]. */
  val DefaultMaxIteration: Int = 10

  /** The default value of [[Parameters.seeder]]. */
  val DefaultSeeder: Seeder = Seeder.Static

  /** The default value of [[Parameters.maxSimpleRepeatCount]]. */
  val DefaultMaxSimpleRepeatCount: Int = 30

  /** The default value of [[Parameters.seedingLimit]]. */
  val DefaultSeedingLimit: Int = 1000

  /** The default value of [[Parameters.seedingTimeout]]. */
  val DefaultSeedingTimeout: Duration = Duration(100, MILLISECONDS)

  /** The default value of [[Parameters.maxInitialGenerationSize]]. */
  val DefaultMaxInitialGenerationSize: Int = 500

  /** The default value of [[Parameters.incubationLimit]]. */
  val DefaultIncubationLimit: Int = 25000

  /** The default value of [[Parameters.incubationTimeout]]. */
  val DefaultIncubationTimeout: Duration = Duration(250, MILLISECONDS)

  /** The default value of [[Parameters.maxGeneStringSize]]. */
  val DefaultMaxGeneStringSize: Int = 2400

  /** The default value of [[Parameters.maxGenerationSize]]. */
  val DefaultMaxGenerationSize: Int = 100

  /** The default value of [[Parameters.crossoverSize]]. */
  val DefaultCrossoverSize: Int = 25

  /** The default value of [[Parameters.mutationSize]]. */
  val DefaultMutationSize: Int = 50

  /** The default value of [[Parameters.attackLimit]]. */
  val DefaultAttackLimit: Int = 1500000000

  /** The default value of [[Parameters.attackTimeout]]. */
  val DefaultAttackTimeout: Duration = Duration(1, SECONDS)

  /** The default value of [[Parameters.maxAttackStringSize]]. */
  val DefaultMaxAttackStringSize: Int = 300000

  /** The default value of [[Parameters.maxDegree]]. */
  val DefaultMaxDegree: Int = 4

  /** The default value of [[Parameters.heatRatio]]. */
  val DefaultHeatRatio: Double = 0.001

  /** The default value of [[Parameters.accelerationMode]]. */
  val DefaultAccelerationMode: AccelerationMode = AccelerationMode.Auto

  /** The default value of [[Parameters.maxRepeatCount]]. */
  val DefaultMaxRepeatCount: Int = 30

  /** The default value of [[Parameters.maxPatternSize]]. */
  val DefaultMaxPatternSize: Int = 1500

  /** The default value of [[Parameters.maxNFASize]]. */
  val DefaultMaxNFASize: Int = 35000

  /** The default value of [[Parameters.recallLimit]]. */
  val DefaultRecallLimit: Int = 1500000000

  /** The default value of [[Parameters.recallTimeout]]. */
  val DefaultRecallTimeout: Duration = Duration(-1, SECONDS)

  /** The default value of [[Parameters.maxRecallStringSize]]. */
  val DefaultMaxRecallStringSize: Int = 300000

  // $COVERAGE-ON$
}
