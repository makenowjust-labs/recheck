package codes.quine.labo.recheck.common

import scala.concurrent.duration._

/** Parameters is an immutable representation of [[codes.quine.labo.recheck.ReDoS.check ReDoS.check]] parameters.
  *
  * @param checker
  *   Checker Type of checker used for analysis. (default: `Checker.Hybrid`)
  *
  * @param timeout
  *   Duration Upper limit of analysis time.
  *
  * If the analysis time exceeds this value, the result will be reported as a timeout. If the value is the positive
  * infinite duration, the result never become a timeout.
  *
  * (default: `Duration(10, SECONDS)`)
  *
  * @param maxAttackStringSize
  *   Int Maximum length of an attack string. (default: `400000`)
  *
  * @param attackLimit
  *   Int Upper limit on the number of characters read by the VM during attack string construction. (default:
  *   `100000000`)
  *
  * @param randomSeed
  *   Long Seed value for PRNG used by fuzzing. (default: `0`)
  *
  * @param maxIteration
  *   Int Maximum number of iterations of genetic algorithm. (default: `30`)
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
  *   Int Maximum population at the initial generation. (default: `50`)
  *
  * @param incubationLimit
  *   Int Upper limit on the number of characters read by the VM during incubation. (default: `100000`)
  *
  * @param incubationTimeout
  *   Duration Upper limit of VM execution time during incubation.
  *
  * (default: `Duration(250, MILLISECONDS)`)
  *
  * @param maxGeneStringSize
  *   Int Maximum length of an attack string on genetic algorithm iterations. (default: `4000`)
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
  * @param usesAcceleration
  *   Boolean Whether to use acceleration for VM execution. (default: `true`)
  *
  * @param maxRepeatCount
  *   Int Maximum number of sum of repeat counts.
  *
  * If this value is exceeded, it switches to use the fuzzing checker. (default: `20`)
  *
  * @param maxNFASize
  *   Int Maximum transition size of NFA to use the automaton checker.
  *
  * If transition size of NFA (and also DFA because it is larger in general) exceeds this value, it switches to use the
  * fuzzing checker. (default: `40000`)
  *
  * @param maxPatternSize
  *   Int Maximum pattern size to use the automaton checker.
  *
  * If this value is exceeded, it switches to use the fuzzing checker. (default: `1500`)
  */
final case class Parameters(
    checker: Checker = Parameters.CHECKER,
    timeout: Duration = Parameters.TIMEOUT,
    maxAttackStringSize: Int = Parameters.MAX_ATTACK_STRING_SIZE,
    attackLimit: Int = Parameters.ATTACK_LIMIT,
    randomSeed: Long = Parameters.RANDOM_SEED,
    maxIteration: Int = Parameters.MAX_ITERATION,
    seedingLimit: Int = Parameters.SEEDING_LIMIT,
    seedingTimeout: Duration = Parameters.SEEDING_TIMEOUT,
    maxInitialGenerationSize: Int = Parameters.MAX_INITIAL_GENERATION_SIZE,
    incubationLimit: Int = Parameters.INCUBATION_LIMIT,
    incubationTimeout: Duration = Parameters.INCUBATION_TIMEOUT,
    maxGeneStringSize: Int = Parameters.MAX_GENE_STRING_SIZE,
    maxGenerationSize: Int = Parameters.MAX_GENERATION_SIZE,
    crossoverSize: Int = Parameters.CROSSOVER_SIZE,
    mutationSize: Int = Parameters.MUTATION_SIZE,
    attackTimeout: Duration = Parameters.ATTACK_TIMEOUT,
    maxDegree: Int = Parameters.MAX_DEGREE,
    heatRatio: Double = Parameters.HEAT_RATIO,
    usesAcceleration: Boolean = Parameters.USES_ACCELERATION,
    maxRepeatCount: Int = Parameters.MAX_REPEAT_COUNT,
    maxNFASize: Int = Parameters.MAX_N_F_A_SIZE,
    maxPatternSize: Int = Parameters.MAX_PATTERN_SIZE
)

object Parameters {

  /** The default value of [[Parameters.checker]]. */
  val CHECKER: Checker = Checker.Hybrid

  /** The default value of [[Parameters.timeout]]. */
  val TIMEOUT: Duration = Duration(10, SECONDS)

  /** The default value of [[Parameters.maxAttackStringSize]]. */
  val MAX_ATTACK_STRING_SIZE: Int = 400000

  /** The default value of [[Parameters.attackLimit]]. */
  val ATTACK_LIMIT: Int = 100000000

  /** The default value of [[Parameters.randomSeed]]. */
  val RANDOM_SEED: Long = 0

  /** The default value of [[Parameters.maxIteration]]. */
  val MAX_ITERATION: Int = 30

  /** The default value of [[Parameters.seedingLimit]]. */
  val SEEDING_LIMIT: Int = 1000

  /** The default value of [[Parameters.seedingTimeout]]. */
  val SEEDING_TIMEOUT: Duration = Duration(100, MILLISECONDS)

  /** The default value of [[Parameters.maxInitialGenerationSize]]. */
  val MAX_INITIAL_GENERATION_SIZE: Int = 50

  /** The default value of [[Parameters.incubationLimit]]. */
  val INCUBATION_LIMIT: Int = 100000

  /** The default value of [[Parameters.incubationTimeout]]. */
  val INCUBATION_TIMEOUT: Duration = Duration(250, MILLISECONDS)

  /** The default value of [[Parameters.maxGeneStringSize]]. */
  val MAX_GENE_STRING_SIZE: Int = 4000

  /** The default value of [[Parameters.maxGenerationSize]]. */
  val MAX_GENERATION_SIZE: Int = 100

  /** The default value of [[Parameters.crossoverSize]]. */
  val CROSSOVER_SIZE: Int = 25

  /** The default value of [[Parameters.mutationSize]]. */
  val MUTATION_SIZE: Int = 50

  /** The default value of [[Parameters.attackTimeout]]. */
  val ATTACK_TIMEOUT: Duration = Duration(1000, MILLISECONDS)

  /** The default value of [[Parameters.maxDegree]]. */
  val MAX_DEGREE: Int = 4

  /** The default value of [[Parameters.heatRatio]]. */
  val HEAT_RATIO: Double = 0.001

  /** The default value of [[Parameters.usesAcceleration]]. */
  val USES_ACCELERATION: Boolean = true

  /** The default value of [[Parameters.maxRepeatCount]]. */
  val MAX_REPEAT_COUNT: Int = 20

  /** The default value of [[Parameters.maxNFASize]]. */
  val MAX_N_F_A_SIZE: Int = 40000

  /** The default value of [[Parameters.maxPatternSize]]. */
  val MAX_PATTERN_SIZE: Int = 1500
}
