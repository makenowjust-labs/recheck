package codes.quine.labo.recheck

import scala.util.Random

import common.Checker
import common.Context

/** Config is a ReDoS checker configuration. */
final case class Config(
    // General:
    // An execution context in this analysis.
    context: Context = Context(),
    // A checker to use.
    checker: Checker = Checker.Hybrid,
    // A maximum size of an attack string.
    maxAttackSize: Int = Config.MaxAttackSize,
    // A limit of VM execution steps on attack string construction.
    attackLimit: Int = Config.AttackLimit,
    // Fuzz:
    // A random instance for fuzzing.
    random: Random = Random,
    // A limit of VM execution steps on seeding.
    seedLimit: Int = Config.SeedLimit,
    // A limit of VM execution steps on population.
    populationLimit: Int = Config.PopulationLimit,
    // A size to compute crossing.
    crossSize: Int = Config.CrossSize,
    // A size to compute mutation.
    mutateSize: Int = Config.MutateSize,
    // A maximum size of a initial seed set.
    maxSeedSize: Int = Config.MaxSeedSize,
    // A maximum size of a population on a generation.
    maxGenerationSize: Int = Config.MaxGenerationSize,
    // A maximum iteration number of GA.
    maxIteration: Int = Config.MaxIteration,
    // A maximum degree number to attempt on building attack string.
    maxDegree: Int = Config.MaxDegree,
    // Hybrid:
    // A maximum number of sum of repeat counts like `/a{10}/`.
    // If this value is exceeded, it switches to use fuzzing based checker.
    maxRepeatCount: Int = Config.MaxRepeatCount,
    // A maximum transition size of NFA to use the automaton based checker.
    // If this value is exceeded, it switches to use fuzzing based checker.
    maxNFASize: Int = Config.MaxNFASize,
    // A maximum pattern size to use the automaton based checker.
    // If this value is exceeded, it switches to use fuzzing based checker.
    maxPatternSize: Int = Config.MaxPatternSize
) {

  /** Provides a timeout instance as an implicit parameter. */
  implicit def ctx: Context = context
}

object Config {

  /** The default value of [[Config#maxAttackSize]]. */
  val MaxAttackSize = 4_000

  /** The default value of [[Config#AttackLimit]]. */
  val AttackLimit = 100_000

  /** The default value of [[Config#seedLimit]]. */
  val SeedLimit = 1_000

  /** The default value of [[Config#populationLimit]]. */
  val PopulationLimit = 10_000

  /** The default value of [[Config#crossSize]]. */
  val CrossSize = 25

  /** The default value of [[Config#mutateSize]]. */
  val MutateSize = 50

  /** The default value of [[Config#maxSeedSize]]. */
  val MaxSeedSize = 50

  /** The default value of [[Config#maxGenerationSize]]. */
  val MaxGenerationSize = 100

  /** The default value of [[Config#maxIteration]]. */
  val MaxIteration = 30

  /** The default value of [[Config#maxDegree]]. */
  val MaxDegree = 4

  /** The default value of [[Config#maxRepeatCount]]. */
  val MaxRepeatCount = 20

  /** The default value of [[Config#maxNFASize]]. */
  val MaxNFASize = 40000

  /** The default value of [[Config#maxPatternSize]]. */
  val MaxPatternSize = 1500
}
