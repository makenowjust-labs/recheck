package codes.quine.labo.recheck.cli

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scala.util.Random

import io.circe.Decoder
import io.circe.HCursor

import codes.quine.labo.recheck.Config
import codes.quine.labo.recheck.cli.codecs._
import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.Context

/** InputConfig is a state-less data object of the configuration. */
final case class InputConfig(
    timeout: Duration,
    checker: Checker,
    maxAttackSize: Int,
    attackLimit: Int,
    randomSeed: Long,
    seedLimit: Int,
    incubationLimit: Int,
    crossSize: Int,
    mutateSize: Int,
    maxSeedSize: Int,
    maxGenerationSize: Int,
    maxIteration: Int,
    maxDegree: Int,
    heatRate: Double,
    usesAcceleration: Boolean,
    maxRepeatCount: Int,
    maxNFASize: Int,
    maxPatternSize: Int
) {

  /** Creates a new configuration object and cancellation function. */
  def instantiate(): (Config, () => Unit) = {
    val (context, cancel) = Context.cancellable(timeout)
    val config = Config(
      context = context,
      checker = checker,
      maxAttackSize = maxAttackSize,
      attackLimit = attackLimit,
      random = new Random(randomSeed),
      seedLimit = seedLimit,
      incubationLimit = incubationLimit,
      crossSize = crossSize,
      mutateSize = mutateSize,
      maxSeedSize = maxSeedSize,
      maxGenerationSize = maxGenerationSize,
      maxIteration = maxIteration,
      maxDegree = maxDegree,
      heatRate = heatRate,
      usesAcceleration = usesAcceleration,
      maxRepeatCount = maxRepeatCount,
      maxNFASize = maxNFASize,
      maxPatternSize = maxPatternSize
    )
    (config, cancel)
  }
}

object InputConfig {
  implicit def decode: Decoder[InputConfig] = (c: HCursor) =>
    for {
      timeout <- c.get[Option[Int]]("timeout")
      checker <- c.getOrElse[Checker]("checker")(Checker.Hybrid)
      maxAttackSize <- c.getOrElse[Int]("maxAttackSize")(Config.MaxAttackSize)
      attackLimit <- c.getOrElse[Int]("attackLimit")(Config.AttackLimit)
      randomSeed <- c.getOrElse[Long]("randomSeed")(42L)
      seedLimit <- c.getOrElse[Int]("seedLimit")(Config.SeedLimit)
      incubationLimit <- c.getOrElse[Int]("incubationLimit")(Config.IncubationLimit)
      crossSize <- c.getOrElse[Int]("crossSize")(Config.CrossSize)
      mutateSize <- c.getOrElse[Int]("mutateSize")(Config.MutateSize)
      maxSeedSize <- c.getOrElse[Int]("maxSeedSize")(Config.MaxSeedSize)
      maxGenerationSize <- c.getOrElse[Int]("maxGenerationSize")(Config.MaxGenerationSize)
      maxIteration <- c.getOrElse[Int]("maxIteration")(Config.MaxIteration)
      maxDegree <- c.getOrElse[Int]("maxDegree")(Config.MaxDegree)
      heatRate <- c.getOrElse[Double]("heatRate")(Config.HeatRate)
      usesAcceleration <- c.getOrElse("usesAcceleration")(Config.UsesAcceleration)
      maxRepeatCount <- c.getOrElse("maxRepeatCount")(Config.MaxRepeatCount)
      maxNFASize <- c.getOrElse("maxNFASize")(Config.MaxNFASize)
      maxPatternSize <- c.getOrElse("maxPatternSize")(Config.MaxPatternSize)
    } yield InputConfig(
      timeout.map(Duration(_, TimeUnit.MILLISECONDS)).getOrElse(Duration.Inf),
      checker = checker,
      maxAttackSize = maxAttackSize,
      attackLimit = attackLimit,
      randomSeed = randomSeed,
      seedLimit = seedLimit,
      incubationLimit = incubationLimit,
      crossSize = crossSize,
      mutateSize = mutateSize,
      maxSeedSize = maxSeedSize,
      maxGenerationSize = maxGenerationSize,
      maxIteration = maxIteration,
      maxDegree = maxDegree,
      heatRate = heatRate,
      usesAcceleration = usesAcceleration,
      maxRepeatCount = maxRepeatCount,
      maxNFASize = maxNFASize,
      maxPatternSize = maxPatternSize
    )
}
