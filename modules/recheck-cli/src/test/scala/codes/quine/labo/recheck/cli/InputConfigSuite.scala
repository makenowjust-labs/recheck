package codes.quine.labo.recheck.cli

import scala.concurrent.duration._

import io.circe.Json
import io.circe.syntax._

import codes.quine.labo.recheck.Config
import codes.quine.labo.recheck.common.Checker

class InputConfigSuite extends munit.FunSuite {
  test("InputConfig#instantiate") {
    val (result, cancel) = InputConfig(
      timeout = 10.seconds,
      checker = Checker.Automaton,
      maxAttackSize = 12345,
      attackLimit = 12345,
      randomSeed = 12345L,
      seedLimit = 12345,
      incubationLimit = 12345,
      crossSize = 12345,
      mutateSize = 12345,
      maxSeedSize = 12345,
      maxGenerationSize = 12345,
      maxIteration = 12345,
      maxDegree = 12345,
      heatRate = 0.123,
      usesAcceleration = false,
      maxRepeatCount = 12345,
      maxNFASize = 12345,
      maxPatternSize = 1234
    ).instantiate()
    val expected = Config(
      context = result.context,
      checker = Checker.Automaton,
      maxAttackSize = 12345,
      attackLimit = 12345,
      random = result.random,
      seedLimit = 12345,
      incubationLimit = 12345,
      crossSize = 12345,
      mutateSize = 12345,
      maxSeedSize = 12345,
      maxGenerationSize = 12345,
      maxIteration = 12345,
      maxDegree = 12345,
      heatRate = 0.123,
      usesAcceleration = false,
      maxRepeatCount = 12345,
      maxNFASize = 12345,
      maxPatternSize = 1234
    )
    assertEquals(result, expected)

    assertEquals(result.context.isInterrupted(), false)
    cancel()
    assertEquals(result.context.isInterrupted(), true)
  }

  test("InputConfig.decode") {
    assertEquals(
      InputConfig.decode.decodeJson(Json.obj()),
      Right(
        InputConfig(
          timeout = Duration.Inf,
          checker = Checker.Hybrid,
          maxAttackSize = Config.MaxAttackSize,
          attackLimit = Config.AttackLimit,
          randomSeed = 42L,
          seedLimit = Config.SeedLimit,
          incubationLimit = Config.IncubationLimit,
          crossSize = Config.CrossSize,
          mutateSize = Config.MutateSize,
          maxSeedSize = Config.MaxSeedSize,
          maxGenerationSize = Config.MaxGenerationSize,
          maxIteration = Config.MaxIteration,
          maxDegree = Config.MaxDegree,
          heatRate = Config.HeatRate,
          usesAcceleration = Config.UsesAcceleration,
          maxRepeatCount = Config.MaxRepeatCount,
          maxNFASize = Config.MaxNFASize,
          maxPatternSize = Config.MaxPatternSize
        )
      )
    )
    assertEquals(
      InputConfig.decode.decodeJson(
        Json.obj(
          "timeout" := 10000,
          "checker" := "fuzz",
          "maxAttackSize" := 12345,
          "attackLimit" := 12345,
          "randomSeed" := 12345,
          "seedLimit" := 12345,
          "incubationLimit" := 12345,
          "crossSize" := 12345,
          "mutateSize" := 12345,
          "maxSeedSize" := 12345,
          "maxGenerationSize" := 12345,
          "maxIteration" := 12345,
          "maxDegree" := 12345,
          "heatRate" := 0.123,
          "usesAcceleration" := false,
          "maxRepeatCount" := 12345,
          "maxNFASize" := 12345,
          "maxPatternSize" := 12345
        )
      ),
      Right(
        InputConfig(
          timeout = 10000.millis,
          checker = Checker.Fuzz,
          maxAttackSize = 12345,
          attackLimit = 12345,
          randomSeed = 12345L,
          seedLimit = 12345,
          incubationLimit = 12345,
          crossSize = 12345,
          mutateSize = 12345,
          maxSeedSize = 12345,
          maxGenerationSize = 12345,
          maxIteration = 12345,
          maxDegree = 12345,
          heatRate = 0.123,
          usesAcceleration = false,
          maxRepeatCount = 12345,
          maxNFASize = 12345,
          maxPatternSize = 12345
        )
      )
    )
  }
}
