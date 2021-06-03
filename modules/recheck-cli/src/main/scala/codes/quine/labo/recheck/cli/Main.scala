package codes.quine.labo.recheck.cli

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

import cats.syntax.apply._
import cats.syntax.semigroupk._
import com.monovore.decline.Command
import com.monovore.decline.Opts

import codes.quine.labo.recheck.Config
import codes.quine.labo.recheck.ReDoS
import codes.quine.labo.recheck.cli.arguments._
import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.diagnostics.Diagnostics

object Main {
  sealed abstract class Action extends Product with Serializable

  final case class BatchAction(threadSize: Int) extends Action
  final case class CheckAction(pattern: InputPattern, config: InputConfig) extends Action

  val command: Command[Action] =
    Command(name = "recheck", header = "A command-line utility of Recheck") {
      val timeout = Opts
        .option[Duration](
          long = "timeout",
          short = "t",
          help = "A timeout duration in a checking."
        )
        .withDefault(Duration(10, TimeUnit.SECONDS))

      val checker = Opts
        .option[Checker](
          long = "checker",
          short = "c",
          help = "A checker name to be used (one of 'hybrid', 'automaton', or 'fuzz'.)"
        )
        .withDefault(Checker.Hybrid)
      val maxAttackSize = Opts
        .option[Int](long = "max-attack-size", help = "A maximum length of an attack string.")
        .withDefault(Config.MaxAttackSize)
      val attackLimit = Opts
        .option[Int](long = "attack-limit", help = "A limit of VM execution steps.")
        .withDefault(Config.AttackLimit)
      val randomSeed = Opts
        .option[Long](long = "random-seed", help = "A PRNG seed.")
        .withDefault(42L)
      val seedLimit = Opts
        .option[Int](long = "seed-limit", help = "A limit of VM execution steps on the seeding phase.")
        .withDefault(Config.SeedLimit)
      val incubationLimit = Opts
        .option[Int](long = "incubation-limit", help = "A limit of VM execution steps on the incubation phase.")
        .withDefault(Config.IncubationLimit)
      val crossSize = Opts
        .option[Int](long = "cross-size", help = "The number of crossings on one generation.")
        .withDefault(Config.CrossSize)
      val mutateSize = Opts
        .option[Int](long = "mutate-size", help = "The number of mutations on one generation.")
        .withDefault(Config.MutateSize)
      val maxSeedSize = Opts
        .option[Int](long = "max-seed-size", help = "A maximum size of a seed set.")
        .withDefault(Config.MaxSeedSize)
      val maxGenerationSize = Opts
        .option[Int](long = "max-generation-size", help = "A maximum size of a living population on one generation.")
        .withDefault(Config.MaxGenerationSize)
      val maxIteration = Opts
        .option[Int](long = "max-iteration", help = "The number of iterations on the incubation phase.")
        .withDefault(Config.MaxIteration)
      val maxDegree = Opts
        .option[Int](long = "max-degree", help = "A maximum degree to attempt on building an attack string.")
        .withDefault(Config.MaxDegree)
      val heatRate = Opts
        .option[Double](long = "heat-rate", help = "A rate of a hotspot steps by the maximum steps.")
        .withDefault(Config.HeatRate)
      val noAcceleration = Opts
        .flag(long = "no-acceleration", help = "Don't use acceleration.")
        .orTrue
      val maxRepeatCount = Opts
        .option[Int](long = "max-repeat-count", help = "A limit of repetition count in the RegExp.")
        .withDefault(Config.MaxRepeatCount)
      val maxNFASize = Opts
        .option[Int](long = "max-nfa-size", help = "A maximum size of the transition function of NFA.")
        .withDefault(Config.MaxNFASize)
      val maxPatternSize = Opts
        .option[Int](long = "max-pattern-size", help = "A maximum size of the pattern.")
        .withDefault(Config.MaxPatternSize)

      val config = (
        timeout,
        checker,
        maxAttackSize,
        attackLimit,
        randomSeed,
        seedLimit,
        incubationLimit,
        crossSize,
        mutateSize,
        maxSeedSize,
        maxGenerationSize,
        maxIteration,
        maxDegree,
        heatRate,
        noAcceleration,
        maxRepeatCount,
        maxNFASize,
        maxPatternSize
      ).mapN(InputConfig.apply)

      val pattern = Opts.argument[InputPattern](metavar = "pattern")
      val check: Opts[Action] = (pattern, config).mapN(CheckAction)

      val batch: Opts[Action] = Opts.subcommand(name = "batch", help = "Starts the batch mode.") {
        val threadSize = Opts
          .option[Int](long = "thread-size", short = "t", help = "A number of thread for processing")
          .withDefault(Runtime.getRuntime().availableProcessors())

        threadSize.map(BatchAction.apply)
      }

      batch <+> check
    }

  def main(args: Array[String]): Unit = command.parse(args.toSeq, sys.env) match {
    case Left(help) =>
      System.err.println(help.toString)
      sys.exit(2)

    case Right(action: CheckAction) =>
      val (config, _) = action.config.instantiate()
      val diagnostics = ReDoS.check(action.pattern.source, action.pattern.flags, config)
      println(diagnostics)
      diagnostics match {
        case _: Diagnostics.Safe                                => // skip
        case _: Diagnostics.Vulnerable | _: Diagnostics.Unknown => sys.exit(1)
      }

    case Right(action: BatchAction) =>
      new BatchCommand(action.threadSize).run()
  }
}
