package codes.quine.labs.recheck.cli
import java.time.LocalDateTime
import scala.concurrent.duration.Duration

import cats.syntax.apply._
import cats.syntax.semigroupk._
import com.monovore.decline.Command
import com.monovore.decline.Opts

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.cli.Main.BatchAction
import codes.quine.labs.recheck.cli.Main.CheckAction
import codes.quine.labs.recheck.cli.Main.command
import codes.quine.labs.recheck.cli.arguments._
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.Seeder
import codes.quine.labs.recheck.diagnostics.Diagnostics

/** Main provides the entrypoint of `recheck` command. */
object Main {

  /** Action is a subcommand of `recheck` command. */
  sealed abstract class Action extends Product with Serializable

  /** BatchAction holds `recheck batch` subcommand parameters. */
  final case class BatchAction(threadSize: Int) extends Action

  /** CheckAction holds `recheck check` subcommand parameters. */
  final case class CheckAction(pattern: InputPattern, params: Parameters) extends Action

  /** A command-line definition of `recheck`. */
  def command: Command[Action] =
    Command(name = "recheck", header = "Checks ReDoS vulnerability on the given RegExp pattern") {
      val checker = Opts
        .option[Checker](long = "checker", help = "Type of checker used for analysis.")
        .withDefault(Parameters.Checker)
      val timeout =
        Opts.option[Duration](long = "timeout", help = "Upper limit of analysis time.").withDefault(Parameters.Timeout)
      val logger = Opts
        .flag(long = "enable-log", help = "Enable logging.")
        .orFalse
        .map {
          case true =>
            Some[Context.Logger] { message =>
              val date = LocalDateTime.now()
              Console.out.println(s"[$date] $message")
            }
          case false => None
        }
      val maxAttackStringSize = Opts
        .option[Int](long = "max-attack-string-size", help = "Maximum length of an attack string.")
        .withDefault(Parameters.MaxAttackStringSize)
      val attackLimit = Opts
        .option[Int](
          long = "attack-limit",
          help = "Upper limit on the number of characters read by the VM during attack string construction."
        )
        .withDefault(Parameters.AttackLimit)
      val randomSeed = Opts
        .option[Long](long = "random-seed", help = "Seed value for PRNG used by fuzzing.")
        .withDefault(Parameters.RandomSeed)
      val maxIteration = Opts
        .option[Int](long = "max-iteration", help = "Maximum number of iterations of genetic algorithm.")
        .withDefault(Parameters.MaxIteration)
      val seeder = Opts
        .option[Seeder](
          long = "seeder",
          help = "Type of seeder used for constructing the initial generation of fuzzing."
        )
        .withDefault(Parameters.Seeder)
      val maxSimpleRepeatCount = Opts
        .option[Int](
          long = "max-simple-repeat-count",
          help = "Maximum number of sum of repeat counts for static seeder."
        )
        .withDefault(Parameters.MaxSimpleRepeatCount)
      val seedingLimit = Opts
        .option[Int](
          long = "seeding-limit",
          help = "Upper limit on the number of characters read by the VM during seeding."
        )
        .withDefault(Parameters.SeedingLimit)
      val seedingTimeout = Opts
        .option[Duration](long = "seeding-timeout", help = "Upper limit of VM execution time during seeding.")
        .withDefault(Parameters.SeedingTimeout)
      val maxInitialGenerationSize = Opts
        .option[Int](long = "max-initial-generation-size", help = "Maximum population at the initial generation.")
        .withDefault(Parameters.MaxInitialGenerationSize)
      val incubationLimit = Opts
        .option[Int](
          long = "incubation-limit",
          help = "Upper limit on the number of characters read by the VM during incubation."
        )
        .withDefault(Parameters.IncubationLimit)
      val incubationTimeout = Opts
        .option[Duration](long = "incubation-timeout", help = "Upper limit of VM execution time during incubation.")
        .withDefault(Parameters.IncubationTimeout)
      val maxGeneStringSize = Opts
        .option[Int](
          long = "max-gene-string-size",
          help = "Maximum length of an attack string on genetic algorithm iterations."
        )
        .withDefault(Parameters.MaxGeneStringSize)
      val maxGenerationSize = Opts
        .option[Int](long = "max-generation-size", help = "Maximum population at a single generation.")
        .withDefault(Parameters.MaxGenerationSize)
      val crossoverSize = Opts
        .option[Int](long = "crossover-size", help = "Number of crossovers in a single generation.")
        .withDefault(Parameters.CrossoverSize)
      val mutationSize = Opts
        .option[Int](long = "mutation-size", help = "Number of mutations in a single generation.")
        .withDefault(Parameters.MutationSize)
      val attackTimeout = Opts
        .option[Duration](
          long = "attack-timeout",
          help = "The upper limit of the VM execution time when constructing a attack string."
        )
        .withDefault(Parameters.AttackTimeout)
      val maxDegree = Opts
        .option[Int](long = "max-degree", help = "Maximum degree for constructing attack string.")
        .withDefault(Parameters.MaxDegree)
      val heatRatio = Opts
        .option[Double](
          long = "heat-ratio",
          help = "Ratio of the number of characters read to the maximum number to be considered a hotspot."
        )
        .withDefault(Parameters.HeatRatio)
      val accelerationMode = Opts
        .option[AccelerationMode](long = "acceleration-mode", help = "Mode of acceleration of VM execution.")
        .withDefault(Parameters.AccelerationMode)
      val maxRecallStringSize = Opts
        .option[Int](
          long = "max-recall-string-size",
          help = "Maximum length of an attack string on the recall validation."
        )
        .withDefault(Parameters.MaxRecallStringSize)
      val recallLimit = Opts
        .option[Int](
          long = "recall-limit",
          help = "Upper limit on the number of characters read on the recall validation."
        )
        .withDefault(Parameters.RecallLimit)
      val recallTimeout = Opts
        .option[Duration](long = "recall-timeout", help = "Upper limit of recall validation time.")
        .withDefault(Parameters.RecallTimeout)
      val maxRepeatCount = Opts
        .option[Int](long = "max-repeat-count", help = "Maximum number of sum of repeat counts.")
        .withDefault(Parameters.MaxRepeatCount)
      val maxNFASize = Opts
        .option[Int](long = "max-n-f-a-size", help = "Maximum transition size of NFA to use the automaton checker.")
        .withDefault(Parameters.MaxNFASize)
      val maxPatternSize = Opts
        .option[Int](long = "max-pattern-size", help = "Maximum pattern size to use the automaton checker.")
        .withDefault(Parameters.MaxPatternSize)
      val params = (
        (
          checker,
          timeout,
          logger,
          maxAttackStringSize,
          attackLimit,
          randomSeed
        ).tupled,
        (
          maxIteration,
          seeder,
          maxSimpleRepeatCount,
          seedingLimit,
          seedingTimeout,
          maxInitialGenerationSize,
          incubationLimit,
          incubationTimeout,
          maxGeneStringSize,
          maxGenerationSize,
          crossoverSize,
          mutationSize,
          attackTimeout,
          maxDegree,
          heatRatio,
          accelerationMode,
          maxRecallStringSize,
          recallLimit,
          recallTimeout,
          maxRepeatCount,
          maxNFASize,
          maxPatternSize
        ).tupled
      ).mapN {
        case (
              (checker, timeout, logger, maxAttackStringSize, attackLimit, randomSeed),
              (
                maxIteration,
                seeder,
                maxSimpleRepeatCount,
                seedingLimit,
                seedingTimeout,
                maxInitialGenerationSize,
                incubationLimit,
                incubationTimeout,
                maxGeneStringSize,
                maxGenerationSize,
                crossoverSize,
                mutationSize,
                attackTimeout,
                maxDegree,
                heatRatio,
                accelerationMode,
                maxRecallStringSize,
                recallLimit,
                recallTimeout,
                maxRepeatCount,
                maxNFASize,
                maxPatternSize
              )
            ) =>
          Parameters(
            checker,
            timeout,
            logger,
            maxAttackStringSize,
            attackLimit,
            randomSeed,
            maxIteration,
            seeder,
            maxSimpleRepeatCount,
            seedingLimit,
            seedingTimeout,
            maxInitialGenerationSize,
            incubationLimit,
            incubationTimeout,
            maxGeneStringSize,
            maxGenerationSize,
            crossoverSize,
            mutationSize,
            attackTimeout,
            maxDegree,
            heatRatio,
            accelerationMode,
            maxRecallStringSize,
            recallLimit,
            recallTimeout,
            maxRepeatCount,
            maxNFASize,
            maxPatternSize
          )
      }

      val pattern = Opts.argument[InputPattern](metavar = "pattern")
      val check: Opts[Action] = (pattern, params).mapN(CheckAction)

      val agent: Opts[Action] = Opts.subcommand(name = "agent", help = "Starts the batch mode.") {
        val threadSize = Opts
          .option[Int](long = "thread-size", short = "t", help = "A number of thread for processing")
          .withDefault(sys.runtime.availableProcessors())

        threadSize.map(BatchAction.apply)
      }

      agent <+> check
    }

  // $COVERAGE-OFF$
  /** An entrypoint of `recheck` command. */
  def main(args: Array[String]): Unit = new Main().run(args)
  // $COVERAGE-ON$
}

class Main {
  // $COVERAGE-OFF$
  def exit(exitCode: Int): Unit = sys.exit(exitCode)
  // $COVERAGE-ON$

  def run(args: Array[String]): Unit = command.parse(args.toSeq, sys.env) match {
    case Left(help) =>
      Console.err.println(help.toString)
      exit(2)

    case Right(action: CheckAction) =>
      val diagnostics = ReDoS.check(action.pattern.source, action.pattern.flags, action.params)
      Console.out.println(diagnostics)
      diagnostics match {
        case _: Diagnostics.Safe                                => () // skip
        case _: Diagnostics.Vulnerable | _: Diagnostics.Unknown => exit(1)
      }

    case Right(action: BatchAction) =>
      new AgentCommand(action.threadSize).run()
  }
}