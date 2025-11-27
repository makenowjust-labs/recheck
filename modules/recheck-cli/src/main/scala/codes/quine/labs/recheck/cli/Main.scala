package codes.quine.labs.recheck.cli
import java.time.LocalDateTime
import scala.concurrent.duration.Duration

import cats.syntax.apply.*
import cats.syntax.semigroupk.*
import com.monovore.decline.Command
import com.monovore.decline.Opts

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.cli.Main.BatchAction
import codes.quine.labs.recheck.cli.Main.CheckAction
import codes.quine.labs.recheck.cli.Main.command
import codes.quine.labs.recheck.cli.arguments.given
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.Seeder
import codes.quine.labs.recheck.diagnostics.Diagnostics

/** Main provides the entrypoint of `recheck` command. */
object Main:

  /** Action is a subcommand of `recheck` command. */
  sealed abstract class Action extends Product with Serializable

  /** BatchAction holds `recheck batch` subcommand parameters. */
  final case class BatchAction(threadSize: Int) extends Action

  /** CheckAction holds `recheck check` subcommand parameters. */
  final case class CheckAction(pattern: InputPattern, params: Parameters) extends Action

  /** A command-line definition of `recheck`. */
  def command: Command[Action] =
    Command(name = "recheck", header = "Checks ReDoS vulnerability on the given RegExp pattern"):
      val accelerationMode = Opts
        .option[AccelerationMode](
          long = "acceleration-mode",
          help = "The type of acceleration mode strategy on fuzzing."
        )
        .withDefault(Parameters.DefaultAccelerationMode)
      val attackLimit = Opts
        .option[Int](
          long = "attack-limit",
          help = "The upper limit on the number of characters read by VM on the attack."
        )
        .withDefault(Parameters.DefaultAttackLimit)
      val attackTimeout = Opts
        .option[Duration](
          long = "attack-timeout",
          help = "The upper limit of matching time on the attack."
        )
        .withDefault(Parameters.DefaultAttackTimeout)
      val checker = Opts
        .option[Checker](
          long = "checker",
          help = "The type of checker to be used."
        )
        .withDefault(Parameters.DefaultChecker)
      val crossoverSize = Opts
        .option[Int](
          long = "crossover-size",
          help = "The number of crossover on each generation."
        )
        .withDefault(Parameters.DefaultCrossoverSize)
      val heatRatio = Opts
        .option[Double](
          long = "heat-ratio",
          help = "The ratio of the number of characters read to the maximum number to be considered as a hot spot."
        )
        .withDefault(Parameters.DefaultHeatRatio)
      val incubationLimit = Opts
        .option[Int](
          long = "incubation-limit",
          help = "The upper limit on the number of characters read by VM on incubation."
        )
        .withDefault(Parameters.DefaultIncubationLimit)
      val incubationTimeout = Opts
        .option[Duration](
          long = "incubation-timeout",
          help = "The upper limit of matching time on incubation."
        )
        .withDefault(Parameters.DefaultIncubationTimeout)
      val logger = Opts
        .flag(long = "enable-log", help = "Enable logging.")
        .orFalse
        .map:
          case true =>
            Some[Context.Logger] { message =>
              val date = LocalDateTime.now()
              Console.out.println(s"[$date] $message")
            }
          case false => None
      val maxAttackStringSize = Opts
        .option[Int](
          long = "max-attack-string-size",
          help = "The maximum length of the attack string on fuzzing."
        )
        .withDefault(Parameters.DefaultMaxAttackStringSize)
      val maxDegree = Opts
        .option[Int](
          long = "max-degree",
          help = "The maximum degree to be considered in fuzzing."
        )
        .withDefault(Parameters.DefaultMaxDegree)
      val maxGeneStringSize = Opts
        .option[Int](
          long = "max-gene-string-size",
          help = "The maximum length of the gene string on fuzzing."
        )
        .withDefault(Parameters.DefaultMaxGeneStringSize)
      val maxGenerationSize = Opts
        .option[Int](
          long = "max-generation-size",
          help = "The maximum size of each generation on fuzzing."
        )
        .withDefault(Parameters.DefaultMaxGenerationSize)
      val maxInitialGenerationSize = Opts
        .option[Int](
          long = "max-initial-generation-size",
          help = "The maximum size of the initial generation on fuzzing."
        )
        .withDefault(Parameters.DefaultMaxInitialGenerationSize)
      val maxIteration = Opts
        .option[Int](
          long = "max-iteration",
          help = "The maximum number of fuzzing iteration."
        )
        .withDefault(Parameters.DefaultMaxIteration)
      val maxNFASize = Opts
        .option[Int](
          long = "max-nfa-size",
          help = "The maximum size of NFA to determine which algorithm is used."
        )
        .withDefault(Parameters.DefaultMaxNFASize)
      val maxPatternSize = Opts
        .option[Int](
          long = "max-pattern-size",
          help = "The maximum size of the regular expression pattern to determine which algorithm is used."
        )
        .withDefault(Parameters.DefaultMaxPatternSize)
      val maxRecallStringSize = Opts
        .option[Int](
          long = "max-recall-string-size",
          help = "The maximum length of the attack string on recall validation."
        )
        .withDefault(Parameters.DefaultMaxRecallStringSize)
      val maxRepeatCount = Opts
        .option[Int](
          long = "max-repeat-count",
          help =
            "The maximum number of sum of repetition quantifier’s repeat counts to determine which algorithm is used."
        )
        .withDefault(Parameters.DefaultMaxRepeatCount)
      val maxSimpleRepeatCount = Opts
        .option[Int](
          long = "max-simple-repeat-count",
          help = "The maximum number of each repetition quantifier’s repeat count on `static` seeding."
        )
        .withDefault(Parameters.DefaultMaxSimpleRepeatCount)
      val mutationSize = Opts
        .option[Int](
          long = "mutation-size",
          help = "The number of mutation on each generation."
        )
        .withDefault(Parameters.DefaultMutationSize)
      val randomSeed = Opts
        .option[Long](
          long = "random-seed",
          help = "The PRNG seed number."
        )
        .withDefault(Parameters.DefaultRandomSeed)
      val recallLimit = Opts
        .option[Int](
          long = "recall-limit",
          help = "The upper limit on the number of characters read by VM on the recall validation."
        )
        .withDefault(Parameters.DefaultRecallLimit)
      val recallTimeout = Opts
        .option[Duration](
          long = "recall-timeout",
          help = "The upper limit of matching time on the recall validation."
        )
        .withDefault(Parameters.DefaultRecallTimeout)
      val seeder = Opts
        .option[Seeder](
          long = "seeder",
          help = "The type of seeder to be used in fuzzing."
        )
        .withDefault(Parameters.DefaultSeeder)
      val seedingLimit = Opts
        .option[Int](
          long = "seeding-limit",
          help = "The upper limit on the number of characters read by VM on `dynamic` seeding."
        )
        .withDefault(Parameters.DefaultSeedingLimit)
      val seedingTimeout = Opts
        .option[Duration](
          long = "seeding-timeout",
          help = "The upper limit of matching time on `dynamic` seeding."
        )
        .withDefault(Parameters.DefaultSeedingTimeout)
      val timeout = Opts
        .option[Duration](
          long = "timeout",
          help = "The upper limit of checking time."
        )
        .withDefault(Parameters.DefaultTimeout)

      val params = (
        (
          accelerationMode,
          attackLimit,
          attackTimeout,
          checker,
          crossoverSize,
          heatRatio,
          incubationLimit,
          incubationTimeout,
          logger,
          maxAttackStringSize
        ).tupled,
        (
          maxDegree,
          maxGeneStringSize,
          maxGenerationSize,
          maxInitialGenerationSize,
          maxIteration,
          maxNFASize,
          maxPatternSize,
          maxRecallStringSize,
          maxRepeatCount,
          maxSimpleRepeatCount
        ).tupled,
        (mutationSize, randomSeed, recallLimit, recallTimeout, seeder, seedingLimit, seedingTimeout, timeout).tupled
      ).mapN {
        case (
              (
                accelerationMode,
                attackLimit,
                attackTimeout,
                checker,
                crossoverSize,
                heatRatio,
                incubationLimit,
                incubationTimeout,
                logger,
                maxAttackStringSize
              ),
              (
                maxDegree,
                maxGeneStringSize,
                maxGenerationSize,
                maxInitialGenerationSize,
                maxIteration,
                maxNFASize,
                maxPatternSize,
                maxRecallStringSize,
                maxRepeatCount,
                maxSimpleRepeatCount
              ),
              (mutationSize, randomSeed, recallLimit, recallTimeout, seeder, seedingLimit, seedingTimeout, timeout)
            ) =>
          Parameters(
            accelerationMode,
            attackLimit,
            attackTimeout,
            checker,
            crossoverSize,
            heatRatio,
            incubationLimit,
            incubationTimeout,
            logger,
            maxAttackStringSize,
            maxDegree,
            maxGeneStringSize,
            maxGenerationSize,
            maxInitialGenerationSize,
            maxIteration,
            maxNFASize,
            maxPatternSize,
            maxRecallStringSize,
            maxRepeatCount,
            maxSimpleRepeatCount,
            mutationSize,
            randomSeed,
            recallLimit,
            recallTimeout,
            seeder,
            seedingLimit,
            seedingTimeout,
            timeout
          )
      }

      val pattern = Opts.argument[InputPattern](metavar = "pattern")
      val check: Opts[Action] = (pattern, params).mapN(CheckAction(_, _))

      val agent: Opts[Action] = Opts.subcommand(name = "agent", help = "Starts the batch mode."):
        val threadSize = Opts
          .option[Int](long = "thread-size", short = "t", help = "A number of thread for processing")
          .withDefault(sys.runtime.availableProcessors())

        threadSize.map(BatchAction.apply)

      agent <+> check

  // $COVERAGE-OFF$
  /** An entrypoint of `recheck` command. */
  def main(args: Array[String]): Unit = new Main().run(args)
  // $COVERAGE-ON$

class Main:

  // $COVERAGE-OFF$
  def exit(exitCode: Int): Unit = sys.exit(exitCode)
  // $COVERAGE-ON$

  def run(args: Array[String]): Unit = command.parse(args.toSeq, sys.env) match
    case Left(help) =>
      Console.err.println(help.toString)
      exit(2)

    case Right(action: CheckAction) =>
      val diagnostics = ReDoS.check(action.pattern.source, action.pattern.flags, action.params)
      Console.out.println(diagnostics)
      diagnostics match
        case _: Diagnostics.Safe                                => () // skip
        case _: Diagnostics.Vulnerable | _: Diagnostics.Unknown => exit(1)

    case Right(action: BatchAction) =>
      new AgentCommand(action.threadSize).run()
