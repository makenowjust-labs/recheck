package codes.quine.labo.recheck.cli
import scala.concurrent.duration.Duration

import cats.syntax.apply._
import cats.syntax.semigroupk._
import com.monovore.decline.Command
import com.monovore.decline.Opts

import codes.quine.labo.recheck.ReDoS
import codes.quine.labo.recheck.cli.Main.BatchAction
import codes.quine.labo.recheck.cli.Main.CheckAction
import codes.quine.labo.recheck.cli.Main.command
import codes.quine.labo.recheck.cli.arguments._
import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.Parameters
import codes.quine.labo.recheck.diagnostics.Diagnostics

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
        .option[Checker](long = "checker", short = "c", help = "Type of checker used for analysis.")
        .withDefault(Parameters.CHECKER)
      val timeout =
        Opts
          .option[Duration](long = "timeout", short = "t", help = "Upper limit of analysis time.")
          .withDefault(Parameters.TIMEOUT)
      val maxAttackStringSize = Opts
        .option[Int](long = "max-attack-string-size", help = "Maximum length of an attack string.")
        .withDefault(Parameters.MAX_ATTACK_STRING_SIZE)
      val attackLimit = Opts
        .option[Int](
          long = "attack-limit",
          help = "Upper limit on the number of characters read by the VM during attack string construction."
        )
        .withDefault(Parameters.ATTACK_LIMIT)
      val randomSeed = Opts
        .option[Long](long = "random-seed", help = "Seed value for PRNG used by fuzzing.")
        .withDefault(Parameters.RANDOM_SEED)
      val maxIteration = Opts
        .option[Int](long = "max-iteration", help = "Maximum number of iterations of genetic algorithm.")
        .withDefault(Parameters.MAX_ITERATION)
      val seedingLimit = Opts
        .option[Int](
          long = "seeding-limit",
          help = "Upper limit on the number of characters read by the VM during seeding."
        )
        .withDefault(Parameters.SEEDING_LIMIT)
      val seedingTimeout = Opts
        .option[Duration](long = "seeding-timeout", help = "Upper limit of VM execution time during seeding.")
        .withDefault(Parameters.SEEDING_TIMEOUT)
      val maxInitialGenerationSize = Opts
        .option[Int](long = "max-initial-generation-size", help = "Maximum population at the initial generation.")
        .withDefault(Parameters.MAX_INITIAL_GENERATION_SIZE)
      val incubationLimit = Opts
        .option[Int](
          long = "incubation-limit",
          help = "Upper limit on the number of characters read by the VM during incubation."
        )
        .withDefault(Parameters.INCUBATION_LIMIT)
      val incubationTimeout = Opts
        .option[Duration](long = "incubation-timeout", help = "Upper limit of VM execution time during incubation.")
        .withDefault(Parameters.INCUBATION_TIMEOUT)
      val maxGeneStringSize = Opts
        .option[Int](
          long = "max-gene-string-size",
          help = "Maximum length of an attack string on genetic algorithm iterations."
        )
        .withDefault(Parameters.MAX_GENE_STRING_SIZE)
      val maxGenerationSize = Opts
        .option[Int](long = "max-generation-size", help = "Maximum population at a single generation.")
        .withDefault(Parameters.MAX_GENERATION_SIZE)
      val crossoverSize = Opts
        .option[Int](long = "crossover-size", help = "Number of crossovers in a single generation.")
        .withDefault(Parameters.CROSSOVER_SIZE)
      val mutationSize = Opts
        .option[Int](long = "mutation-size", help = "Number of mutations in a single generation.")
        .withDefault(Parameters.MUTATION_SIZE)
      val attackTimeout = Opts
        .option[Duration](
          long = "attack-timeout",
          help = "The upper limit of the VM execution time when constructing a attack string."
        )
        .withDefault(Parameters.ATTACK_TIMEOUT)
      val maxDegree = Opts
        .option[Int](long = "max-degree", help = "Maximum degree for constructing attack string.")
        .withDefault(Parameters.MAX_DEGREE)
      val heatRatio = Opts
        .option[Double](
          long = "heat-ratio",
          help = "Ratio of the number of characters read to the maximum number to be considered a hotspot."
        )
        .withDefault(Parameters.HEAT_RATIO)
      val noUsesAcceleration = Opts
        .flag(long = "no-uses-acceleration", help = "Whether to use acceleration for VM execution.")
        .orTrue
      val maxRepeatCount = Opts
        .option[Int](long = "max-repeat-count", help = "Maximum number of sum of repeat counts.")
        .withDefault(Parameters.MAX_REPEAT_COUNT)
      val maxNFASize = Opts
        .option[Int](long = "max-n-f-a-size", help = "Maximum transition size of NFA to use the automaton checker.")
        .withDefault(Parameters.MAX_N_F_A_SIZE)
      val maxPatternSize = Opts
        .option[Int](long = "max-pattern-size", help = "Maximum pattern size to use the automaton checker.")
        .withDefault(Parameters.MAX_PATTERN_SIZE)

      val params = (
        checker,
        timeout,
        maxAttackStringSize,
        attackLimit,
        randomSeed,
        maxIteration,
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
        noUsesAcceleration,
        maxRepeatCount,
        maxNFASize,
        maxPatternSize
      ).mapN(Parameters.apply)

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
