package codes.quine.labs.recheck.cli
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime

import cats.syntax.apply.*
import cats.syntax.semigroupk.*
import com.monovore.decline.Command
import com.monovore.decline.Opts

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.cli.Main.BatchAction
import codes.quine.labs.recheck.cli.Main.CheckAction
import codes.quine.labs.recheck.cli.Main.command
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.Diagnostics

/** Main provides the entrypoint of `recheck` command. */
object Main:

  /** Action is a subcommand of `recheck` command. */
  sealed abstract class Action extends Product with Serializable

  /** BatchAction holds `recheck batch` subcommand parameters. */
  final case class BatchAction(threadSize: Int) extends Action

  /** CheckAction holds `recheck check` subcommand parameters.
    *
    * @param all
    *   whether it shows all the diagnostics of `ReDoS.checkAll`
    */
  final case class CheckAction(pattern: InputPattern, params: Parameters, all: Boolean = false) extends Action

  /** A command-line definition of `recheck`. */
  def command: Command[Action] =
    Command(name = "recheck", header = "Checks ReDoS vulnerability on the given RegExp pattern"):
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
      val params = ParametersOpts(logger)

      val pattern = Opts.argument[InputPattern](metavar = "pattern")
      val all = Opts
        .flag(
          long = "all",
          help = "Show all the diagnostics, including the vulnerable ones whose recall validation failed."
        )
        .orFalse
      val check: Opts[Action] = (pattern, params, all).mapN(CheckAction(_, _, _))

      val agent: Opts[Action] = Opts.subcommand(name = "agent", help = "Starts the batch mode."):
        val threadSize = Opts
          .option[Int](long = "thread-size", short = "t", help = "A number of thread for processing")
          .withDefault(sys.runtime.availableProcessors())

        threadSize.map(BatchAction.apply)

      agent <+> check

  // $COVERAGE-OFF$
  /** An entrypoint of `recheck` command. */
  def main(args: Array[String]): Unit =
    // It reads the standard input as UTF-8 to be independent of the platform's default charset (#1910).
    // For example, a native image on Windows uses the ANSI code page as the default charset.
    Console.withIn(new InputStreamReader(System.in, StandardCharsets.UTF_8)):
      new Main().run(args)
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
      val diagnostics =
        if action.all then ReDoS.checkAll(action.pattern.source, action.pattern.flags, action.params)
        else List(ReDoS.check(action.pattern.source, action.pattern.flags, action.params))
      Console.out.println(diagnostics.mkString("\n\n"))
      diagnostics.last match
        case _: Diagnostics.Safe                                => () // skip
        case _: Diagnostics.Vulnerable | _: Diagnostics.Unknown => exit(1)

    case Right(action: BatchAction) =>
      new AgentCommand(action.threadSize).run()
