package codes.quine.labs.recheck.cli
import java.io.IOException
import java.io.InputStreamReader
import java.io.StringWriter
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.Paths
import java.time.LocalDateTime

import cats.data.Validated
import cats.syntax.apply.*
import cats.syntax.semigroupk.*
import com.monovore.decline.Command
import com.monovore.decline.Opts
import io.circe.syntax.*

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.cli.Main.BatchAction
import codes.quine.labs.recheck.cli.Main.CheckAction
import codes.quine.labs.recheck.cli.Main.GenerateRuntimeAttackScriptAction
import codes.quine.labs.recheck.cli.Main.RepairAction
import codes.quine.labs.recheck.cli.Main.command
import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.repair.Keep
import codes.quine.labs.recheck.repair.Rx

/** Main provides the entrypoint of `recheck` command. */
object Main:

  /** Action is a subcommand of `recheck` command. */
  sealed abstract class Action extends Product with Serializable

  /** BatchAction holds `recheck batch` subcommand parameters. */
  final case class BatchAction(threadSize: Int) extends Action

  /** CheckAction holds `recheck check` subcommand parameters. */
  final case class CheckAction(pattern: InputPattern, params: Parameters, json: Boolean) extends Action

  /** RepairAction holds `recheck repair` subcommand parameters. */
  final case class RepairAction(pattern: InputPattern, params: Parameters, keep: Keep, json: Boolean) extends Action

  /** GenerateRuntimeAttackScriptAction holds `recheck generate-runtime-attack-script` subcommand parameters.
    *
    * `diagnostics` is the path of the diagnostics JSON, or `-` for the standard input.
    */
  final case class GenerateRuntimeAttackScriptAction(diagnostics: String) extends Action

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
      val params = ParametersOption(logger)

      val pattern = Opts.argument[InputPattern](metavar = "pattern")
      val json = Opts.flag(long = "json", help = "Show the diagnostics in JSON.").orFalse
      val check: Opts[Action] = (pattern, params, json).mapN(CheckAction(_, _, _))

      val agent: Opts[Action] = Opts.subcommand(name = "agent", help = "Starts the batch mode."):
        val threadSize = Opts
          .option[Int](long = "thread-size", short = "t", help = "A number of thread for processing")
          .withDefault(sys.runtime.availableProcessors())

        threadSize.map(BatchAction.apply)

      val generateRuntimeAttackScript: Opts[Action] = Opts.subcommand(
        name = "generate-runtime-attack-script",
        help = "Generates the runtime-attack program of vulnerable diagnostics in JSON as a standalone script."
      ):
        Opts.argument[String](metavar = "diagnostics").map(GenerateRuntimeAttackScriptAction.apply)

      val repair: Opts[Action] =
        Opts.subcommand(name = "repair", help = "Repairs the given RegExp pattern to a better one."):
          val keep = Opts
            .option[String](
              long = "keep",
              help =
                "What the repair keeps of the matches: all (default), named, match-range, match, or capture numbers " +
                  "and names separated by commas."
            )
            .mapValidated:
              case "all"         => Validated.validNel(Keep.All)
              case "named"       => Validated.validNel(Keep.Named)
              case "match-range" => Validated.validNel(Keep.MatchRange)
              case "match"       => Validated.validNel(Keep.Match)
              case refs          =>
                Validated.validNel(
                  Keep.Captures(refs.split(",").toSeq.map(r => r.trim.toIntOption.getOrElse[Int | String](r.trim)))
                )
            .withDefault(Keep.All)
          val json = Opts.flag(long = "json", help = "Show the result in JSON.").orFalse
          (pattern, params, keep, json).mapN(RepairAction(_, _, _, _))

      agent <+> generateRuntimeAttackScript <+> repair <+> check

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
      val diagnostics = ReDoS.check(action.pattern.source, action.pattern.flags, action.params)
      Console.out.println(if action.json then diagnostics.asJson.noSpaces else diagnostics.toString)
      diagnostics match
        case _: Diagnostics.Safe                                => () // skip
        case _: Diagnostics.Vulnerable | _: Diagnostics.Unknown => exit(1)

    case Right(action: RepairAction) =>
      val rx = ReDoS.repair(action.pattern.source, action.pattern.flags, action.params, action.keep)
      Console.out.println(if action.json then rx.asJson.noSpaces else rx.toString)
      rx match
        case _: Rx.Safe | _: Rx.Repaired => () // skip
        case _: Rx.Unknown               => exit(1)

    case Right(action: GenerateRuntimeAttackScriptAction) =>
      val script = for
        json <- read(action.diagnostics)
        script <- GenerateRuntimeAttackScriptCommand.generate(json)
      yield script
      script match
        case Right(script) =>
          val bytes = script.content.getBytes(StandardCharsets.UTF_8)
          Console.out.write(bytes, 0, bytes.length)
          Console.out.flush()
        case Left(message) =>
          Console.err.println(message)
          exit(1)

    case Right(action: BatchAction) =>
      new AgentCommand(action.threadSize).run()

  /** Reads the file of the path as UTF-8, or the standard input for `-`. */
  private def read(path: String): Either[String, String] =
    try
      if path == "-" then
        val in = new StringWriter
        Console.in.transferTo(in)
        Right(in.toString)
      else Right(Files.readString(Paths.get(path), StandardCharsets.UTF_8))
    catch
      case _: NoSuchFileException => Left(s"No such file: $path")
      case ex: IOException        => Left(s"Cannot read $path: ${ex.getMessage}")
