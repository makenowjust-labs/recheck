package codes.quine.labs.recheck.cli
import java.io.IOException
import java.io.InputStreamReader
import java.io.StringWriter
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.Paths
import java.time.LocalDateTime
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.SECONDS

import cats.data.Validated
import cats.syntax.apply.*
import cats.syntax.semigroupk.*
import com.monovore.decline.Command
import com.monovore.decline.Opts
import io.circe.syntax.*

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.cli.Main.BatchAction
import codes.quine.labs.recheck.cli.Main.CalibrateRuntimeAttackAction
import codes.quine.labs.recheck.cli.Main.CheckAction
import codes.quine.labs.recheck.cli.Main.GenerateRuntimeAttackScriptAction
import codes.quine.labs.recheck.cli.Main.command
import codes.quine.labs.recheck.cli.arguments.given
import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

/** Main provides the entrypoint of `recheck` command. */
object Main:

  /** Action is a subcommand of `recheck` command. */
  sealed abstract class Action extends Product with Serializable

  /** BatchAction holds `recheck batch` subcommand parameters. */
  final case class BatchAction(threadSize: Int) extends Action

  /** CalibrateRuntimeAttackAction holds `recheck calibrate-runtime-attack` subcommand parameters.
    *
    * `ratio` is the ratio of the recommended timeout to the estimated time of an attack, and `time` is the time of the
    * measurement.
    */
  final case class CalibrateRuntimeAttackAction(params: Parameters, ratio: Double, time: FiniteDuration) extends Action

  /** CheckAction holds `recheck check` subcommand parameters. */
  final case class CheckAction(pattern: InputPattern, params: Parameters, json: Boolean) extends Action

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

      val calibrateRuntimeAttack: Opts[Action] = Opts.subcommand(
        name = "calibrate-runtime-attack",
        help = "Measures reference matchings on the runtime of the dialect, and recommends the runtime-attack timeout."
      ):
        val ratio = Opts
          .option[Double](
            long = "ratio",
            help = "The ratio of the recommended timeout to the estimated time of an attack."
          )
          .withDefault(0.02)
          .validate("--ratio must be positive")(_ > 0)
        val time = Opts
          .option[Duration](long = "time", help = "The time of the measurement.")
          .withDefault(Duration(30, SECONDS))
          .mapValidated:
            case d: FiniteDuration if d > Duration.Zero => Validated.validNel(d)
            case _                                      => Validated.invalidNel("--time must be positive and finite")

        (params, ratio, time).mapN(CalibrateRuntimeAttackAction(_, _, _))

      agent <+> calibrateRuntimeAttack <+> generateRuntimeAttackScript <+> check

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

  /** Runs a matching on the runtime for `recheck calibrate-runtime-attack`. */
  def runRuntimeAttack(logger: Option[Context.Logger], compileTimeout: Duration)(
      source: String,
      dialect: Dialect,
      mode: UnitMode,
      string: AttackPattern,
      timeout: FiniteDuration
  ): Option[RuntimeAttack] =
    CalibrateRuntimeAttackCommand.run(logger, compileTimeout)(source, dialect, mode, string, timeout)
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

    case Right(action: CalibrateRuntimeAttackAction) =>
      val run = runRuntimeAttack(action.params.logger, action.params.runtimeAttackCompileTimeout)
      CalibrateRuntimeAttackCommand.calibrate(action.params, action.ratio, action.time)(run) match
        case Right(calibration) =>
          Console.out.println(calibration.toString)
          if calibration.timeout.isEmpty then exit(1)
        case Left(message) =>
          Console.err.println(message)
          exit(1)

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
