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
import cats.data.ValidatedNel
import cats.syntax.apply.*
import cats.syntax.semigroupk.*
import com.monovore.decline.Command
import com.monovore.decline.Opts
import io.circe.syntax.*

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.cli.Main.BatchAction
import codes.quine.labs.recheck.cli.Main.CheckAction
import codes.quine.labs.recheck.cli.Main.RecallScriptAction
import codes.quine.labs.recheck.cli.Main.command
import codes.quine.labs.recheck.cli.arguments.given
import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource

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
    * @param json
    *   whether it shows the diagnostics in JSON
    */
  final case class CheckAction(pattern: InputPattern, params: Parameters, all: Boolean = false, json: Boolean = false)
      extends Action

  /** RecallScriptAction holds `recheck generate-recall-script` subcommand parameters.
    *
    * @param diagnostics
    *   the path of the diagnostics JSON, or `-` for the standard input
    */
  final case class RecallScriptAction(diagnostics: String) extends Action

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
      val params = (ParametersOpts(logger), dialectSettings).tupled.mapValidated: (params, settings) =>
        settings(params.dialect).map(dialect => params.copy(dialect = dialect))

      val pattern = Opts.argument[InputPattern](metavar = "pattern")
      val all = Opts
        .flag(
          long = "all",
          help = "Show all the diagnostics, including the vulnerable ones whose recall validation failed."
        )
        .orFalse
      val json = Opts
        .flag(long = "json", help = "Show the diagnostics in JSON, an array of them with `--all`.")
        .orFalse
      val check: Opts[Action] = (pattern, params, all, json).mapN(CheckAction(_, _, _, _))

      val agent: Opts[Action] = Opts.subcommand(name = "agent", help = "Starts the batch mode."):
        val threadSize = Opts
          .option[Int](long = "thread-size", short = "t", help = "A number of thread for processing")
          .withDefault(sys.runtime.availableProcessors())

        threadSize.map(BatchAction.apply)

      val recallScript: Opts[Action] = Opts.subcommand(
        name = "generate-recall-script",
        help = "Generates the recall validation program of a vulnerable diagnostics in JSON as a standalone script."
      ):
        Opts.argument[String](metavar = "diagnostics").map(RecallScriptAction.apply)

      agent <+> recallScript <+> check

  /** The options of the dialect's settings, which set them on the dialect of `--dialect`. */
  def dialectSettings: Opts[Dialect => ValidatedNel[String, Dialect]] =
    val encoding = Opts
      .option[String](long = "encoding", help = "The encoding of the dialect (python, ruby and perl).")
      .orNone
    val source = Opts.option[PerlSource](long = "perl-source", help = "How the pattern is given (perl).").orNone
    val utf8 = Opts.option[Boolean](long = "perl-utf8", help = "Whether the pattern string is UTF-8 (perl).").orNone
    (encoding, source, utf8).mapN((encoding, source, utf8) => withSettings(_, encoding, source, utf8))

  /** Returns the dialect with the settings, or an error if the dialect does not have a given setting. */
  def withSettings(
      dialect: Dialect,
      encoding: Option[String],
      source: Option[PerlSource],
      utf8: Option[Boolean]
  ): ValidatedNel[String, Dialect] =
    def invalid(option: String): ValidatedNel[String, Dialect] =
      Validated.invalidNel(s"--$option is not an option of ${dialect.name}")
    def encodingOf[A](of: String => Option[A], default: A): ValidatedNel[String, A] =
      encoding.fold(Validated.validNel(default)): name =>
        Validated.fromOption(of(name), s"unknown encoding of ${dialect.name}: $name").toValidatedNel
    dialect match
      case _ if !dialect.isPerl && source.isDefined => invalid("perl-source")
      case _ if !dialect.isPerl && utf8.isDefined   => invalid("perl-utf8")
      case python: Dialect.Python                   =>
        encodingOf(Dialect.Python.Encoding.of, python.encoding).map(Dialect.Python(_))
      case ruby: Dialect.Ruby => encodingOf(Dialect.Ruby.Encoding.of, ruby.encoding).map(Dialect.Ruby(_))
      case perl: Dialect.Perl =>
        encodingOf(Dialect.Perl.Encoding.of, perl.encoding)
          .map(Dialect.Perl(_, source.getOrElse(perl.source), utf8.orElse(perl.utf8)))
      case _ if encoding.isDefined => invalid("encoding")
      case _                       => Validated.validNel(dialect)

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
      if !action.json then Console.out.println(diagnostics.mkString("\n\n"))
      else if action.all then Console.out.println(diagnostics.asJson.noSpaces)
      else Console.out.println(diagnostics.head.asJson.noSpaces)
      diagnostics.last match
        case _: Diagnostics.Safe                                => () // skip
        case _: Diagnostics.Vulnerable | _: Diagnostics.Unknown => exit(1)

    case Right(action: RecallScriptAction) =>
      val script = for
        json <- read(action.diagnostics)
        script <- RecallScriptCommand.generate(json)
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
