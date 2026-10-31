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
    val pythonType = Opts.option[String](long = "python-type", help = "The string type (python).").orNone
    val rubyEncoding = Opts.option[String](long = "ruby-encoding", help = "The encoding (ruby).").orNone
    val perlUtf8 = Opts
      .option[Boolean](long = "perl-utf8", help = "Whether the program has `use utf8` and decoded strings (perl).")
      .orNone
    val perlSource = Opts.option[PerlSource](long = "perl-source", help = "How the pattern is given (perl).").orNone
    (pythonType, rubyEncoding, perlUtf8, perlSource).mapN: (pythonType, rubyEncoding, perlUtf8, perlSource) =>
      withSettings(_, pythonType, rubyEncoding, perlUtf8, perlSource)

  /** Returns the dialect with the settings, or an error if the dialect does not have a given setting. */
  def withSettings(
      dialect: Dialect,
      pythonType: Option[String],
      rubyEncoding: Option[String],
      perlUtf8: Option[Boolean],
      perlSource: Option[PerlSource]
  ): ValidatedNel[String, Dialect] =
    val misplaced = Seq(
      ("python-type", pythonType.isDefined, dialect.isPython),
      ("ruby-encoding", rubyEncoding.isDefined, dialect.isRuby),
      ("perl-utf8", perlUtf8.isDefined, dialect.isPerl),
      ("perl-source", perlSource.isDefined, dialect.isPerl)
    ).collectFirst { case (option, true, false) => option }
    def named[A](option: String, value: Option[String], default: A)(of: String => Option[A]): ValidatedNel[String, A] =
      value.fold(Validated.validNel(default)): name =>
        Validated.fromOption(of(name), s"unknown value of --$option: $name").toValidatedNel
    misplaced match
      case Some(option) => Validated.invalidNel(s"--$option is not an option of ${dialect.name}")
      case None         =>
        dialect match
          case python: Dialect.Python =>
            named("python-type", pythonType, python.`type`)(Dialect.Python.Type.of).map(Dialect.Python(_))
          case ruby: Dialect.Ruby =>
            named("ruby-encoding", rubyEncoding, ruby.encoding)(Dialect.Ruby.Encoding.of).map(Dialect.Ruby(_))
          case perl: Dialect.Perl =>
            Validated.validNel(Dialect.Perl(perlUtf8.getOrElse(perl.utf8), perlSource.getOrElse(perl.source)))
          case _ => Validated.validNel(dialect)

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
