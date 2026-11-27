package codes.quine.labs.recheck.exec

import scala.annotation.unused
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.context.Context

/** Executor runs programs on a runtime, which is a command in `PATH`.
  *
  * A replacement replaces the command with a command line of the shell (e.g. `env LC_ALL=C ruby`), which the arguments
  * follow.
  */
sealed abstract class Executor:

  /** The command of the runtime. */
  def command: String

  /** The options of the command, which precede the program. */
  def options: Seq[String]

  /** The arguments of the command running the input. */
  protected def args: Seq[String]

  /** The environment variables to set in addition to the inherited ones. */
  protected def variables: Map[String, String] = Map.empty

  /** Returns the command line of the runtime, which is the replacement or the command. */
  def runtime(replacement: Option[String]): String = replacement.getOrElse(command)

  /** Returns the command line running the input. */
  def commandLine(replacement: Option[String]): String = (runtime(replacement) +: args).mkString(" ")

  /** Returns the command line running a program file of the name, which has the input in itself. */
  def scriptCommandLine(name: String): String = (command +: options :+ name).mkString(" ")

  /** Runs the input on the runtime, whose command is replaced by the replacement, if any.
    *
    * The timeout counts from the time when the process prints the marker as a line, so that the startup of the runtime
    * is not counted. Waiting for the marker is limited only by the context. `compileTimeout` limits the compilation of
    * an executor compiling the program (see [[Executor.Compiled]]).
    */
  def exec(
      input: String,
      timeout: Option[FiniteDuration],
      @unused compileTimeout: Duration,
      marker: String,
      replacement: Option[String]
  )(using Context): ExecResult =
    Platform.run(command, args, replacement, variables, input, timeout, marker)

object Executor:

  /** Whether this platform can run programs. */
  val isAvailable: Boolean = Platform.isAvailable

  /** Stdin is an executor whose input is the program, which the command reads from the standard input. */
  abstract class Stdin private[exec] (
      val command: String,
      val options: Seq[String] = Seq.empty,
      stdin: Seq[String] = Seq("-")
  ) extends Executor:

    protected val args: Seq[String] = options ++ stdin

  /** Compiled is an executor running a program file that the command compiles once, whose input is the data of the
    * program on the standard input.
    *
    * The command compiles the program into an output, which is a binary running by itself or a directory that the
    * command runs. The output is written into a directory of this user, and stays there for the same source, the same
    * command line and the same version of the command, while it has its entry file (see [[entry]]). The compilation
    * does not count toward the timeout of the context, and `compileTimeout` of [[exec]] limits it.
    */
  abstract class Compiled private[exec] (
      val command: String,
      program: SourceFile,
      override protected val variables: Map[String, String] = Map.empty
  ) extends Executor:

    val options: Seq[String] = Seq.empty

    protected val args: Seq[String] = Seq.empty

    /** The arguments of the runs of the command printing its version, whose outputs tell an output from another. */
    protected def versionArgs: Seq[Seq[String]]

    /** Returns the arguments of the command compiling the program file `source` into `output`, which the command
      * creates.
      */
    protected def compileArgs(source: String, output: String): Seq[String]

    /** Returns the arguments of the command running the program compiled into `output`, or `None` if `output` is a
      * binary running by itself.
      */
    protected def runArgs(output: String): Option[Seq[String]] = None

    /** Returns the file of the program compiled into `output` that a run reads, which is `output` itself for a binary.
      *
      * An output without it (e.g. emptied by a cleaner of the temporary directory) is compiled again.
      */
    protected def entry(output: String): String = output

    /** Returns the command line compiling the program and running it. */
    override def commandLine(replacement: Option[String]): String =
      compileRunCommandLine(runtime(replacement), program.name)

    /** Returns the command line compiling a program file of the name and running it. */
    override def scriptCommandLine(name: String): String = compileRunCommandLine(command, name)

    override def exec(
        input: String,
        timeout: Option[FiniteDuration],
        compileTimeout: Duration,
        marker: String,
        replacement: Option[String]
    )(using ctx: Context): ExecResult =
      val output = ctx.pauseTimeout:
        Platform.compile(
          command,
          replacement,
          program,
          versionArgs,
          compileArgs,
          entry,
          variables,
          compileTimeout
        )
      runArgs(output) match
        case Some(args) => Platform.run(command, args, replacement, variables, input, timeout, marker)
        case None       => Platform.run(output, Seq.empty, None, variables, input, timeout, marker)

    private def compileRunCommandLine(runtime: String, name: String): String =
      val output = outputName(name)
      val run = runArgs(output).fold(s"./$output")(args => (runtime +: args).mkString(" "))
      s"${(runtime +: compileArgs(name, output)).mkString(" ")} && $run"

  /** Returns the name of the output compiled from a source file, which is the name without the extension. */
  private[exec] def outputName(name: String): String =
    val dot = name.lastIndexOf('.')
    if dot < 0 then name else name.take(dot)
