package codes.quine.labs.recheck.exec

import scala.annotation.unused
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.context.Context

/** Executor runs programs on a runtime, which is a command in `PATH`.
  *
  * The environment variable [[variable]] replaces the command with a command line of the shell (e.g. `env LC_ALL=C
  * ruby`), which the arguments follow.
  */
sealed abstract class Executor:

  /** The command of the runtime. */
  def command: String

  /** The environment variable of the command line replacing the command. */
  def variable: String

  /** The options of the command, which precede the program. */
  def options: Seq[String]

  /** The arguments of the command running the input. */
  protected def args: Seq[String]

  /** The environment variables to set in addition to the inherited ones. */
  protected def variables: Map[String, String] = Map.empty

  /** Returns the command line of the runtime, which is the value of the variable or the command. */
  def runtime(env: Map[String, String]): String = replacement(env).getOrElse(command)

  /** Returns the command line running the input. */
  def commandLine(env: Map[String, String]): String = (runtime(env) +: args).mkString(" ")

  /** Returns the command line running a program file of the name, which has the input in itself. */
  def scriptCommandLine(name: String): String = (command +: options :+ name).mkString(" ")

  /** Runs the input on the runtime in the environment variables.
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
      env: Map[String, String]
  )(using Context): ExecResult =
    Platform.run(command, args, replacement(env), variables, input, timeout, marker)

  /** Returns the command line replacing the command in the environment variables, if any. */
  protected def replacement(env: Map[String, String]): Option[String] =
    env.get(variable).map(_.trim).filter(_.nonEmpty)

object Executor:

  /** Whether this platform can run programs. */
  val isAvailable: Boolean = Platform.isAvailable

  /** Stdin is an executor whose input is the program, which the command reads from the standard input. */
  abstract class Stdin private[exec] (
      val command: String,
      val variable: String,
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
      val variable: String,
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
    override def commandLine(env: Map[String, String]): String = compileRunCommandLine(runtime(env), program.name)

    /** Returns the command line compiling a program file of the name and running it. */
    override def scriptCommandLine(name: String): String = compileRunCommandLine(command, name)

    override def exec(
        input: String,
        timeout: Option[FiniteDuration],
        compileTimeout: Duration,
        marker: String,
        env: Map[String, String]
    )(using ctx: Context): ExecResult =
      val output = ctx.pauseTimeout:
        Platform.compile(
          command,
          replacement(env),
          program,
          versionArgs,
          compileArgs,
          entry,
          variables,
          compileTimeout
        )
      runArgs(output) match
        case Some(args) => Platform.run(command, args, replacement(env), variables, input, timeout, marker)
        case None       => Platform.run(output, Seq.empty, None, variables, input, timeout, marker)

    private def compileRunCommandLine(runtime: String, name: String): String =
      val output = outputName(name)
      val run = runArgs(output).fold(s"./$output")(args => (runtime +: args).mkString(" "))
      s"${(runtime +: compileArgs(name, output)).mkString(" ")} && $run"

  /** Returns the name of the output compiled from a source file, which is the name without the extension. */
  private[exec] def outputName(name: String): String =
    val dot = name.lastIndexOf('.')
    if dot < 0 then name else name.take(dot)
