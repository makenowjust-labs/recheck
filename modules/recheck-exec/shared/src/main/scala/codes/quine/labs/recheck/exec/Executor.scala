package codes.quine.labs.recheck.exec

import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.common.Context

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

  /** The program file whose path follows the arguments, or `None` if the input is the program. */
  protected def file: Option[SourceFile] = None

  /** The environment variables to set in addition to the inherited ones. */
  protected def variables: Map[String, String] = Map.empty

  /** Returns the command line of the runtime, which is the value of the variable or the command. */
  def runtime(env: Map[String, String]): String = replacement(env).getOrElse(command)

  /** Returns the command line running the input, with the name of the program file. */
  def commandLine(env: Map[String, String]): String = (runtime(env) +: (args ++ file.map(_.name))).mkString(" ")

  /** Returns the command line running a program file of the name, which has the input in itself. */
  def scriptCommandLine(name: String): String = (command +: options :+ name).mkString(" ")

  /** Runs the input on the runtime in the environment variables.
    *
    * With a marker, the timeout counts from the time when the process prints the marker as a line, so that the startup
    * of the runtime is not counted. Waiting for the marker is limited only by the context.
    */
  def exec(input: String, timeout: Option[FiniteDuration], marker: Option[String], env: Map[String, String])(using
      Context
  ): ExecResult =
    Platform.run(command, args, replacement(env), variables, file, input, timeout, marker)

  private def replacement(env: Map[String, String]): Option[String] =
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

  /** File is an executor running a program file, whose input is the data of the program on the standard input.
    *
    * The file is written once into a directory of this user, and stays there.
    */
  abstract class File private[exec] (
      val command: String,
      val variable: String,
      val options: Seq[String],
      program: SourceFile,
      override protected val variables: Map[String, String] = Map.empty
  ) extends Executor:
    protected val args: Seq[String] = options
    override protected val file: Option[SourceFile] = Some(program)
