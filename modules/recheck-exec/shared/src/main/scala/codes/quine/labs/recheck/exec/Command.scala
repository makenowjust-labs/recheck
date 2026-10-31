package codes.quine.labs.recheck.exec

/** Command is a command line that reads a script from the standard input.
  *
  * @param program
  *   the program name, which is looked up in `PATH`
  * @param args
  *   the arguments
  * @param env
  *   the environment variables to set in addition to the inherited ones
  * @param source
  *   the source file of the program that the command runs, whose path follows the arguments
  */
final case class Command(
    program: String,
    args: Seq[String] = Seq.empty,
    env: Map[String, String] = Map.empty,
    source: Option[Command.Source] = None
):

  /** Returns this command run by a shell command line instead of the program.
    *
    * The shell splits the command line as usual, and the arguments follow it.
    */
  def withShell(shell: String, commandLine: String): Command =
    copy(program = shell, args = Seq("-c", s"$commandLine \"$$@\"", "sh") ++ args)

  override def toString: String = (program +: (args ++ source.map(_.name))).mkString(" ")

object Command:

  /** Source is a source file, which the executor writes once into a directory of this user.
    *
    * @param name
    *   the file name, to which the executor adds the hash of the content
    */
  final case class Source(name: String, content: String)
