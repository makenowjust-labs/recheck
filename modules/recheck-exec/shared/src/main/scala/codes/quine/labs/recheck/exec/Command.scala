package codes.quine.labs.recheck.exec

/** Command is a command line that reads a script from the standard input.
  *
  * @param program
  *   the program name, which is looked up in `PATH`
  * @param args
  *   the arguments
  * @param env
  *   the environment variables to set in addition to the inherited ones
  */
final case class Command(program: String, args: Seq[String] = Seq.empty, env: Map[String, String] = Map.empty):
  override def toString: String = (program +: args).mkString(" ")
