package codes.quine.labs.recheck.exec

/** ICUExecutor runs a C++ program file using ICU4C, which `c++` compiles, or the command line of `RECHECK_ICU` (e.g.
  * `c++ -I/opt/homebrew/opt/icu4c@79/include -L/opt/homebrew/opt/icu4c@79/lib`).
  *
  * The libraries of ICU (`-licui18n -licuuc -licudata`) follow the program on the command line.
  */
final case class ICUExecutor(program: SourceFile) extends Executor.Compiled("c++", "RECHECK_ICU", program):

  protected val versionArgs: Seq[Seq[String]] = Seq(Seq("--version"))

  protected def compileArgs(source: String, output: String): Seq[String] =
    Seq(source, "-o", output) ++ ICUExecutor.Libraries

object ICUExecutor:

  /** The libraries of ICU on a command line, which follow the program. */
  val Libraries: Seq[String] = Seq("-licui18n", "-licuuc", "-licudata")
