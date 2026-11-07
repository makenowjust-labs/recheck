package codes.quine.labs.recheck.exec

/** ICUExecutor runs a C++ program file using ICU4C, which `c++` compiles, or the command line of `RECHECK_ICU` (e.g.
  * `c++ -I/opt/homebrew/opt/icu4c@79/include -L/opt/homebrew/opt/icu4c@79/lib`).
  *
  * The libraries of ICU (`-licui18n -licuuc -licudata`) follow the program on the command line.
  */
final case class ICUExecutor(program: SourceFile)
    extends Executor.Compiled("c++", "RECHECK_ICU", program, Seq("-licui18n", "-licuuc", "-licudata")):

  protected val versionArgs: Seq[String] = Seq("--version")
