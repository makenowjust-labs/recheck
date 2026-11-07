package codes.quine.labs.recheck.exec

/** BoostRegexExecutor runs a C++ program file using Boost.Regex, which `c++` compiles, or the command line of
  * `RECHECK_BOOST_REGEX` (e.g. `c++ -std=c++17 -O2 -I/opt/homebrew/include`).
  *
  * With `hasICU`, the program uses `boost::u32regex`, and the libraries of ICU follow the program on the command line,
  * which has the paths of ICU too.
  */
final case class BoostRegexExecutor(program: SourceFile, hasICU: Boolean)
    extends Executor.Compiled("c++", "RECHECK_BOOST_REGEX", program):

  protected val versionArgs: Seq[Seq[String]] = Seq(Seq("--version"))

  protected def compileArgs(source: String, output: String): Seq[String] =
    Seq(source, "-o", output) ++ (if hasICU then ICUExecutor.Libraries else Seq.empty)
