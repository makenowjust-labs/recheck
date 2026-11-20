package codes.quine.labs.recheck.exec

/** DotNetExecutor runs a C# program file on `dotnet` (.NET 10 SDK or later).
  *
  * The command builds the program as a file-based app without build servers, and runs the assembly of the build, whose
  * name is the name of the file without the extension.
  */
final case class DotNetExecutor(program: SourceFile)
    extends Executor.Compiled(
      "dotnet",
      program,
      Map("DOTNET_NOLOGO" -> "1", "DOTNET_CLI_TELEMETRY_OPTOUT" -> "1")
    ):

  // The latest SDK builds the program without `global.json`, and these print faster than `--version`.
  protected val versionArgs: Seq[Seq[String]] = Seq(Seq("--list-sdks"), Seq("--list-runtimes"))

  protected def compileArgs(source: String, output: String): Seq[String] =
    Seq("build", source, "-o", output, "--disable-build-servers")

  override protected def runArgs(output: String): Option[Seq[String]] = Some(Seq(entry(output)))

  override protected def entry(output: String): String = s"$output/${Executor.outputName(program.name)}.dll"

  /** Returns the command line running a program file of the name as a file-based app. */
  override def scriptCommandLine(name: String): String = s"$command run $name"
