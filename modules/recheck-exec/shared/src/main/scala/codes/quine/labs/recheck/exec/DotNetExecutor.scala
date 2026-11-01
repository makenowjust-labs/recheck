package codes.quine.labs.recheck.exec

/** DotNetExecutor runs a C# program file on `dotnet run` (.NET 10 or later), or the command line of `RECHECK_DOTNET`.
  *
  * `dotnet run` builds the program on its first run, which takes some seconds, and reuses the build after that.
  */
final case class DotNetExecutor(program: SourceFile)
    extends Executor.File(
      "dotnet",
      "RECHECK_DOTNET",
      Seq("run"),
      program,
      Map("DOTNET_NOLOGO" -> "1", "DOTNET_CLI_TELEMETRY_OPTOUT" -> "1")
    )
