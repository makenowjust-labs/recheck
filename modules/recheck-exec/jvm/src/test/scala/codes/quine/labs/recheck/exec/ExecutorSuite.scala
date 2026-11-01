package codes.quine.labs.recheck.exec

import java.nio.file.Files

import codes.quine.labs.recheck.common.Context

class ExecutorSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val java: JavaExecutor = JavaExecutor(SourceFile("RecheckTest.java", "class RecheckTest {}"))

  val dotnet: DotNetExecutor = DotNetExecutor(SourceFile("RecheckTest.cs", "// test"))

  val executors: Seq[Executor] =
    Seq(NodeExecutor, PythonExecutor, RubyExecutor, java, dotnet, PerlExecutor, PHPExecutor)

  test("Executor#variable"):
    assertEquals(
      executors.map(_.variable),
      Seq("NODE", "PYTHON", "RUBY", "JAVA", "DOTNET", "PERL", "PHP").map("RECHECK_" + _)
    )

  test("Executor#commandLine"):
    assertEquals(
      executors.map(_.commandLine(Map.empty)),
      Seq(
        "node -",
        "python3 -I -",
        "ruby -",
        "java --source 17 RecheckTest.java",
        "dotnet run RecheckTest.cs",
        "perl -",
        "php"
      )
    )
    // The variable replaces the command, and an empty value is ignored.
    val env = Map("RECHECK_PYTHON" -> " env LC_ALL=C python3 ", "RECHECK_RUBY" -> " ")
    assertEquals(PythonExecutor.runtime(env), "env LC_ALL=C python3")
    assertEquals(PythonExecutor.commandLine(env), "env LC_ALL=C python3 -I -")
    assertEquals(RubyExecutor.runtime(env), "ruby")
    assertEquals(java.commandLine(Map("RECHECK_JAVA" -> "my-java")), "my-java --source 17 RecheckTest.java")

  test("Executor#scriptCommandLine"):
    assertEquals(
      executors.map(_.scriptCommandLine("recall")),
      Seq(
        "node recall",
        "python3 -I recall",
        "ruby recall",
        "java --source 17 recall",
        "dotnet run recall",
        "perl recall",
        "php recall"
      )
    )

  test("Executor#exec"):
    assertEquals(NodeExecutor.exec("console.log('foo');", None, None, Map.empty), ExecResult.Exit(0, "foo\n", ""))

  test("Executor#exec: the variable"):
    // A fake runtime prints the variables of .NET, the arguments with the content of a file, and the input.
    val dir = Files.createTempDirectory("recheck executor ")
    val runtime = dir.resolve("my runtime")
    Files.writeString(
      runtime,
      """|#!/bin/sh
         |echo "$DOTNET_NOLOGO$DOTNET_CLI_TELEMETRY_OPTOUT"
         |for a in "$@"; do if [ -f "$a" ]; then cat "$a"; echo; else echo "$a"; fi; done
         |cat
         |""".stripMargin
    )
    runtime.toFile.setExecutable(true)
    def exec(executor: Executor): String =
      executor.exec("input", None, None, Map(executor.variable -> s"'$runtime'")) match
        case ExecResult.Exit(0, out, "") => out
        case result                      => fail(s"unexpected: $result")

    try
      // The first line has the variables, which the environment may have.
      def args(executor: Executor): String = exec(executor).linesIterator.drop(1).mkString("\n")

      assertEquals(args(NodeExecutor), "-\ninput")
      assertEquals(args(PythonExecutor), "-I\n-\ninput")
      assertEquals(args(RubyExecutor), "-\n# encoding: UTF-8\ninput")
      assertEquals(args(java), "--source\n17\nclass RecheckTest {}\ninput")
      assertEquals(exec(dotnet), "11\nrun\n// test\ninput\n")
      assertEquals(args(PerlExecutor), "-\ninput")
      assertEquals(args(PHPExecutor), "input")
    finally
      Files.deleteIfExists(SourceFiles.path(java.program))
      Files.deleteIfExists(SourceFiles.path(dotnet.program))
      Files.deleteIfExists(runtime)
      Files.deleteIfExists(dir)
