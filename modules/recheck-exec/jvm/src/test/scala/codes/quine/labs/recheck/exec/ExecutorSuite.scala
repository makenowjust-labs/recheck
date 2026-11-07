package codes.quine.labs.recheck.exec

import java.nio.file.Files

import codes.quine.labs.recheck.common.Context

class ExecutorSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val java: JavaExecutor = JavaExecutor(SourceFile("RecheckTest.java", "class RecheckTest {}"))

  val dotnet: DotNetExecutor = DotNetExecutor(SourceFile("RecheckTest.cs", "// test"))

  val icu: ICUExecutor = ICUExecutor(SourceFile("RecheckTest.cpp", "// test"))

  /** A fake compiler, which prints its version for `--version`, and otherwise copies the program, a shell script, to
    * the binary.
    */
  val fakeCompiler: String =
    "#!/bin/sh\nif [ \"$1\" = --version ]; then echo fake 1.0; exit 0; fi\nfor a in \"$@\"; do\n  if [ \"$prev\" = -o ]; then out=\"$a\"; fi\n  case \"$a\" in *.sh) src=\"$a\" ;; esac\n  prev=\"$a\"\ndone\ncp \"$src\" \"$out\"\nchmod +x \"$out\"\n"

  val executors: Seq[Executor] =
    Seq(NodeExecutor, PythonExecutor, PythonRegexExecutor, RubyExecutor, java, dotnet, PerlExecutor, PHPExecutor, icu)

  test("Executor#variable"):
    assertEquals(
      executors.map(_.variable),
      Seq("NODE", "PYTHON", "PYTHON_REGEX", "RUBY", "JAVA", "DOTNET", "PERL", "PHP", "ICU").map("RECHECK_" + _)
    )

  test("Executor#commandLine"):
    assertEquals(
      executors.map(_.commandLine(Map.empty)),
      Seq(
        "node -",
        "python3 -I -",
        "python3 -I -",
        "ruby --disable-gems -",
        "java --source 17 RecheckTest.java",
        "dotnet run RecheckTest.cs",
        "perl -",
        "php",
        "c++ RecheckTest.cpp -o RecheckTest -licui18n -licuuc -licudata"
      )
    )
    // The variable replaces the command, and an empty value is ignored.
    val env = Map("RECHECK_PYTHON" -> " env LC_ALL=C python3 ", "RECHECK_RUBY" -> " ")
    assertEquals(PythonExecutor.runtime(env), "env LC_ALL=C python3")
    assertEquals(PythonExecutor.commandLine(env), "env LC_ALL=C python3 -I -")
    assertEquals(RubyExecutor.runtime(env), "ruby")
    assertEquals(
      PythonRegexExecutor.commandLine(Map("RECHECK_PYTHON_REGEX" -> "venv/bin/python")),
      "venv/bin/python -I -"
    )
    assertEquals(java.commandLine(Map("RECHECK_JAVA" -> "my-java")), "my-java --source 17 RecheckTest.java")
    assertEquals(
      icu.commandLine(Map("RECHECK_ICU" -> "c++ -I/icu/include")),
      "c++ -I/icu/include RecheckTest.cpp -o RecheckTest -licui18n -licuuc -licudata"
    )

  test("Executor#scriptCommandLine"):
    assertEquals(
      executors.map(_.scriptCommandLine("script")),
      Seq(
        "node script",
        "python3 -I script",
        "python3 -I script",
        "ruby --disable-gems script",
        "java --source 17 script",
        "dotnet run script",
        "perl script",
        "php script",
        "c++ script -o script -licui18n -licuuc -licudata && ./script"
      )
    )
    assertEquals(
      icu.scriptCommandLine("runtime-attack.cpp"),
      "c++ runtime-attack.cpp -o runtime-attack -licui18n -licuuc -licudata && ./runtime-attack"
    )

  test("Executor#exec"):
    // The command runs from `PATH` without the variable.
    object ShellExecutor extends Executor.Stdin("sh", "RECHECK_SH", stdin = Seq.empty)
    assertEquals(ShellExecutor.exec("echo foo", None, "start", Map.empty), ExecResult.Exit(0, "foo\n", ""))

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
      executor.exec("input", None, "start", Map(executor.variable -> s"'$runtime'")) match
        case ExecResult.Exit(0, out, "") => out
        case result                      => fail(s"unexpected: $result")

    try
      // The first line has the variables, which the environment may have.
      def args(executor: Executor): String = exec(executor).linesIterator.drop(1).mkString("\n")

      assertEquals(args(NodeExecutor), "-\ninput")
      assertEquals(args(PythonExecutor), "-I\n-\ninput")
      assertEquals(args(RubyExecutor), "--disable-gems\n-\n# encoding: UTF-8\ninput")
      assertEquals(args(java), "--source\n17\nclass RecheckTest {}\ninput")
      assertEquals(exec(dotnet), "11\nrun\n// test\ninput\n")
      assertEquals(args(PerlExecutor), "-\ninput")
      assertEquals(args(PHPExecutor), "input")
    finally
      Files.deleteIfExists(SourceDirectory.path(java.program))
      Files.deleteIfExists(SourceDirectory.path(dotnet.program))
      Files.deleteIfExists(runtime)
      Files.deleteIfExists(dir)

  test("Executor#exec: a compiled program"):
    val dir = Files.createTempDirectory("recheck executor ")
    val compiler = dir.resolve("my compiler")
    Files.writeString(compiler, fakeCompiler)
    compiler.toFile.setExecutable(true)
    val program = SourceFile("RecheckTest.sh", s"#!/bin/sh\necho \"$$(cat)\" # ${System.nanoTime()}\n")
    val executor = ICUExecutor(program)
    try
      assertEquals(
        executor.exec("input", None, "start", Map("RECHECK_ICU" -> s"'$compiler'")),
        ExecResult.Exit(0, "input\n", "")
      )
    finally
      Files.deleteIfExists(SourceDirectory.path(program))
      Files.deleteIfExists(
        SourceDirectory.binaryPath(program, Seq(s"'$compiler'", "-licui18n", "-licuuc", "-licudata"), "fake 1.0\n")
      )
      Files.deleteIfExists(compiler)
      Files.deleteIfExists(dir)
