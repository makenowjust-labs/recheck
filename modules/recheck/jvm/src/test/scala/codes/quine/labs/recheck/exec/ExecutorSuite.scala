package codes.quine.labs.recheck.exec

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Comparator
import scala.concurrent.duration.*

import codes.quine.labs.recheck.context.Context

class ExecutorSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  val java: JavaExecutor = JavaExecutor(SourceFile("RecheckTest.java", "class RecheckTest {}"))

  val dotnet: DotNetExecutor = DotNetExecutor(SourceFile("RecheckTest.cs", "// test"))

  val icu: ICUExecutor = ICUExecutor(SourceFile("RecheckTest.cpp", "// test"))

  val boostRegex: BoostRegexExecutor = BoostRegexExecutor(SourceFile("RecheckTest.cpp", "// test"), hasICU = false)

  val boostRegexICU: BoostRegexExecutor = BoostRegexExecutor(SourceFile("RecheckTest.cpp", "// test"), hasICU = true)

  /** A fake compiler, which prints its version for `--version`, and otherwise copies the program, a shell script, to
    * the output.
    */
  val fakeCompiler: String =
    "#!/bin/sh\nif [ \"$1\" = --version ]; then echo fake 1.0; exit 0; fi\nfor a in \"$@\"; do\n  if [ \"$prev\" = -o ]; then out=\"$a\"; fi\n  case \"$a\" in *.sh) src=\"$a\" ;; esac\n  prev=\"$a\"\ndone\ncp \"$src\" \"$out\"\nchmod +x \"$out\"\n"

  val executors: Seq[Executor] =
    Seq(
      NodeExecutor,
      PythonExecutor,
      PythonRegexExecutor,
      RubyExecutor,
      java,
      dotnet,
      PerlExecutor,
      PHPExecutor,
      icu,
      boostRegex,
      boostRegexICU
    )

  test("Executor#variable"):
    assertEquals(
      executors.map(_.variable),
      Seq(
        "NODE",
        "PYTHON",
        "PYTHON_REGEX",
        "RUBY",
        "JAVA",
        "DOTNET",
        "PERL",
        "PHP",
        "ICU",
        "BOOST_REGEX",
        "BOOST_REGEX"
      )
        .map("RECHECK_" + _)
    )

  test("Executor#commandLine"):
    assertEquals(
      executors.map(_.commandLine(Map.empty)),
      Seq(
        "node -",
        "python3 -I -",
        "python3 -I -",
        "ruby --disable-gems -",
        "java -m jdk.compiler/com.sun.tools.javac.Main --release 17 -d RecheckTest RecheckTest.java && java -cp RecheckTest RecheckTest",
        "dotnet build RecheckTest.cs -o RecheckTest --disable-build-servers && dotnet RecheckTest/RecheckTest.dll",
        "perl -",
        "php",
        "c++ RecheckTest.cpp -o RecheckTest -licui18n -licuuc -licudata && ./RecheckTest",
        "c++ RecheckTest.cpp -o RecheckTest && ./RecheckTest",
        "c++ RecheckTest.cpp -o RecheckTest -licui18n -licuuc -licudata && ./RecheckTest"
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
    assertEquals(
      java.commandLine(Map("RECHECK_JAVA" -> "my-java")),
      "my-java -m jdk.compiler/com.sun.tools.javac.Main --release 17 -d RecheckTest RecheckTest.java && my-java -cp RecheckTest RecheckTest"
    )
    assertEquals(
      icu.commandLine(Map("RECHECK_ICU" -> "c++ -I/icu/include")),
      "c++ -I/icu/include RecheckTest.cpp -o RecheckTest -licui18n -licuuc -licudata && ./RecheckTest"
    )
    assertEquals(
      boostRegex.commandLine(Map("RECHECK_BOOST_REGEX" -> "c++ -I/boost/include")),
      "c++ -I/boost/include RecheckTest.cpp -o RecheckTest && ./RecheckTest"
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
        "c++ script -o script -licui18n -licuuc -licudata && ./script",
        "c++ script -o script && ./script",
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
    assertEquals(
      ShellExecutor.exec("echo foo", None, Duration.Inf, "start", Map.empty),
      ExecResult.Exit(0, "foo\n", "")
    )

  /** Creates an executable script in a new directory with a space in its name, and runs the body with its path. */
  def withScript[A](name: String, content: String)(body: Path => A): A =
    val dir = Files.createTempDirectory("recheck executor ")
    val script = dir.resolve(name)
    Files.writeString(script, content)
    script.toFile.setExecutable(true)
    try body(script)
    finally
      Files.deleteIfExists(script)
      Files.deleteIfExists(dir)

  /** Deletes the file or the directory with its contents. */
  def delete(path: Path): Unit =
    if Files.exists(path) then Files.walk(path).sorted(Comparator.reverseOrder()).forEach(Files.delete)

  test("Executor#exec: the variable"):
    // A fake runtime prints the arguments and the input.
    val content = "#!/bin/sh\nfor a in \"$@\"; do echo \"$a\"; done\ncat\n"
    withScript("my runtime", content): runtime =>
      def exec(executor: Executor): ExecResult =
        executor.exec("input", None, Duration.Inf, "start", Map(executor.variable -> s"'$runtime'"))

      assertEquals(exec(NodeExecutor), ExecResult.Exit(0, "-\ninput\n", ""))
      assertEquals(exec(PythonExecutor), ExecResult.Exit(0, "-I\n-\ninput\n", ""))
      assertEquals(exec(RubyExecutor), ExecResult.Exit(0, "--disable-gems\n-\n# encoding: UTF-8\ninput\n", ""))
      assertEquals(exec(PerlExecutor), ExecResult.Exit(0, "-\ninput\n", ""))
      assertEquals(exec(PHPExecutor), ExecResult.Exit(0, "input\n", ""))

  test("Executor#exec: a program that the runtime compiles"):
    // A fake runtime creates the output of a compilation with the entry files of Java and .NET, and otherwise prints the
    // variables of .NET, the arguments and the input.
    val content =
      """|#!/bin/sh
         |for a in "$@"; do
         |  if [ "$prev" = -d ] || [ "$prev" = -o ]; then mkdir "$a"; touch "$a/RecheckTest.class" "$a/RecheckTest.dll"; exit 0; fi
         |  prev="$a"
         |done
         |echo "$DOTNET_NOLOGO$DOTNET_CLI_TELEMETRY_OPTOUT"
         |echo "$*"
         |cat
         |""".stripMargin
    // The content is unique to this test, so that the runtime compiles it.
    val nonce = s"// ${System.nanoTime()}"
    val compiledJava = JavaExecutor(SourceFile("RecheckTest.java", nonce))
    val compiledDotNet = DotNetExecutor(SourceFile("RecheckTest.cs", nonce))
    withScript("my runtime", content): runtime =>
      def exec(executor: Executor): Seq[String] =
        executor.exec("input", None, Duration.Inf, "start", Map(executor.variable -> s"'$runtime'")) match
          case ExecResult.Exit(0, out, "") => out.linesIterator.toSeq
          case result                      => fail(s"unexpected: $result")

      // The first line of Java has the variables of .NET, which the environment may have.
      val Seq(_, javaArgs, "input") = exec(compiledJava): @unchecked
      val Seq("11", dotnetArgs, "input") = exec(compiledDotNet): @unchecked
      try
        assert(clue(javaArgs).matches("-cp .*/RecheckTest-[0-9a-f]{16} RecheckTest"))
        // The key of the output of .NET has the lists of the SDKs and the runtimes.
        val dotnetLine = Seq(s"'$runtime'", "build", "RecheckTest.cs", "-o", "RecheckTest", "--disable-build-servers")
        val versions = "11\n--list-sdks\n11\n--list-runtimes\n"
        val dotnetOutput = SourceDirectory.outputPath(compiledDotNet.program, dotnetLine, versions)
        assertEquals(dotnetArgs, s"$dotnetOutput/RecheckTest.dll")
      finally
        delete(Paths.get(javaArgs.stripPrefix("-cp ").stripSuffix(" RecheckTest")))
        delete(Paths.get(dotnetArgs).getParent)

  test("Executor#exec: a compiled program"):
    val program = SourceFile("RecheckTest.sh", s"#!/bin/sh\necho \"$$(cat)\" # ${System.nanoTime()}\n")
    withScript("my compiler", fakeCompiler): compiler =>
      val line = s"'$compiler'"
      try
        assertEquals(
          ICUExecutor(program).exec("input", None, Duration.Inf, "start", Map("RECHECK_ICU" -> line)),
          ExecResult.Exit(0, "input\n", "")
        )
      finally
        val args = Seq(line, "RecheckTest.sh", "-o", "RecheckTest") ++ ICUExecutor.Libraries
        delete(SourceDirectory.outputPath(program, args, "fake 1.0\n"))

  test("Executor#exec: the compilation does not count toward the timeout"):
    val program = SourceFile("RecheckTest.sh", s"#!/bin/sh\necho \"$$(cat)\" # ${System.nanoTime()}\n")
    withScript(
      "my compiler",
      "#!/bin/sh\n[ \"$1\" = --version ] || sleep 1\n" + fakeCompiler.stripPrefix("#!/bin/sh\n")
    ): compiler =>
      val line = s"'$compiler'"
      try
        val result = ICUExecutor(program).exec("input", None, Duration.Inf, "start", Map("RECHECK_ICU" -> line))(using
          Context(timeout = 500.millis)
        )
        assertEquals(result, ExecResult.Exit(0, "input\n", ""))
      finally
        val args = Seq(line, "RecheckTest.sh", "-o", "RecheckTest") ++ ICUExecutor.Libraries
        delete(SourceDirectory.outputPath(program, args, "fake 1.0\n"))
