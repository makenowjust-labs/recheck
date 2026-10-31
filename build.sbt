Global / onChangedBuildSource := ReloadOnSourceChanges
Global / excludeLintKeys += nativeImageVersion
Global / excludeLintKeys += nativeImageJvm

ThisBuild / organization := "codes.quine.labs"
ThisBuild / homepage := Some(uri("https://github.com/makenowjust-labs/recheck"))
ThisBuild / licenses := Seq(License.MIT)
ThisBuild / developers := List(
  Developer(
    "makenowjust",
    "TSUYUSATO Kitsune",
    "make.just.on@gmail.com",
    uri("https://quine.codes/")
  )
)
ThisBuild / versionScheme := Some("early-semver")

ThisBuild / scalaVersion := "3.10.0"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Wunused:all"
)

// Scalafix config:
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

val releaseVersion = taskKey[String]("A release version")
ThisBuild / releaseVersion := {
  if (version.value.endsWith("-SNAPSHOT")) previousStableVersion.value.getOrElse("0.0.0")
  else version.value
}

val generateParameters = taskKey[Seq[File]]("Generate the Scala sources of the parameters from the schema")

def parametersGenerator(target: ParametersGen.Target): Seq[Def.Setting[?]] = Seq(
  Compile / sourceGenerators += generateParameters.taskValue,
  generateParameters := Def.uncached {
    Seq(ParametersGen.generate(target, (ThisBuild / baseDirectory).value, (Compile / sourceManaged).value))
  },
  coverageExcludedFiles := ".*[/\\\\]src_managed[/\\\\].*"
)

val generateParametersSchema = taskKey[Seq[File]]("Generate the Scala test source of the schema's dialect annotations")

def parametersSchemaGenerator: Seq[Def.Setting[?]] = Seq(
  Test / sourceGenerators += generateParametersSchema.taskValue,
  generateParametersSchema := Def.uncached {
    Seq(
      ParametersGen.generate(
        ParametersGen.Target.SchemaTest,
        (ThisBuild / baseDirectory).value,
        (Test / sourceManaged).value
      )
    )
  }
)

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    coverageEnabled := false
  )
  .aggregate(
    coreJVM,
    coreJS,
    commonJVM,
    commonJS,
    execJVM,
    execJS,
    resyntaxJVM,
    resyntaxJS,
    resyntaxCharsetJVM,
    resyntaxCharsetJS,
    codecJVM,
    codecJS,
    js,
    cli
  )

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-core"))
  .settings(
    name := "recheck-core",
    console / initialCommands := """
      |import scala.concurrent.duration.*
      |import scala.util.{Failure, Random, Success, Try}
      |
      |import codes.quine.labs.recheck.*
      |import codes.quine.labs.recheck.automaton.*
      |import codes.quine.labs.recheck.common.*
      |import codes.quine.labs.recheck.exec.*
      |import codes.quine.labs.recheck.data.*
      |import codes.quine.labs.recheck.diagnostics.*
      |import codes.quine.labs.recheck.fuzz.*
      |import codes.quine.labs.recheck.recall.*
      |import codes.quine.labs.recheck.regexp.*
      |import codes.quine.labs.recheck.util.*
      |import codes.quine.labs.recheck.vm.*
      |import codes.quine.labs.resyntax.ast.Dialect
      |import codes.quine.labs.resyntax.ast.PerlSource
      |import codes.quine.labs.resyntax.charset.*
      |
      |def logger: Context.Logger = (message: String) =>
      |  val date = java.time.LocalDateTime.now()
      |  Console.out.println(s"[$date] $message")
      |
      |given ctx: Context = Context(timeout = 10.seconds, logger = Some(logger))
      |
      |def time[A](name: String)(body: => A): A =
      |  val start = System.nanoTime()
      |  try body
      |  finally
      |    println(s"$name: ${(System.nanoTime() - start) / 1e9} s")
      |
      |def parse(source: String, flags: String, dialect: Dialect = Dialect.JavaScript): ParsedPattern =
      |  time("parse")(ParsedPattern.parse(source, flags, dialect).toTry.get)
      |
      |def compile(source: String, flags: String, dialect: Dialect = Dialect.JavaScript): Program =
      |  val pattern = parse(source, flags, dialect)
      |  time("compile")(ProgramBuilder.build(pattern).get)
      |
      |def run(
      |    source: String,
      |    flags: String,
      |    input: String,
      |    pos: Int = 0,
      |    limit: Int = Int.MaxValue,
      |    usesAcceleration: Boolean = true,
      |    needsLoopAnalysis: Boolean = false,
      |    needsFailedPoints: Boolean = false,
      |    needsCoverage: Boolean = false,
      |    needsHeatmap: Boolean = false,
      |    dialect: Dialect = Dialect.JavaScript
      |)(using ctx: Context): Interpreter.Result =
      |  val pattern = parse(source, flags, dialect)
      |  val program = time("compile")(ProgramBuilder.build(pattern).get)
      |  val uinput = UStringUtil.fromString(input)
      |  val opts = Interpreter.Options(
      |    limit = limit,
      |    usesAcceleration = usesAcceleration,
      |    needsLoopAnalysis = needsLoopAnalysis,
      |    needsFailedPoints = needsFailedPoints,
      |    needsCoverage = needsCoverage,
      |    needsHeatmap = needsHeatmap
      |  )
      |  time("run")(Interpreter.run(program, uinput, pos, opts))
      |
      |def seed(
      |    source: String,
      |    flags: String,
      |    maxSimpleRepeatSize: Int = Parameters.DefaultMaxSimpleRepeatCount,
      |    maxInitialGenerationSize: Int = Parameters.DefaultMaxInitialGenerationSize,
      |    limit: Int = Parameters.DefaultIncubationLimit
      |)(using ctx: Context): Set[FString] =
      |  val pattern = parse(source, flags)
      |  time("seed")(StaticSeeder.seed(pattern, maxSimpleRepeatSize, maxInitialGenerationSize, limit))
      |
      |def validate(
      |  source: String,
      |  flags: String,
      |  pattern: AttackPattern,
      |  timeout: Duration = Duration(1, SECONDS)
      |)(using ctx: Context): Option[RecallValidation] =
      |  time("validate")(RecallValidator.validate(parse(source, flags), pattern, timeout, env = sys.env)(RecallValidator.defaultExec.get))
      |
      |""".stripMargin + ParametersGen.consoleCheck((ThisBuild / baseDirectory).value),
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    coverageEnabled := false,
    libraryDependencies += ("org.scala-js" %% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
  )
  .dependsOn(common, exec, resyntaxCharset, resyntax)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val common = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-common"))
  .settings(
    name := "recheck-common",
    parametersGenerator(ParametersGen.Target.Parameters),
    parametersSchemaGenerator,
    console / initialCommands := """
      |import codes.quine.labs.recheck.common.*
      |""".stripMargin,
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    // `coverage` is not supported in Scala.js with Scala 3 yet.
    // See https://github.com/scoverage/sbt-scoverage/issues/629.
    coverageEnabled := false,
    libraryDependencies += ("org.scala-js" %% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
  )
  .dependsOn(resyntax)

lazy val commonJVM = common.jvm
lazy val commonJS = common.js

lazy val exec = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-exec"))
  .settings(
    name := "recheck-exec",
    console / initialCommands := """
      |import codes.quine.labs.recheck.exec._
      |""".stripMargin,
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    coverageEnabled := false,
    libraryDependencies += ("org.scala-js" %% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
  )
  .dependsOn(common)

lazy val execJVM = exec.jvm
lazy val execJS = exec.js

lazy val resyntax = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/resyntax"))
  .dependsOn(resyntaxCharset)
  .settings(
    name := "resyntax",
    console / initialCommands := """
      |import codes.quine.labs.resyntax.*
      |import codes.quine.labs.resyntax.ast.*
      |import codes.quine.labs.resyntax.ir.*
      |import codes.quine.labs.resyntax.parser.*
      |""".stripMargin,
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jvmSettings(
    fork := true
  )
  .jsSettings(
    coverageEnabled := false,
    libraryDependencies += ("org.scala-js" %% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
  )

lazy val resyntaxJVM = resyntax.jvm
lazy val resyntaxJS = resyntax.js

lazy val resyntaxCharset = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/resyntax-charset"))
  .settings(
    name := "resyntax-charset",
    console / initialCommands := """
      |import codes.quine.labs.resyntax.charset.*
      |""".stripMargin,
    coverageExcludedFiles := ".*[/\\\\]src_managed[/\\\\].*",
    // Generators:
    {
      val generateUnicodeData = taskKey[Seq[File]]("Generate Unicode data")
      Seq(
        Compile / sourceGenerators += generateUnicodeData.taskValue,
        generateUnicodeData := Def.uncached {
          val gens = Map[String, UnicodeDataGen](
            "CharsetCaseDataGen.scala" -> CharsetCaseDataGen,
            "CharsetPropertyDataGen.scala" -> CharsetPropertyDataGen
          )
          val pkg = "codes.quine.labs.resyntax.charset"
          val dir = (Compile / sourceManaged).value / "codes" / "quine" / "labs" / "resyntax" / "charset"
          val projectDir = (ThisBuild / baseDirectory).value / "project"
          for ((fileName, gen) <- gens) {
            val output = gen.file(dir)
            val inputs = Seq(projectDir / fileName, projectDir / "CharsetEncoder.scala")
            if (!output.exists() || inputs.exists(_.lastModified() > output.lastModified())) gen.gen(pkg, dir)
          }
          gens.values.map(_.file(dir)).toSeq
        }
      )
    },
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    coverageEnabled := false,
    libraryDependencies += ("org.scala-js" %% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
  )

lazy val resyntaxCharsetJVM = resyntaxCharset.jvm
lazy val resyntaxCharsetJS = resyntaxCharset.js

lazy val codec = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-codec"))
  .settings(
    name := "recheck-codec",
    parametersGenerator(ParametersGen.Target.Codec),
    console / initialCommands := """
      |import io.circe.*
      |import io.circe.syntax.*
      |
      |import codes.quine.labs.recheck.codec.{*, given}
      |""".stripMargin,
    // Dependencies:
    libraryDependencies += "io.circe" %% "circe-core" % "0.14.17",
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    coverageEnabled := false,
    libraryDependencies += ("org.scala-js" %% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
  )
  .dependsOn(core)

lazy val codecJVM = codec.jvm
lazy val codecJS = codec.js

lazy val js = project
  .in(file("modules/recheck-js"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "recheck-js",
    publish / skip := true,
    console / initialCommands := """
      |import codes.quine.labs.recheck.*
      |""".stripMargin,
    // Dependencies:
    libraryDependencies += "io.circe" %% "circe-scalajs" % "0.14.17",
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    libraryDependencies += ("org.scala-js" %% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13),
    testFrameworks += new TestFramework("munit.Framework"),
    // ScalaJS config:
    coverageEnabled := false,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    // Output `recheck.js` to a path independent of the Scala version.
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := (ThisBuild / baseDirectory).value / "target" / "recheck-js-opt"
  )
  .dependsOn(coreJS, codecJS)

lazy val cli = project
  .in(file("modules/recheck-cli"))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "recheck-cli",
    parametersGenerator(ParametersGen.Target.Cli),
    NativeImage / name := "recheck",
    publish / skip := true,
    Compile / mainClass := Some("codes.quine.labs.recheck.cli.Main"),
    assembly / mainClass := (Compile / mainClass).value,
    assembly / assemblyJarName := "recheck.jar",
    // Output `recheck.jar` to a path independent of the Scala version.
    assembly / assemblyOutputPath := (ThisBuild / baseDirectory).value / "target" / (assembly / assemblyJarName).value,
    NativeImage / target := (ThisBuild / baseDirectory).value / "target" / "native-image",
    nativeImageJvm := "graalvm-community",
    nativeImageVersion := "25.0.1",
    nativeImageOptions += "--no-fallback",
    // `scala.runtime.LazyVals` uses `sun.misc.Unsafe`, and JDK 24+ warns on its first use (JEP 471).
    nativeImageOptions += "-Dsun.misc.unsafe.memory.access=allow",
    // The minimum macOS version of the binary is the build machine's version by default.
    nativeImageOptions ++= {
      if (scala.util.Properties.isMac)
        Seq("-H:+UnlockExperimentalVMOptions", "-H:NativeLinkerOption=-mmacosx-version-min=15.0")
      else Seq.empty
    },
    console / initialCommands := """
      |import io.circe.*
      |import io.circe.parser.*
      |import io.circe.syntax.*
      |
      |import codes.quine.labs.recheck.cli.*
      |""".stripMargin,
    // Dependencies:
    libraryDependencies += "com.monovore" %% "decline" % "2.6.2",
    libraryDependencies += "io.circe" %% "circe-core" % "0.14.17",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.14.17",
    libraryDependencies += "io.circe" %% "circe-parser" % "0.14.17",
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .dependsOn(coreJVM, codecJVM)
