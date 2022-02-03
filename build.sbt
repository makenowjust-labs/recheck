Global / onChangedBuildSource := ReloadOnSourceChanges
Global / excludeLintKeys += nativeImageVersion

ThisBuild / organization := "codes.quine.labs"
ThisBuild / homepage := Some(url("https://github.com/makenowjust-labs/recheck"))
ThisBuild / licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))
ThisBuild / developers := List(
  Developer(
    "makenowjust",
    "TSUYUSATO Kitsune",
    "make.just.on@gmail.com",
    url("https://quine.codes/")
  )
)

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Wunused",
  "-P:bm4:implicit-patterns:n"
)

// Scalafix config:
ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.21"

val releaseVersion = taskKey[String]("A release version")
ThisBuild / releaseVersion := {
  if (version.value.endsWith("-SNAPSHOT")) previousStableVersion.value.getOrElse("0.0.0")
  else version.value
}

lazy val root = project
  .in(file("."))
  .settings(
    sonatypeProfileName := "codes.quine",
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
    unicodeJVM,
    unicodeJS,
    parseJVM,
    parseJS,
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
      |import scala.concurrent.duration._
      |import scala.util.{Failure, Random, Success, Try}
      |
      |import codes.quine.labs.recheck._
      |import codes.quine.labs.recheck.automaton._
      |import codes.quine.labs.recheck.common._
      |import codes.quine.labs.recheck.exec._
      |import codes.quine.labs.recheck.data._
      |import codes.quine.labs.recheck.diagnostics._
      |import codes.quine.labs.recheck.fuzz._
      |import codes.quine.labs.recheck.recall._
      |import codes.quine.labs.recheck.regexp._
      |import codes.quine.labs.recheck.unicode._
      |import codes.quine.labs.recheck.util._
      |import codes.quine.labs.recheck.vm._
      |
      |def logger: Context.Logger = (message: String) => {
      |  val date = java.time.LocalDateTime.now()
      |  Console.out.println(s"[$date] $message")
      |}
      |
      |implicit def ctx: Context = Context(timeout = 10.seconds, logger = Some(logger))
      |
      |def time[A](name: String)(body: => A): A = {
      |  val start = System.nanoTime()
      |  try body
      |  finally {
      |    println(s"$name: ${(System.nanoTime() - start) / 1e9} s")
      |  }
      |}
      |
      |def parse(source: String, flags: String): Pattern =
      |  time("parse")(Parser.parse(source, flags) match {
      |    case Right(pattern) => pattern
      |    case Left(ex)       => throw new InvalidRegExpException(ex.getMessage)
      |  })
      |
      |def compile(source: String, flags: String): Program = {
      |  val pattern = parse(source, flags)
      |  time("compile")(ProgramBuilder.build(pattern).get)
      |}
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
      |    needsHeatmap: Boolean = false
      |)(implicit ctx: Context): Interpreter.Result = {
      |  val pattern = parse(source, flags)
      |  val flagSet = pattern.flagSet
      |  val program = time("compile")(ProgramBuilder.build(pattern).get)
      |  val uinput0 = UString(input)
      |  val uinput = if (flagSet.ignoreCase) UString.canonicalize(uinput0, flagSet.unicode) else uinput0
      |  val opts = Interpreter.Options(
      |    limit = limit,
      |    usesAcceleration = usesAcceleration,
      |    needsLoopAnalysis = needsLoopAnalysis,
      |    needsFailedPoints = needsFailedPoints,
      |    needsCoverage = needsCoverage,
      |    needsHeatmap = needsHeatmap
      |  )
      |  time("run")(Interpreter.run(program, uinput, 0, opts))
      |}
      |
      |def seed(
      |    source: String,
      |    flags: String,
      |    maxSimpleRepeatSize: Int = Parameters.MaxSimpleRepeatCount,
      |    maxInitialGenerationSize: Int = Parameters.MaxInitialGenerationSize,
      |    limit: Int = Parameters.IncubationLimit
      |)(implicit ctx: Context): Set[FString] = {
      |  val pattern = parse(source, flags)
      |  time("seed")(StaticSeeder.seed(pattern, maxSimpleRepeatSize, maxInitialGenerationSize, limit))
      |}
      |
      |def validate(
      |  source: String,
      |  flags: String,
      |  pattern: AttackPattern,
      |  timeout: Duration = Duration(1, SECONDS)
      |): RecallResult =
      |  time("validate")(RecallValidator.validate(source, flags, pattern, timeout)(NodeExecutor.exec))
      |
      |def check(
      |    source: String,
      |    flags: String,
      |    checker: Checker = Parameters.Checker,
      |    timeout: Duration = Parameters.Timeout,
      |    logger: Option[Context.Logger] = Parameters.Logger,
      |    maxAttackStringSize: Int = Parameters.MaxAttackStringSize,
      |    attackLimit: Int = Parameters.AttackLimit,
      |    randomSeed: Long = Parameters.RandomSeed,
      |    maxIteration: Int = Parameters.MaxIteration,
      |    seeder: Seeder = Parameters.Seeder,
      |    maxSimpleRepeatCount: Int = Parameters.MaxSimpleRepeatCount,
      |    seedingLimit: Int = Parameters.SeedingLimit,
      |    seedingTimeout: Duration = Parameters.SeedingTimeout,
      |    maxInitialGenerationSize: Int = Parameters.MaxInitialGenerationSize,
      |    incubationLimit: Int = Parameters.IncubationLimit,
      |    incubationTimeout: Duration = Parameters.IncubationTimeout,
      |    maxGeneStringSize: Int = Parameters.MaxGeneStringSize,
      |    maxGenerationSize: Int = Parameters.MaxGenerationSize,
      |    crossoverSize: Int = Parameters.CrossoverSize,
      |    mutationSize: Int = Parameters.MutationSize,
      |    attackTimeout: Duration = Parameters.AttackTimeout,
      |    maxDegree: Int = Parameters.MaxDegree,
      |    heatRatio: Double = Parameters.HeatRatio,
      |    accelerationMode: AccelerationMode = Parameters.AccelerationMode,
      |    maxRecallStringSize: Int = Parameters.MaxRecallStringSize,
      |    recallLimit: Int = Parameters.RecallLimit,
      |    recallTimeout: Duration = Parameters.RecallTimeout,
      |    maxRepeatCount: Int = Parameters.MaxRepeatCount,
      |    maxNFASize: Int = Parameters.MaxNFASize,
      |    maxPatternSize: Int = Parameters.MaxPatternSize,
      |): Diagnostics = {
      |  val params = Parameters(
      |        checker,
      |      timeout,
      |      logger,
      |      maxAttackStringSize,
      |      attackLimit,
      |      randomSeed,
      |      maxIteration,
      |      seeder,
      |      maxSimpleRepeatCount,
      |      seedingLimit,
      |      seedingTimeout,
      |      maxInitialGenerationSize,
      |      incubationLimit,
      |      incubationTimeout,
      |      maxGeneStringSize,
      |      maxGenerationSize,
      |      crossoverSize,
      |      mutationSize,
      |      attackTimeout,
      |      maxDegree,
      |      heatRatio,
      |      accelerationMode,
      |      maxRecallStringSize,
      |      recallLimit,
      |      recallTimeout,
      |      maxRepeatCount,
      |      maxNFASize,
      |      maxPatternSize
      |  )
      |  time("check")(ReDoS.check(source, flags, params))
      |}
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .dependsOn(common, exec, unicode, parse)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val common = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-common"))
  .settings(
    name := "recheck-common",
    console / initialCommands := """
      |import codes.quine.labs.recheck.common._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Dependencies:
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )

lazy val commonJVM = common.jvm
lazy val commonJS = common.js

lazy val exec = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-exec"))
  .settings(
    name := "recheck-exec",
    console / initialCommands := """
      |import codes.quine.labs.recheck.exec._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .dependsOn(common)

lazy val execJVM = exec.jvm
lazy val execJS = exec.js

lazy val unicode = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-unicode"))
  .settings(
    name := "recheck-unicode",
    console / initialCommands := """
      |import codes.quine.labs.recheck.unicode._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Generators:
    {
      val generateUnicodeData = taskKey[Seq[File]]("Generate Unicode data")
      Seq(
        Compile / sourceGenerators += generateUnicodeData.taskValue,
        generateUnicodeData / fileInputs += baseDirectory.value.toGlob / ".." / ".." / ".." / "project" / "*DataGen.scala",
        generateUnicodeData := {
          val gens = Map[String, UnicodeDataGen](
            "CaseMapDataGen.scala" -> CaseMapDataGen,
            "PropertyDataGen.scala" -> PropertyDataGen
          )
          val pkg = "codes.quine.labs.recheck.unicode"
          val dir = (Compile / sourceManaged).value / "codes" / "quine" / "labs" / "recheck" / "unicode"
          val changes = generateUnicodeData.inputFileChanges
          val updatedPaths = changes.created ++ changes.modified
          for (path <- updatedPaths) {
            val fileName = path.getFileName.toString
            gens.get(fileName).foreach(_.gen(pkg, dir))
          }
          gens.map(_._2.file(dir)).toSeq
        }
      )
    },
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

lazy val unicodeJVM = unicode.jvm
lazy val unicodeJS = unicode.js

lazy val parse = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-parse"))
  .settings(
    name := "recheck-parse",
    console / initialCommands := """
      |import codes.quine.labs.recheck.regexp._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.3",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .dependsOn(unicode)

lazy val parseJVM = parse.jvm
lazy val parseJS = parse.js

lazy val codec = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-codec"))
  .settings(
    name := "recheck-codec",
    console / initialCommands := """
      |import io.circe._
      |import io.circe.syntax._
      |
      |import codes.quine.labs.recheck.codec._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Dependencies:
    libraryDependencies += "io.circe" %%% "circe-core" % "0.14.1",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
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
      |import codes.quine.labs.recheck._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Dependencies:
    libraryDependencies += "io.circe" %%% "circe-scalajs" % "0.14.1",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    // ScalaJS config:
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .dependsOn(coreJS, codecJS)

lazy val cli = project
  .in(file("modules/recheck-cli"))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "recheck-cli",
    NativeImage / name := "recheck",
    publish / skip := true,
    Compile / mainClass := Some("codes.quine.labs.recheck.cli.Main"),
    assembly / mainClass := (Compile / mainClass).value,
    assembly / assemblyJarName := "recheck.jar",
    nativeImageVersion := "21.3.0",
    nativeImageOptions ++= List(
      "--no-fallback",
      "--initialize-at-build-time=java",
      "--initialize-at-build-time=scala",
      "--initialize-at-build-time=cats",
      "--initialize-at-build-time=io.circe",
      "--initialize-at-build-time=codes.quine.labs.recheck"
    ),
    console / initialCommands := """
      |import io.circe._
      |import io.circe.parser._
      |import io.circe.syntax._
      |
      |import codes.quine.labs.recheck.cli._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Dependencies:
    libraryDependencies += "com.monovore" %% "decline" % "2.2.0",
    libraryDependencies += "io.circe" %% "circe-core" % "0.14.1",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.14.1",
    libraryDependencies += "io.circe" %% "circe-parser" % "0.14.1",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .dependsOn(coreJVM, codecJVM)
