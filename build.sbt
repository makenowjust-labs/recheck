Global / onChangedBuildSource := ReloadOnSourceChanges
Global / excludeLintKeys += nativeImageVersion
Global / excludeLintKeys += nativeImageJvm

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
ThisBuild / versionScheme := Some("early-semver")

ThisBuild / scalaVersion := "2.13.11"
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
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.23"

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
      |    maxSimpleRepeatSize: Int = Parameters.DefaultMaxSimpleRepeatCount,
      |    maxInitialGenerationSize: Int = Parameters.DefaultMaxInitialGenerationSize,
      |    limit: Int = Parameters.DefaultIncubationLimit
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
      |    accelerationMode: AccelerationMode = Parameters.DefaultAccelerationMode,
      |    attackLimit: Int = Parameters.DefaultAttackLimit,
      |    attackTimeout: Duration = Parameters.DefaultAttackTimeout,
      |    checker: Checker = Parameters.DefaultChecker,
      |    crossoverSize: Int = Parameters.DefaultCrossoverSize,
      |    heatRatio: Double = Parameters.DefaultHeatRatio,
      |    incubationLimit: Int = Parameters.DefaultIncubationLimit,
      |    incubationTimeout: Duration = Parameters.DefaultIncubationTimeout,
      |    logger: Option[Context.Logger] = Parameters.DefaultLogger,
      |    maxAttackStringSize: Int = Parameters.DefaultMaxAttackStringSize,
      |    maxDegree: Int = Parameters.DefaultMaxDegree,
      |    maxGeneStringSize: Int = Parameters.DefaultMaxGeneStringSize,
      |    maxGenerationSize: Int = Parameters.DefaultMaxGenerationSize,
      |    maxInitialGenerationSize: Int = Parameters.DefaultMaxInitialGenerationSize,
      |    maxIteration: Int = Parameters.DefaultMaxIteration,
      |    maxNFASize: Int = Parameters.DefaultMaxNFASize,
      |    maxPatternSize: Int = Parameters.DefaultMaxPatternSize,
      |    maxRecallStringSize: Int = Parameters.DefaultMaxRecallStringSize,
      |    maxRepeatCount: Int = Parameters.DefaultMaxRepeatCount,
      |    maxSimpleRepeatCount: Int = Parameters.DefaultMaxSimpleRepeatCount,
      |    mutationSize: Int = Parameters.DefaultMutationSize,
      |    randomSeed: Long = Parameters.DefaultRandomSeed,
      |    recallLimit: Int = Parameters.DefaultRecallLimit,
      |    recallTimeout: Duration = Parameters.DefaultRecallTimeout,
      |    seeder: Seeder = Parameters.DefaultSeeder,
      |    seedingLimit: Int = Parameters.DefaultSeedingLimit,
      |    seedingTimeout: Duration = Parameters.DefaultSeedingTimeout,
      |    timeout: Duration = Parameters.DefaultTimeout,
      |): Diagnostics = {
      |  val params = Parameters(
      |    accelerationMode,
      |    attackLimit,
      |    attackTimeout,
      |    checker,
      |    crossoverSize,
      |    heatRatio,
      |    incubationLimit,
      |    incubationTimeout,
      |    logger,
      |    maxAttackStringSize,
      |    maxDegree,
      |    maxGeneStringSize,
      |    maxGenerationSize,
      |    maxInitialGenerationSize,
      |    maxIteration,
      |    maxNFASize,
      |    maxPatternSize,
      |    maxRecallStringSize,
      |    maxRepeatCount,
      |    maxSimpleRepeatCount,
      |    mutationSize,
      |    randomSeed,
      |    recallLimit,
      |    recallTimeout,
      |    seeder,
      |    seedingLimit,
      |    seedingTimeout,
      |    timeout,
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
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
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
  .jsSettings(
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
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
  .jsSettings(
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
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
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
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
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.0.2",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13)
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
    libraryDependencies += "io.circe" %%% "circe-core" % "0.14.6",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % Test)
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
      |import codes.quine.labs.recheck._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Dependencies:
    libraryDependencies += "io.circe" %%% "circe-scalajs" % "0.14.5",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % Test)
      .cross(CrossVersion.for3Use2_13),
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
    nativeImageVersion := "22.3.0",
    nativeImageJvm := "graalvm-java17",
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
    libraryDependencies += "com.monovore" %% "decline" % "2.4.1",
    libraryDependencies += "io.circe" %% "circe-core" % "0.14.6",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.14.6",
    libraryDependencies += "io.circe" %% "circe-parser" % "0.14.6",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .dependsOn(coreJVM, codecJVM)
