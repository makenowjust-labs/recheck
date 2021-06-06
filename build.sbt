Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization := "codes.quine.labo"
ThisBuild / homepage := Some(url("https://github.com/MakeNowJust-Labo/recheck"))
ThisBuild / licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))
ThisBuild / developers := List(
  Developer(
    "MakeNowJust",
    "TSUYUSATO Kitsune",
    "make.just.on@gmail.com",
    url("https://quine.codes/")
  )
)

ThisBuild / scalaVersion := "2.13.6"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Wunused"
)

// Scalafix config:
ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.20"

lazy val root = project
  .in(file("."))
  .settings(
    sonatypeProfileName := "codes.quine",
    publish / skip := true,
    coverageEnabled := false,
    mdocVariables := Map(
      "VERSION" -> {
        if (version.value.endsWith("-SNAPSHOT")) previousStableVersion.value.getOrElse("0.0.0")
        else version.value
      }
    ),
    mdocOut := baseDirectory.value / "site" / "content"
  )
  .enablePlugins(MdocPlugin)
  .aggregate(recheckJVM, recheckJS, unicodeJVM, unicodeJS, parseJVM, parseJS, codecJVM, codecJS js, cli)
  .dependsOn(recheckJVM)

lazy val recheck = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck"))
  .settings(
    name := "recheck",
    console / initialCommands := """
      |import scala.concurrent.duration._
      |import scala.util.Random
      |
      |import codes.quine.labo.recheck._
      |import codes.quine.labo.recheck.automaton._
      |import codes.quine.labo.recheck.common._
      |import codes.quine.labo.recheck.data._
      |import codes.quine.labo.recheck.fuzz._
      |import codes.quine.labo.recheck.regexp._
      |import codes.quine.labo.recheck.unicode._
      |import codes.quine.labo.recheck.util._
      |import codes.quine.labo.recheck.vm._
      |
      |implicit def ctx: Context = Context(10.seconds)
      |
      |def time[A](body: => A): A = {
      |  val start = System.nanoTime()
      |  val result = body
      |  println(s"${(System.nanoTime() - start) / 1e9} s")
      |  result
      |}
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Add inline options:
    Compile / scalacOptions ++= {
      if ((ThisBuild / coverageEnabled).value) Seq.empty
      else
        Seq(
          "-opt:l:inline",
          "-opt-inline-from:codes.quine.labo.recheck.common.Context.**"
        )
    },
    // Settings for scaladoc:
    Compile / doc / scalacOptions += "-diagrams",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.2.7",
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.26" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .dependsOn(unicode, parse)

lazy val recheckJVM = recheck.jvm
lazy val recheckJS = recheck.js

lazy val unicode = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/recheck-unicode"))
  .settings(
    name := "recheck-unicode",
    console / initialCommands := """
      |import codes.quine.labo.recheck.unicode._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Settings for scaladoc:
    Compile / doc / scalacOptions += "-diagrams",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
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
          val pkg = "codes.quine.labo.recheck.unicode"
          val dir = (Compile / sourceManaged).value / "codes" / "quine" / "labo" / "recheck" / "unicode"
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
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.26" % Test,
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
      |import codes.quine.labo.recheck.regexp._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Settings for scaladoc:
    Compile / doc / scalacOptions += "-diagrams",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.2",
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.26" % Test,
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
      |import codes.quine.labo.recheck.codec._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Settings for scaladoc:
    Compile / doc / scalacOptions += "-diagrams",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += "io.circe" %% "circe-core" % "0.14.1",
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.26" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .dependsOn(recheck)

lazy val codecJVM = codec.jvm
lazy val codecJS = codec.js

lazy val js = project
  .in(file("modules/recheck-js"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "recheck-js",
    publish / skip := true,
    console / initialCommands := """
      |import codes.quine.labo.recheck._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Settings for scaladoc:
    Compile / doc / scalacOptions += "-diagrams",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.26" % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    // ScalaJS config:
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .dependsOn(recheckJS)

lazy val cli = project
  .in(file("modules/recheck-cli"))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "recheck-cli",
    NativeImage / name := "recheck",
    publish / skip := true,
    Compile / mainClass := Some("codes.quine.labo.recheck.cli.Main"),
    nativeImageOptions ++= List(
      "--no-fallback",
      "--initialize-at-build-time"
    ),
    console / initialCommands := """
      |import io.circe._
      |import io.circe.parser._
      |import io.circe.syntax._
      |
      |import codes.quine.labo.recheck.cli._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    // Dependencies:
    libraryDependencies += "com.monovore" %% "decline" % "2.0.0",
    libraryDependencies += "io.circe" %% "circe-core" % "0.14.1",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.14.1",
    libraryDependencies += "io.circe" %% "circe-parser" % "0.14.1",
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.26" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .dependsOn(recheckJVM)
