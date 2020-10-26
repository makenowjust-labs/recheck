import java.nio.file.Path

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization := "codes.quine.labo"
ThisBuild / homepage := Some(url("https://github.com/MakeNowJust-Labo/redos"))
ThisBuild / licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))
ThisBuild / developers := List(
  Developer(
    "MakeNowJust",
    "TSUYUSATO Kitsune",
    "make.just.on@gmail.com",
    url("https://quine.codes/")
  )
)

ThisBuild / scalaVersion := "2.13.3"
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
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.4.3"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.14"

lazy val root = project
  .in(file("."))
  .settings(
    sonatypeProfileName := "codes.quine",
    publish / skip := true
  )
  .aggregate(coreJVM, coreJS, demoJS)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/redos-core"))
  .settings(
    name := "redos-core",
    console / initialCommands := """
      |import scala.concurrent.duration._
      |
      |import codes.quine.labo.redos._
      |import codes.quine.labo.redos.automaton._
      |import codes.quine.labo.redos.data._
      |import codes.quine.labo.redos.regexp._
      |import codes.quine.labo.redos.unicode._
      |import codes.quine.labo.redos.util._
      |
      |implicit val timeout = Timeout.NoTimeout
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
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.0",
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
          val dir = (Compile / sourceManaged).value / "codes" / "quine" / "labo" / "redos" / "unicode"
          val changes = generateUnicodeData.inputFileChanges
          val updatedPaths = changes.created ++ changes.modified
          for (path <- updatedPaths) {
            val fileName = path.getFileName.toString
            gens.get(fileName).foreach(_.gen(dir))
          }
          gens.map(_._2.file(dir)).toSeq
        }
      )
    },
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.14" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) })

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val demoJS = project
  .in(file("modules/demo"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    publish / skip := true,
    name := "redos-demo",
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("codes.quine.labo.redos.demo.DemoApp"),
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
  )
  .dependsOn(coreJS)
