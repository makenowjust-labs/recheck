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

ThisBuild / scalaVersion := "2.13.5"
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
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.18"

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
  .aggregate(recheckJVM, recheckJS)
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
      |import codes.quine.labo.recheck.backtrack._
      |import codes.quine.labo.recheck.common._
      |import codes.quine.labo.recheck.data._
      |import codes.quine.labo.recheck.fuzz._
      |import codes.quine.labo.recheck.regexp._
      |import codes.quine.labo.recheck.util._
      |
      |implicit def ctx: Context = Context()
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
    Compile / scalacOptions ++= Seq(
      "-opt:l:inline",
      "-opt-inline-from:codes.quine.labo.recheck.**"
    ),
    // Settings for scaladoc:
    Compile / doc / scalacOptions += "-diagrams",
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.1",
    libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.2.4",
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
          val dir = (Compile / sourceManaged).value / "codes" / "quine" / "labo" / "recheck" / "data" / "unicode"
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
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.22" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
  .jsSettings(
    coverageEnabled := false,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

lazy val recheckJVM = recheck.jvm
lazy val recheckJS = recheck.js
