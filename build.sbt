import java.nio.file.Path

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / githubOwner := "MakeNowJust-Labo"
ThisBuild / githubRepository := "redos"

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
    organization := "codes.quine.labo",
    name := "redos",
    version := "0.1.0-SNAPSHOT",
    console / initialCommands := """
      |import codes.quine.labo.redos._
      |import codes.quine.labo.redos.automaton._
      |import codes.quine.labo.redos.data._
      |import codes.quine.labo.redos.regexp._
      |import codes.quine.labo.redos.unicode._
      |import codes.quine.labo.redos.util._
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
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1",
    // Generators:
    Compile / sourceGenerators += generateUnicodeData.taskValue,
    // Settings for test:
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.14" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )

val generateUnicodeData = taskKey[Seq[File]]("Generate Unicode data")
generateUnicodeData / fileInputs += baseDirectory.value.toGlob / "project" / "UniicodeDataGen.scala"
generateUnicodeData / fileInputs += baseDirectory.value.toGlob / "project" / "CaseMapDataGen.scala"
generateUnicodeData / fileInputs += baseDirectory.value.toGlob / "project" / "PropertyDataGen.scala"
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
