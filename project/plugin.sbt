addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.5")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.33")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.7.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.10")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.24")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.2")

// https://github.com/scala-js/scala-js-js-envs/issues/12#issuecomment-958925883
libraryDependencies += "org.scala-js" %% "scalajs-env-nodejs" % "1.2.1"
