import sbt.Keys._

enablePlugins(ScalaJSPlugin)

scalaJSUseRhino in Global := false

lazy val root = project.in(file(".")).
  aggregate(js, jvm).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val ostinato = crossProject.in(file(".")).
  settings (
    name := "ostinato",

    version := "0.1-SNAPSHOT",

    organization := "org.gappa",

    scalaVersion := "2.11.7",

    libraryDependencies ++= Seq("org.scalatest" %%% "scalatest" % "3.0.0-M12")
  ).
  jvmSettings(
    scalaVersion := "2.11.7",

    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http-experimental" % "2.0.3",
      "com.typesafe.akka" %% "akka-http-testkit-experimental" % "2.0.3"
    )
  ).
  jsSettings (
    scalaVersion := "2.11.7"
  )

lazy val js = ostinato.js

lazy val jvm = ostinato.jvm
