import sbt.Keys._

name := "ostinato"

version := "0.1-SNAPSHOT"

enablePlugins(ScalaJSPlugin)

scalaJSUseRhino in Global := false

val akkaVersion = "2.4.2"


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
      "com.typesafe.akka" %% "akka-http-experimental" % akkaVersion,
      "com.typesafe.akka" %% "akka-http-testkit" % akkaVersion % "test",
      "com.typesafe.akka" % "akka-http-jackson-experimental_2.11" % akkaVersion,
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.7.2"
    )
  ).
  jsSettings (
    scalaVersion := "2.11.7"
  )

lazy val js = ostinato.js

lazy val jvm = ostinato.jvm

// https://github.com/xerial/sbt-pack
packAutoSettings
