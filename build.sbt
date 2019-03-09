import sbt.Keys._

name := "ostinato"

version := "1.0.2"

enablePlugins(ScalaJSPlugin)
enablePlugins(PackPlugin)

val akkaVersion = "10.0.4"


lazy val root = project.in(file(".")).
  aggregate(js, jvm).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val ostinato = crossProject.in(file(".")).
  settings (
    name := "ostinato",

    version := "1.0.2",

    organization := "org.gappa",

    scalaVersion := "2.12.1",

    libraryDependencies ++= Seq("org.scalatest" %%% "scalatest" % "3.0.1")
  ).
  jvmSettings(
    scalaVersion := "2.12.1",

    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % akkaVersion,
      "com.typesafe.akka" %% "akka-http-testkit" % akkaVersion % "test",
      "com.typesafe.akka" %% "akka-http-spray-json" % akkaVersion
    )
  ).
  jsSettings (
    scalaVersion := "2.12.1"
  )

lazy val js = ostinato.js

lazy val jvm = ostinato.jvm

// https://github.com/xerial/sbt-pack
// TODO re-enable this
//packAutoSettings
