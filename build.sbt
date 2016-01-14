name := "ostinato"

version := "0.1-SNAPSHOT"

organization := "org.gappa"

scalaVersion := "2.11.7"

//enablePlugins(ScalaJSPlugin)
//
//val library = crossProject.settings(
libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test"
)
//).jsSettings(
//  scalaVersion := "2.11.7"
//   /*JS-specific settings here*/
//).jvmSettings(
//  scalaVersion := "2.11.7"
//   /*JVM-specific settings here*/
//)
//
//lazy val js = library.js
//
//lazy val jvm = library.jvm
//