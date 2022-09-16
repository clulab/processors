val scala211 = "2.11.12" // up to 2.11.12
val scala212 = "2.12.16" // up to 2.12.16
val scala213 = "2.13.8"  // up to 2.13.8
val scala30  = "3.0.2"   // up to 3.0.2
val scala31  = "3.1.3"   // up to 3.1.3

ThisBuild / crossScalaVersions := Seq(scala212, scala211, scala213)
ThisBuild / scalaVersion := crossScalaVersions.value.head

lazy val root = (project in file("."))
  .aggregate(main, corenlp, openie)
  .dependsOn(main, corenlp, openie) // so that we can import from the console
  .settings(
    publish / skip := true
  )

lazy val main = project

lazy val corenlp = project
  .dependsOn(main % "compile -> compile; test -> test")

lazy val openie = project
  .dependsOn(main % "compile -> compile; test -> test")
