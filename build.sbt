val scala211 = "2.11.12" // up to 2.11.12
val scala212 = "2.12.17" // up to 2.12.17
val scala213 = "2.13.10" // up to 2.13.10
val scala30  = "3.0.2"   // up to 3.0.2
val scala31  = "3.1.3"   // up to 3.1.3
val scala32  = "3.2.1"   // up to 3.2.1

// See https://www.scala-lang.org/blog/2022/08/17/long-term-compatibility-plans.html.
// Scala30: "If you are maintaining a library, you should drop Scala 3.0."  Dropped.
// Scala31: This is the current LTS (long term support) version and default Scala 3 release.
// Scala32: This is for experimentation, as in Scala Next, and not for release.
ThisBuild / crossScalaVersions := Seq(scala212, scala211, scala213, scala31) // , scala32) // , scala30)++
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
