// These were last checked on 2025-02-19.
val scala211 = "2.11.12" // up to 2.11.12
val scala212 = "2.12.18" // up to 2.12.20
val scala213 = "2.13.11" // up to 2.13.16
val scala30  = "3.0.2"   // up to 3.0.2
val scala31  = "3.1.3"   // up to 3.1.3
val scala32  = "3.2.2"   // up to 3.2.2
val scala33  = "3.3.5"   // up to 3.3.5 (LTS)
val scala34  = "3.4.3"   // up to 3.4.3
val scala35  = "3.5.2"   // up to 3.5.2
val scala36  = "3.6.3"   // up to 3.6.3

// See https://www.scala-lang.org/blog/2022/08/17/long-term-compatibility-plans.html.
// Scala30: "If you are maintaining a library, you should drop Scala 3.0."  Dropped.
// Scala31: This is a LTS (long term support) version before it was called that.
// Scala32: This is for experimentation, as in Scala Next, and not for release.
// Scala33: This is the first official LTS, but hold off until necessary.
val scala3 = scala31

ThisBuild / crossScalaVersions := Seq(scala212, scala211, scala213, scala3)
ThisBuild / scalaVersion := crossScalaVersions.value.head

lazy val root = (project in file("."))
    .aggregate(main, corenlp, openie, debugger)
    .dependsOn(main, corenlp, openie) // so that we can import from the console
    .settings(
      publish / skip := true,
    )

lazy val main = project

lazy val corenlp = project
    .dependsOn(main % "compile -> compile; test -> test")

lazy val openie = project
    .dependsOn(main % "compile -> compile; test -> test")

lazy val webapp = project
    .enablePlugins(PlayScala)
    .dependsOn(main % "compile -> compile; test -> test")
    .settings(
      // scala3 doesn't have play and is ruled out completely.
      // scala213 dies at runtime thinking it needs something from scala11.
      // scala212 works!
      // scala211 isn't compiling and complains on twirlCompileTemplates.
      crossScalaVersions := Seq(scala212)
    )

lazy val debugger = project
    .dependsOn(main  % "compile -> compile; test -> test")

addCommandAlias("dockerizeWebapp", ";webapp/docker:publishLocal")
