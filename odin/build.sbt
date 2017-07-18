name := "processors-odin"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.yaml" % "snakeyaml" % "1.14",
  "jline" % "jline" % "2.12.1",
  "commons-io" % "commons-io" % "2.5"
)
