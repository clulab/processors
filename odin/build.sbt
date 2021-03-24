name := "processors-odin"
description := "processors-odin"

libraryDependencies ++= Seq(
  "org.apache.commons"      % "commons-text"             % "1.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang"          % "scala-reflect"            % scalaVersion.value,
  "org.yaml"                % "snakeyaml"                % "1.14"
)
