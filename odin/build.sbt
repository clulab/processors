name := "processors-odin"
description := "processors-odin"

libraryDependencies ++= Seq(
  "org.scala-lang"           % "scala-reflect"              % scalaVersion.value,
  "org.scala-lang.modules"  %%  "scala-parser-combinators"  % "1.0.4",
  "org.apache.commons"       %  "commons-text"              % "1.1",
  "org.yaml"                 %  "snakeyaml"                 % "1.14"
)
