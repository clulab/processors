name := "processors-webapp2"
description := "A web application providing a user interface to processors"

libraryDependencies ++= {
  val http4sVersion = "0.23.26"
  val json4sVersion = "4.0.6"

  Seq(
    "org.clulab"              % "processors-model"    % "0.3.1",

    "org.http4s"             %% "http4s-ember-server" % http4sVersion,
    "org.http4s"             %% "http4s-dsl"          % http4sVersion,
    "org.http4s"             %% "http4s-circe"        % http4sVersion,

    "org.json4s"             %% "json4s-core"         % json4sVersion,
    "org.json4s"             %% "json4s-jackson"      % json4sVersion,

    "org.scala-lang.modules" %% "scala-xml"           % "2.2.0",

    "ch.qos.logback"          % "logback-classic"    % "1.2.8"
  )
}

Compile / run / fork := true
