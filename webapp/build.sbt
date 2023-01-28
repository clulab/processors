name := "processors-webapp"
description := "A web application providing a user interface to processors"

libraryDependencies ++= Seq(
  // Versions were last checked 2021 Mar 12.
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test // up to 5.1.0
)
