name := "eidos-webapp"
description := "A web application providing a user interface to Eidos"

// This is because of a transitive dependency, apparently.
resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  // Versions were last checked 2021 Mar 12.
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test // up to 5.1.0
)
