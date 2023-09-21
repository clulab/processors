name := "processors-webapp"
description := "A web application providing a user interface to processors"

libraryDependencies ++= Seq(
  // Versions were last checked 2023 Jan 31.
  guice,
  // Newer than 4.0.3 does not work for Scala 2.11.  There is no Scala 3 version.
  // See https://github.com/playframework/scalatestplus-play#releases.
  // For play 2.8.19, need scalatestplus-play 5.1.0 and Scalatest 3.1.x.
  // So, if we test, then we rule out Scala 2.11.
  "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0"  % Test // up to 5.1.0
)

// In general, we do not want to include routes or application.conf in
// something like the published jar file.  This is just a reminder.
Compile / packageBin / mappings := {
  val filtered = (Compile / packageBin / mappings).value.filter {
    case (file, name) =>
      name != "application.conf" &&
      name != "routes"
  }

  filtered
}

PlayKeys.devSettings += "config.resource" -> "processors.conf"
