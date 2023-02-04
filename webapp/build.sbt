name := "processors-webapp"
description := "A web application providing a user interface to processors"

libraryDependencies ++= Seq(
  // Versions were last checked 2023 Jan 31.
  guice,
  // Newer than 4.0.3 does not work for Scala 2.11.  There is no Scala 3 version.
  "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.3" % Test // up to 5.1.0
)

// In general, we do not want to include application.conf in something
// like the published jar file.  This is just a reminder.
Compile / packageBin / mappings := {
  val filtered = (Compile / packageBin / mappings).value.filter {
    case (file, name) =>
      name != "application.conf"
  }

  filtered
}

PlayKeys.devSettings += "config.resource" -> "processors.conf"
