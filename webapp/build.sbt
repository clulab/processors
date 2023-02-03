name := "processors-webapp"
description := "A web application providing a user interface to processors"

libraryDependencies ++= Seq(
  // Versions were last checked 2023 Jan 31.
  guice,
  // Newer than 4.0.3 does not work for Scala 2.11.  There is no Scala 3 version.
  "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.3" % Test // up to 5.1.0
) // .map(_.cross(CrossVersion.for3Use2_13))

Compile / packageBin / mappings := {
  val filtered = (Compile / packageBin / mappings).value.filter {
    case (file, name) =>
      val filter = false ||
          name == "routes" ||
          name == "application.conf" ||
          name.startsWith("router") ||
          name.startsWith("controllers") ||
          name.endsWith("ReverseHomeController.class") ||
          name.endsWith("routes.class") ||
          name.endsWith("routes$javascript.class")

      !filter
  }

  filtered
}
