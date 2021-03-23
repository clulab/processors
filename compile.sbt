ThisBuild / Compile / doc / scalacOptions += "-no-link-warnings" // Suppresses problems with scaladoc @throws links.
ThisBuild / Compile / scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")
