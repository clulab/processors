// In sbt 1.6.2, the option below does not seem to work.  @throws have been removed.
// Suppresses problems with scaladoc @throws links.
// Compile / doc / scalacOptions += "-no-link-warnings"
ThisBuild / Compile / scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")
