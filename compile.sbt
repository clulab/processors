// In sbt 1.6.2, the option below does not seem to work.  @throws have been removed.
// Suppresses problems with scaladoc @throws links.
// Compile / doc / scalacOptions += "-no-link-warnings"
ThisBuild / Compile / scalacOptions ++= Seq(
  "-feature", "-unchecked"
) ++ {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) => Seq.empty // The deprecation check is disabled.
    case Some((2, 12) | (2, 13)) => Seq("-deprecation")
    case _ => Seq.empty
  }
}
