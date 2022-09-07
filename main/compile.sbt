Compile / unmanagedSourceDirectories ++= {
  val sharedSourceDir = (ThisBuild / baseDirectory).value / "main/src/main"
  // println(s"sharedSourceDir $sharedSourceDir")
  val additionalDirs = CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11) | (2, 12)) => Seq(sharedSourceDir / "scala-2.11_2.12")
    case Some((2, 13)) => Seq.empty
    case _ => Seq.empty
  }
  // println(s"additionalDirs: $additionalDirs")
  additionalDirs
}
