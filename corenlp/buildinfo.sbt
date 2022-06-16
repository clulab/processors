enablePlugins(BuildInfoPlugin)

buildInfoKeys := {
  val stanfordVersion = {
    val Array(major, minor, revision) = libraryDependencies.value
      .find(_.name == "stanford-corenlp")
      .map(_.revision)
      .get          // It must exist
      .split('.')   // and be formatted
      .map(_.toInt) // compatibly!

    Map(
      "major" -> major,
      "minor" -> minor,
      "revision" -> revision
    )
  }

  Seq[BuildInfoKey](
    "stanfordVersion" -> stanfordVersion
  )
}
buildInfoPackage := "org.clulab.processors.corenlp"
