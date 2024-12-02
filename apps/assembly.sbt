assembly / mainClass := Some("org.clulab.processors.apps.CommandLineInterface")
assembly / assemblyJarName := "processors.jar"
assembly / assemblyMergeStrategy := {
  // See https://github.com/sbt/sbt-assembly.
  case PathList("javax", "xml", "bind", xs @ _*) => MergeStrategy.first
  case PathList("module-info.class")             => MergeStrategy.discard
  case other => // MergeStrategy.deduplicate
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(other)
}
assembly / test := {}
