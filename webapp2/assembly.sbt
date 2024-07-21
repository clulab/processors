assembly / mainClass := Some("org.clulab.processors.webapp.WebApp")
assembly / assemblyJarName := "processors-webapp.jar"
assembly / assemblyMergeStrategy := {
  // See https://github.com/sbt/sbt-assembly.
  case PathList("module-info.class")                              => MergeStrategy.discard
  case PathList("javax", "xml", "bind", xs @ _*)                  => MergeStrategy.first
  case other => // MergeStrategy.deduplicate
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(other)
}
// This prevents testing in core, then non-aggregation prevents it in other subprojects.
assembly / test := {}
