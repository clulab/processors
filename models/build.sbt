name := "models"

version := Common.version

exportJars := true

exportJars in Test := false

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  s"${Common.name}-${module.revision}-${module.name}.${artifact.extension}"
}
