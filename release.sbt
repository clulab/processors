import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommandAndRemaining("sonaUpload"),
  setNextVersion,
  commitNextVersion,
//  pushChanges
)

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / useGpg := false // GPG doesn't need to be installed, particularly for Windows.
