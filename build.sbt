import ReleaseTransformations._

lazy val commonSettings = Seq(
  organization := "org.clulab",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),
  parallelExecution in Test := false,
  scalacOptions in (Compile, doc) += "-no-link-warnings", // suppresses problems with scaladoc @throws links

  //
  // publishing settings
  //
  // publish to a maven repo
  publishMavenStyle := true,

  // the standard maven repository
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },

  // let’s remove any repositories for optional dependencies in our artifact
  pomIncludeRepository := { _ => false },

  // mandatory stuff to add to the pom for publishing
  pomExtra :=
    <url>https://github.com/clulab/processors</url>
    <licenses>
      <license>
        <name>Apache License, Version 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/clulab/processors</url>
      <connection>https://github.com/clulab/processors</connection>
    </scm>
    <developers>
      <developer>
        <id>mihai.surdeanu</id>
        <name>Mihai Surdeanu</name>
        <email>mihai@surdeanu.info</email>
      </developer>
    </developers>

  //
  // end publishing settings
  //
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false,
    publishTo := Some("dummy" at "nowhere"),
    publish := {},
    publishLocal := {},
    publishM2 := {},
    Keys.`package` := {
      // avoid generating an empty jar for the root project
      (Keys.`package` in (main, Compile)).value
    }
  )
  .aggregate(main, corenlp, models)
  .dependsOn(main, corenlp, models) // so that we can import from the console

lazy val main = project
  .settings(commonSettings: _*)

lazy val corenlp = project
  .settings(commonSettings: _*)
  .dependsOn(main)

lazy val models = project
  .settings(commonSettings: _*)

// release steps
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _)),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
  pushChanges
)
