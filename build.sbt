import ReleaseTransformations._

name := "processors"

lazy val commonSettings = Seq(
  organization := "org.clulab",

  scalaVersion := "2.11.8",

  scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),

  parallelExecution in Test := false,

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

lazy val main = (project in file("main"))
  .settings (
    commonSettings: _*
  )

lazy val corenlp = (project in file("corenlp"))
  .dependsOn (main)
  .settings (
    commonSettings: _*
  )

lazy val models = (project in file("models"))
  .settings (
    commonSettings: _*
  )
  .settings(
    publish := {},
    publishLocal := {},
    publishM2 := {}
  )

addArtifact(Artifact("processors", "models"), modelsTask in models)

unmanagedJars in Compile += (modelsTask in models).value

unmanagedClasspath in Runtime += baseDirectory.value

lazy val common = (project in file("."))
  .settings (
    publishArtifact := false,
    publishTo := Some("dummy" at "nowhere"),
    publish := {},
    publishLocal := {}
  ).aggregate (
    main,
    corenlp
  )

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
