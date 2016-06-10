name := "processors"

version := "5.8.5-SNAPSHOT"

organization := "org.clulab"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

// fork jvm to separate process
// fork := true

parallelExecution in Test := false

// options for forked jvm. No longer needed since we don't fork anymore.
// see .sbtopts for the JVM configuration that is used.
// javaOptions += "-Xmx6G"
// javaOptions += "-Xss100m"

// forward sbt's stdin to forked process
// connectInput in run := true

// don't show output prefix
// outputStrategy := Some(StdoutOutput)

lazy val core = project in file(".")

lazy val models = (project in file("models"))
  .settings(
    publish := {},
    publishLocal := {},
    publishM2 := {}
  )

addArtifact(Artifact("processors", "models"), modelsTask in models)

unmanagedJars in Compile += (modelsTask in models).value

unmanagedClasspath in Runtime += baseDirectory.value

//
// publishing settings
//

// publish to a maven repo
publishMavenStyle := true

// the standard maven repository
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

// let’s remove any repositories for optional dependencies in our artifact
pomIncludeRepository := { _ => false }

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

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.clulab" % "bioresources" % "1.1.9",
  "com.io7m.xom" % "xom" % "1.2.10",
  "org.json4s" %% "json4s-native" % "3.2.11",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.1" classifier "models",
  "ch.qos.logback" % "logback-classic" % "1.0.10",
  "org.slf4j" % "slf4j-api" % "1.7.10",
  "log4j" % "log4j" % "1.2.17", // this is used by our maltparser clone; otherwise not in use
  "de.bwaldvogel" % "liblinear" % "1.94",
  "tw.edu.ntu.csie" % "libsvm" % "3.17",
  "org.yaml" % "snakeyaml" % "1.14",
  "jline" % "jline" % "2.12.1"
)
