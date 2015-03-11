name := "processors"

version := "5.2-SNAPSHOT"

organization := "edu.arizona.sista"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

// fork jvm to separate process
fork := true

// options for forked jvm
javaOptions += "-Xmx6G"

// forward sbt's stdin to forked process
connectInput in run := true

// don't show output prefix
outputStrategy := Some(StdoutOutput)

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
pomExtra := (
  <url>https://github.com/sistanlp/processors</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/sistanlp/processors</url>
    <connection>https://github.com/sistanlp/processors</connection>
  </scm>
  <developers>
    <developer>
      <id>mihai.surdeanu</id>
      <name>Mihai Surdeanu</name>
      <email>mihai@surdeanu.info</email>
    </developer>
  </developers>)

//
// end publishing settings
//

// we need this to find the snapshot releases for BANNER
resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.5",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scalatest" %% "scalatest" % "3.0.0-SNAP4" % "test",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "com.io7m.xom" % "xom" % "1.2.10",
  "org.json4s" %% "json4s-native" % "3.2.11",
  "joda-time" % "joda-time" % "2.7",
  "de.jollyday" % "jollyday" % "0.4.7",
  "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.23",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.1" classifier "models",
  "ch.qos.logback" % "logback-classic" % "1.0.10",
  "org.slf4j" % "slf4j-api" % "1.7.10",
  "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.10",
  "net.sf.jopt-simple" % "jopt-simple" % "4.5",
  "de.bwaldvogel" % "liblinear" % "1.94",
  "log4j" % "log4j" % "1.2.17",
  "tw.edu.ntu.csie" % "libsvm" % "3.17",
  "org.yaml" % "snakeyaml" % "1.14",
  "jline" % "jline" % "2.12"
)
