name := "processors-apps"
description := "processors-apps"

pomIncludeRepository := { (repo: MavenRepository) =>
  repo.root.startsWith("https://artifactory.clulab.org")
}

// for processors-models
resolvers += "clulab" at "https://artifactory.clulab.org/artifactory/sbt-release"

libraryDependencies ++= {
  val json4sVersion = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // Spark may have problems above 3.2.11, but processors has runtime errors much below 3.5.5.
      case Some((2, minor)) if minor <= 12 => "3.5.5"
      case Some((3, 0)) => "4.0.3"  // This is as close as we can get.
      case _ => "4.0.6"
    }
  }
  val combinatorsVersion = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, minor)) if minor <= 13 => "1.1.2" // Higher causes problems with libraries.
      case _ => "2.1.1" // up to 2.1.1
    }
  }
  // See https://index.scala-lang.org/scala/scala-parallel-collections/scala-parallel-collections.
  val parallelLibraries = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, minor)) if minor <= 12 => Seq()
      case _ => Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4") // up to 1.0.4, Apache-2.0
    }
  }
  val scala2Libraries = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(
        "org.scala-lang"              % "scala-reflect"            % scalaVersion.value // Apache-2.0
      )
      case _ => Seq.empty
    }
  }

  Seq(
    // "org.clulab"                  % "processors"                  % "10.0.0",
    // "org.clulab"                  % "deberta-onnx-model"          % "0.4.0", // TODO: update me!
  ) ++ parallelLibraries ++ scala2Libraries
}
