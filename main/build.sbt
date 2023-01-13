name := "processors-main"
description := "processors-main"

pomIncludeRepository := { (repo: MavenRepository) =>
  repo.root.startsWith("https://artifactory.clulab.org")
}

// for processors-models
resolvers += "clulab" at "https://artifactory.clulab.org/artifactory/sbt-release"

libraryDependencies ++= {
  val json4sVersion = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, 0)) => "4.0.3" // This is as close as we can get.
      case _ => "4.0.6"
    }
  }
  val combinatorsVersion = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => "2.1.1" // up to 2.1.1
      case _ => "1.1.2" // Higher causes problems with libraries.
    }
  }
  // See https://index.scala-lang.org/scala/scala-parallel-collections/scala-parallel-collections.
  val parallelLibraries = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, major)) if major <= 12 => Seq()
      case _ => Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4") // up to 1.0.4
    }
  }
  val scala2Libraries = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(
        "org.scala-lang"              % "scala-reflect"            % scalaVersion.value
      )
      case _ => Seq.empty
    }
  }

  Seq(
    // common tools
    "commons-io"                  % "commons-io"               % "2.5",
    "com.typesafe"                % "config"                   % "1.3.1",
    "jline"                       % "jline"                    % "2.12.1",
    "org.json4s"                 %% "json4s-core"              % json4sVersion,
    "org.json4s"                 %% "json4s-jackson"           % json4sVersion,
    // for machine learning
    "org.clulab"                 %% "fatdynet"                 % "0.4.4",
    "de.bwaldvogel"               % "liblinear"                % "2.30",
    "tw.edu.ntu.csie"             % "libsvm"                   % "3.23",
    // NLP tools used by CluProcessor
    "org.antlr"                   % "antlr4-runtime"           % "4.9.2",  // for tokenization
    "org.clulab"                  % "lemport"                  % "0.9.10" exclude("org.scala-lang", "scala-library"), // Portuguese lemmatizer
    "de.jollyday"                 % "jollyday"                 % "0.5.10", // for holidays normalization
    // logging
    // The Scala interface is not used in processors.
    // "com.typesafe.scala-logging" %% "scala-logging"            % "3.9.4",
    // Instead, all code makes use of the Java interface.
    "org.slf4j"                   % "slf4j-api"                % "1.7.32",
    // Local logging is provided here but not published.
    "ch.qos.logback" % "logback-classic" % "1.2.8", // up to 1.2.8; less than 1.2 is vulnerable
    // testing
    "org.scalatest"              %% "scalatest"                % "3.2.10"  % Test,
    // trained models for local ML models used in both main and corenlp
    // These are stored in the CLU lab Artifactory instance, not maven!
    "org.clulab"                  % "glove-840b-300d-10f-kryo" % "1.0.0",
    "org.clulab"                  % "processors-models"        % "0.2.4" exclude("org.scala-lang", "scala-library"),
    "com.esotericsoftware"        % "kryo"                     % "5.1.1",
    // for odin
    "org.apache.commons"          % "commons-text"             % "1.1",
    // See https://docs.scala-lang.org/overviews/core/collections-migration-213.html.
    "org.scala-lang.modules"     %% "scala-collection-compat"  % "2.6.0", // up to 2.9.0, but match fatdynet
    "org.scala-lang.modules"     %% "scala-parser-combinators" % combinatorsVersion,
    "org.yaml"                    % "snakeyaml"                % "1.14",
    // progress bar for training
    "me.tongfei"                  % "progressbar"              % "0.9.3"
  ) ++ parallelLibraries ++ scala2Libraries
}
