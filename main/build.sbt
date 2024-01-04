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
    // common tools
    "commons-io"                  % "commons-io"               % "2.5", // Apache-2.0
    "com.typesafe"                % "config"                   % "1.3.1", // Apache-2.0
    "jline"                       % "jline"                    % "2.12.1", // BSD
    "org.json4s"                 %% "json4s-core"              % json4sVersion, // Apache-2.0
    "org.json4s"                 %% "json4s-jackson"           % json4sVersion, // Apache-2.0
    // for machine learning
    "org.clulab"                 %% "fatdynet"                 % "0.4.4", // Apache-2.0
    "de.bwaldvogel"               % "liblinear"                % "2.30", // BSD-3
    "tw.edu.ntu.csie"             % "libsvm"                   % "3.23", // BSD
    // NLP tools used by CluProcessor
    "org.antlr"                   % "antlr4-runtime"           % "4.9.2",  // for tokenization // BSD
    "org.clulab"                  % "lemport"                  % "0.9.10" exclude("org.scala-lang", "scala-library"), // Portuguese lemmatizer // LGPL-3.0
    "de.jollyday"                 % "jollyday"                 % "0.4.9", // for holidays normalization, match stanford-nlp dependency // Apache-2.0
    // logging
    // The Scala interface is not used in processors.
    // "com.typesafe.scala-logging" %% "scala-logging"            % "3.9.4",
    // Instead, all code makes use of the Java interface.
    "org.slf4j"                   % "slf4j-api"                % "1.7.32", // MIT
    // Local logging is provided here but not published.
    "ch.qos.logback"              % "logback-classic"          % "1.2.8", // up to 1.2.8; less than 1.2 is vulnerable
    // testing
    "org.scalatest"              %% "scalatest"                % "3.2.15"  % Test, // Apache-2.0
    // trained models for local ML models used in both main and corenlp
    // These are stored in the CLU lab Artifactory instance, not maven!
    "org.clulab"                  % "glove-840b-300d-10f-kryo" % "1.0.0", // Apache-2.0
    "org.clulab"                  % "processors-models"        % "0.2.4" exclude("org.scala-lang", "scala-library"), // Apache-2.0
    "com.esotericsoftware"        % "kryo"                     % "5.1.1", // BSD-3
    // for odin
    "org.apache.commons"          % "commons-text"             % "1.1", // Apache-2.0
    // See https://docs.scala-lang.org/overviews/core/collections-migration-213.html.
    // fatdynet 0.4.4 uses 2.6.0 which will be evicted.  Move to fatdynet 0.4.5 for a 2.11.0 match.
    "org.scala-lang.modules"     %% "scala-collection-compat"  % "2.11.0", // up to 2.11.0 // Apache-2.0
    "org.scala-lang.modules"     %% "scala-parser-combinators" % combinatorsVersion, // Apache-2.0
    "org.yaml"                    % "snakeyaml"                % "1.14", // Apache-2.0
    // progress bar for training
    "me.tongfei"                  % "progressbar"              % "0.9.3", // MIT

//    "com.sun.xml.bind"            % "jaxb-impl"                % "3.0.0",
//    "jakarta.xml.bind"            % "jakarta.xml.bind-api"     % "3.0.0"

    "com.sun.xml.bind"            % "jaxb-ri"                  % "2.3.3",
    "jakarta.xml.bind"            % "jakarta.xml.bind-api"     % "2.3.3"


  ) ++ parallelLibraries ++ scala2Libraries
}
