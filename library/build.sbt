name := "processors"
description := "processors"

libraryDependencies ++= {
  val json4sVersion = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // Spark may have problems above 3.2.11, but processors has runtime errors much below 3.5.5.
      case Some((2, minor)) if minor <= 12 => "3.5.5"
      case Some((3, 0)) => "4.0.3"  // This is as close as we can get.
      case _ => "4.0.6" // up to 4.0.7
    }
  }
  val combinatorsVersion = {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, minor)) if minor <= 13 => "1.1.2" // Higher causes problems with libraries.
      case _ => "2.1.1" // up to 2.4.0
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
      case Some((2, _)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value) // Apache-2.0
      case _ => Seq.empty
    }
  }

  Seq(
    // common tools
    "commons-io"                  % "commons-io"                 % "2.5",              // up to 2.16.1, Apache-2.0
    "com.typesafe"                % "config"                     % "1.3.1",            // up to 1.4.3, Apache-2.0
    "jline"                       % "jline"                      % "2.12.1",           // up to 2.14.6 or 3.26.2, BSD
    "org.json4s"                 %% "json4s-core"                % json4sVersion,      // Apache-2.0
    "org.json4s"                 %% "json4s-jackson"             % json4sVersion,      // Apache-2.0
    // for machine learning
    "org.clulab"                  % "processors-model"           % "0.3.1" % Test,
    "org.clulab"                 %% "scala-transformers-encoder" % "0.7.0",            // up to 0.7.0, Apache-2.0
    "de.bwaldvogel"               % "liblinear"                  % "2.30",             // up to 2.44, BSD-3
    "tw.edu.ntu.csie"             % "libsvm"                     % "3.23",             // up to 3.31, BSD
    // NLP tools used by CluProcessor
    "org.antlr"                   % "antlr4-runtime"             % "4.9.2",            // up to 4.13.1, BSD, for tokenization
    "org.clulab"                  % "lemport"                    % "0.9.10" exclude("org.scala-lang", "scala-library"), // Portuguese lemmatizer // LGPL-3.0
    "de.jollyday"                 % "jollyday"                   % "0.4.9",            // up to 0.5.10, Apache-2.0, for holidays normalization
    // logging
    // The Scala interface is not used in processors.
    // Instead, all code makes use of the Java interface.
    "org.slf4j"                   % "slf4j-api"                  % "1.7.32",           // up to 2.0.13, MIT
    // testing
    "org.scalatest"              %% "scalatest"                  % "3.2.15"  % Test,   // up to 3.2.19, Apache-2.0
    // for odin
    "org.apache.commons"          % "commons-text"               % "1.1",              // up to 1.12.0, Apache-2.0
    "org.scala-lang.modules"     %% "scala-collection-compat"    % "2.11.0",           // up to 2.12.0 // Apache-2.0
    "org.scala-lang.modules"     %% "scala-parser-combinators"   % combinatorsVersion, // Apache-2.0
    "org.yaml"                    % "snakeyaml"                  % "1.14",             // up to 2.2, Apache-2.0
    // progress bar for training
    "me.tongfei"                  % "progressbar"                % "0.9.3",            // up to 0.10.1, MIT
    // This supports JollyDay on Java > 8.
    "com.sun.xml.bind"            % "jaxb-ri"                    % "2.3.3"
  ) ++ parallelLibraries ++ scala2Libraries
}
