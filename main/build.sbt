name := "processors-main"
description := "processors-main"

pomIncludeRepository := { (repo: MavenRepository) =>
  repo.root.startsWith("http://artifactory.cs.arizona.edu")
}

// for processors-models
resolvers += "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release"

libraryDependencies ++= {
  val json4sVersion = "3.5.2"

  Seq(
    // common tools
    "commons-io"                  % "commons-io"               % "2.5",
    "com.typesafe"                % "config"                   % "1.3.1",
    "jline"                       % "jline"                    % "2.12.1",
    "org.json4s"                 %% "json4s-core"              % json4sVersion,
    "org.json4s"                 %% "json4s-jackson"           % json4sVersion,
    "org.scala-lang.modules"     %% "scala-parser-combinators" % "1.0.4",
    "com.io7m.xom"                % "xom"                      % "1.2.10",
    // for machine learning
    "org.clulab"                 %% "fatdynet"                 % "0.3.2", // "0-cuda.2.6-SNAPSHOT"
    "de.bwaldvogel"               % "liblinear"                % "2.30",
    "tw.edu.ntu.csie"             % "libsvm"                   % "3.23",
    // NLP tools used by CluProcessor
    "org.antlr"                   % "antlr4-runtime"           % "4.9.2",   // for tokenization
    "org.clulab"                  % "lemport"                  % "0.9.10", // Portuguese lemmatizer
    "de.jollyday"                 % "jollyday"                 % "0.5.10", // for holidays normalization
    // logging
    "ch.qos.logback"              % "logback-classic"          % "1.2.8", // up to 1.2.8; less than 1.2 is vulnerable
    "com.typesafe.scala-logging" %% "scala-logging"            % "3.7.2",
    "org.slf4j"                   % "slf4j-api"                % "1.7.10",
    // testing
    "org.scalatest"              %% "scalatest"                % "3.0.1"  % Test,
    // trained models for local ML models used in both main and corenlp
    // These are stored in the CLU lab Artifactory not maven!
    "org.clulab"                  % "glove-840b-300d-10f-kryo" % "1.0.0",
    "org.clulab"                  % "processors-models"        % "0.1.10",
    "com.esotericsoftware"        % "kryo"                     % "5.1.1",

    // for odin
    "org.apache.commons"      % "commons-text"             % "1.1",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "org.scala-lang"          % "scala-reflect"            % scalaVersion.value,
    "org.yaml"                % "snakeyaml"                % "1.14"
  )
}
