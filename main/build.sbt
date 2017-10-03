name := "processors-main"

libraryDependencies ++= {
  val akkaV = "2.5.4"
  val json4sVersion = "3.5.2"

  Seq(
    // NLPTools for lemmatization:
    "edu.washington.cs.knowitall.nlptools"  %  "nlptools-stem-morpha_2.10"  % "2.4.5",
    "com.io7m.xom"             %  "xom"                       % "1.2.10",
    "com.typesafe"             %  "config"                    % "1.3.1",
    "commons-io"               %  "commons-io"                % "2.5",
    "de.bwaldvogel"            %  "liblinear"                 % "1.94",
    "jline"                    %  "jline"                     % "2.12.1",
    "org.antlr"                %  "antlr4-runtime"            % "4.6",   // for tokenization
    "org.clulab"               %  "bioresources"              % "1.1.24",
    "org.json4s"              %%  "json4s-core"               % json4sVersion,
    "org.json4s"              %%  "json4s-jackson"            % json4sVersion,
    "org.maltparser"           %  "maltparser"                % "1.9.0", // for dependency parsing
    "org.scala-lang.modules"  %%  "scala-parser-combinators"  % "1.0.3",
    "tw.edu.ntu.csie"          %  "libsvm"                    % "3.17",

    // logging
    "com.typesafe.scala-logging"  %%  "scala-logging"    % "3.4.0",
//  "ch.qos.logback"               % "logback-classic"   % "1.0.10", // OLD: REMOVE LATER
    "ch.qos.logback"               %  "logback-classic"  % "1.1.7",
    "org.slf4j"                    %  "slf4j-api"        % "1.7.10",

    // AKKA
    "com.typesafe.akka"   %%  "akka-actor"   % akkaV,
//  "com.typesafe.akka"   %%  "akka-remote"  % akkaV,
    "com.typesafe.akka"   %%  "akka-slf4j"   % akkaV,

    // testing
    "org.scalatest"       %%  "scalatest"      % "2.2.4"  % "test",
    "com.typesafe.akka"   %%  "akka-testkit"   % akkaV    % "test"
  )

}
