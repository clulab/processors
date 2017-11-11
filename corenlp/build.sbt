name := "processors-corenlp"

libraryDependencies ++= {
  val akkaV = "2.5.4"
  Seq (
    "ai.lum"             %%  "common"            % "0.0.8",
    "edu.stanford.nlp"    %  "stanford-corenlp"  % "3.5.1",
    "edu.stanford.nlp"    %  "stanford-corenlp"  % "3.5.1" classifier "models",
    "org.clulab"          %  "bioresources"      % "1.1.25-SNAPSHOT",

    // logging
    "com.typesafe.scala-logging"  %%  "scala-logging"    % "3.7.2",
    "ch.qos.logback"               %  "logback-classic"  % "1.0.10",
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
