name := "processors-main"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.clulab" % "bioresources" % "1.1.22",
  "com.io7m.xom" % "xom" % "1.2.10",
  "org.json4s" %% "json4s-core" % "3.5.0",
  "org.json4s" %% "json4s-native" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.0.10",
  "org.slf4j" % "slf4j-api" % "1.7.10",
  "log4j" % "log4j" % "1.2.17", // this is used by our maltparser clone; otherwise not in use
  "de.bwaldvogel" % "liblinear" % "1.94",
  "tw.edu.ntu.csie" % "libsvm" % "3.17",
  "org.antlr" % "antlr4-runtime" % "4.6",
  "jline" % "jline" % "2.12.1",
  "commons-io" % "commons-io" % "2.5"
)
