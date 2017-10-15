name := "processors-main"

libraryDependencies ++= {

  val json4sVersion = "3.5.2"

  Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.clulab" % "bioresources" % "1.1.24",
    "com.io7m.xom" % "xom" % "1.2.10",
    "org.json4s" %% "json4s-core" % json4sVersion,
    "org.json4s" %% "json4s-jackson" % json4sVersion,
    "ch.qos.logback" % "logback-classic" % "1.0.10",
    "org.slf4j" % "slf4j-api" % "1.7.10",
    "de.bwaldvogel" % "liblinear" % "1.94",
    "tw.edu.ntu.csie" % "libsvm" % "3.17",
    "org.antlr" % "antlr4-runtime" % "4.6", // for tokenization
    "edu.washington.cs.knowitall.nlptools" % "nlptools-stem-morpha_2.10" % "2.4.5", // for lemmatization
    "org.maltparser" % "maltparser" % "1.9.0", // for dependency parsing
    "jline" % "jline" % "2.12.1",
    "commons-io" % "commons-io" % "2.5",
    "com.typesafe" % "config" % "1.3.1"
  )

}
