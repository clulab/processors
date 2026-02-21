name := "processors-apps"
description := "processors-apps"

libraryDependencies ++= {
  Seq(
    "ch.qos.logback" % "logback-classic"  % "1.2.8", // up to 1.5.6; less than 1.2 is vulnerable
    "org.clulab"     % "processors-model" % "0.3.2"
  )
}
