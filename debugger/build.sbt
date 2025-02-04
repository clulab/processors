name := "processors-debugger"
description := "A debugger for the Odin ExtractionEngine within processors"

libraryDependencies ++= {
  Seq(
    "com.lihaoyi" %% "scalatags"  % "0.8.2", // as of 2025-02-03 up to 0.8.2
    "com.lihaoyi" %% "sourcecode" % "0.3.0"  // as of 2025-01-27 up to 0.4.2
  )
}