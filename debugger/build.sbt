name := "processors-debugger"
description := "A debugger for the Odin ExtractionEngine within processors"

libraryDependencies ++= {
  Seq(
    "com.lihaoyi" %% "scalatags"  % "0.12.0", // as of 2025-02-19 up to 0.13.1
    "com.lihaoyi" %% "sourcecode" % "0.3.0"  // as of 2025-01-27 up to 0.4.2
  )
}