name := "processors-webapp2"
description := "A webapp based on cast"

libraryDependencies ++= {
  Seq(
    "com.lihaoyi" %% "cask"             % "0.10.2", // as of 2025-06-02 up to 0.10.2
    "com.lihaoyi" %% "scalatags"        % "0.12.0", // as of 2025-06-02 up to 0.13.1

    "org.clulab"   % "processors-model" % "0.3.1"
  )
}
