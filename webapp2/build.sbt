name := "processors-webapp2"
description := "A webapp based on cast"

libraryDependencies ++= {
  Seq(
    "com.lihaoyi" %% "cask"             % "0.9.3",  // as of 2025-06-02 up to 0.10.2
    "com.lihaoyi" %% "scalatags"        % "0.13.1", // as of 2025-06-02 up to 0.13.1

    "org.clulab"   % "processors-model" % "0.3.1"
  )
}
