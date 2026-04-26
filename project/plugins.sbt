ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

// Latest version numbers were updated on 20216-04-24.
addSbtPlugin("com.github.sbt"    % "sbt-pgp"      % "2.3.1")  // up to 2.3.1
// See https://www.playframework.com/releases.
// Newer versions of play are not compatible with Scala 2.11.  Only the newest work with Scala 3.
// Play 2.8 is the last that still works with Java 8, but only with Scala 2.12 and 2.13.
addSbtPlugin("com.typesafe.play" % "sbt-plugin"   % "2.8.19") // up to 3.0.4
addSbtPlugin("com.github.sbt"    % "sbt-release"  % "1.4.0")  // up to 1.4.0
// For constructing the fat jar:
addSbtPlugin("com.eed3si9n"      % "sbt-assembly" % "2.3.1")  // up to 2.3.1
