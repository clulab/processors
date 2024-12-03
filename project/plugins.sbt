// Latest version numbers were updated on 2024 July 11.
addSbtPlugin("com.jsuereth"      % "sbt-pgp"      % "1.1.2-1") // up to 2.2.1 *
addSbtPlugin("org.xerial.sbt"    % "sbt-sonatype" % "2.3")     // up to 3.9.21 *
// See https://www.playframework.com/releases.
// Newer versions of play are not compatible with Scala 2.11.  Only the newest work with Scala 3.
// Play 2.8 is the last that still works with Java 8, but only with Scala 2.12 and 2.13.
addSbtPlugin("com.typesafe.play" % "sbt-plugin"   % "2.8.19")  // up to 3.0.4
addSbtPlugin("com.github.gseitz" % "sbt-release"  % "1.0.13")  // up to 1.4.0
// For constructing the fat jar:
addSbtPlugin("com.eed3si9n"      % "sbt-assembly" % "2.3.0")   // up to 2.3.0

// * Held back out of an abundance of caution.
