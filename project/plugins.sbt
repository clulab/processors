// Latest version numbers were updated on 2021 Mar 11.
addSbtPlugin("com.jsuereth"      % "sbt-pgp"      % "1.1.2-1") // up to 1.1.2-1 *
addSbtPlugin("org.xerial.sbt"    % "sbt-sonatype" % "2.3")     // up to 3.9.6 *
// Newer versions of play are not compatible with Scala 2.11.  None works with Scala 3.
addSbtPlugin("com.typesafe.play" % "sbt-plugin"   % "2.8.19")  // up to 2.8.19
addSbtPlugin("com.github.gseitz" % "sbt-release"  % "1.0.13")  // up to 1.0.13
// * Held back out of an abundance of caution.
