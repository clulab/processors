// Include this with (sub)projects that need to be published.
// If this is not included, the delegate comes from Sonatype.scala.
import org.clulab.sbt.BuildUtils

publishMavenStyle := true
pomIncludeRepository := BuildUtils.keepHttpRepos
