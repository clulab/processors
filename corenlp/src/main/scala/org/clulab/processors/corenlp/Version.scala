package org.clulab.processors.corenlp

case class Version(major: Int, minor: Int, revision: Int)

object Version {
  def apply(version: Map[String, Int]): Version = Version(version("major"), version("minor"), version("revision"))

  val stanford: Version = Version(BuildInfo.stanfordVersion)
}
