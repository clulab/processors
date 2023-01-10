name := "processors-corenlp"
description := "processors-corenlp"

// Unfortunately, this doesn't work.
// Avoid the problem of "xml-apis:xml-apis is evicted for all versions".
// dependencyOverrides += "xml-apis" % "xml-apis" % "2.0.2" // "1.0.b2" // "1.3.03"

libraryDependencies ++= {
  val corenlpV = "3.9.2"

  Seq (
    // This sub-project depends on CoreNLP.
    // Both jollyday and slf4j are provided by processors-main in updated versions.
    // The commons-lang3 version is determined by commons-text in processors-main.
    // io7m.xom brings in XML but in such a way that the dependencies are mixed up
    // and one gets "* xml-apis:xml-apis is evicted for all versions" so we have none.
    "edu.stanford.nlp" % "stanford-corenlp" % corenlpV
        exclude("de.jollyday", "jollyday")
        exclude("org.slf4j", "slf4j-api")
        exclude("org.apache.commons", "commons-lang3")
        exclude("com.io7m.xom", "xom"),
    "edu.stanford.nlp" % "stanford-corenlp" % corenlpV classifier "models"
        exclude("de.jollyday", "jollyday")
        exclude("org.slf4j", "slf4j-api")
        exclude("org.apache.commons", "commons-lang3")
        exclude("com.io7m.xom", "xom")
  )
}

// This is an exception to all the other projects.
ThisBuild / licenses := List(
  "GNU General Public License v3 (GPLv3)" ->
      url("https://www.gnu.org/licenses/gpl-3.0.txt")
)
