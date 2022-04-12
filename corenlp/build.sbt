name := "processors-corenlp"
description := "processors-corenlp"

libraryDependencies ++= {
  val corenlpV = "3.9.2"

  Seq (
    // this sub-project depends on CoreNLP
    "edu.stanford.nlp" % "stanford-corenlp" % corenlpV,
    "edu.stanford.nlp" % "stanford-corenlp" % corenlpV classifier "models"
  )
}

// This is an exception to all the other projects.
ThisBuild / licenses := List(
  "GNU General Public License v3 (GPLv3)" ->
      url("https://www.gnu.org/licenses/gpl-3.0.txt")
)
