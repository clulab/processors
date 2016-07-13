package org.clulab.odin


object TestUtils {

  // the rule file containing the path to the embeddings resources
  val embeddingsFile = "src/test/resources/org/clulab/odin/grammars/embeddings.yml"

  def readFile(filename: String) = {
    val source = io.Source.fromFile(filename)
    val data = source.mkString
    source.close()
    data
  }
}
