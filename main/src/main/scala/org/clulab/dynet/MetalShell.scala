package org.clulab.dynet

import org.clulab.utils.Shell

class MetalShell(val mtl: Metal) extends Shell {

  override def work(text: String): Unit = {
    val words = text.split("\\s+")
    val constEmbeddings = ConstEmbeddingsGlove.mkConstLookupParams(words)
    val labels = mtl.predictJointly(AnnotatedSentence(words), constEmbeddings)
    print(words, labels)
  }

  /** Prints one document */
  def print(words: Array[String], labels: IndexedSeq[IndexedSeq[String]]): Unit = {
    println(s"Input words: ${words.mkString(", ")}")
    for (tid <- labels.indices)
      println(s"Labels for task #$tid: ${labels(tid).mkString(", ")}")
  }
}
