package org.clulab.processors.clu

import org.clulab.struct.DirectedGraphEdgeIterator
import org.clulab.utils.Shell

/**
  * An interactive shell for CluProcessor
  * User: mihais
  * Date: 8/2/17
  */
class CluShell extends Shell {
  var proc: CluProcessor = _

  override def initialize(): Unit = {
    proc = new CluProcessor()
  }

  override def work(text: String): Unit = {
    val doc = proc.annotate(text)

    var sentenceCount = 0
    for (sentence <- doc.sentences) {
      println("Sentence #" + sentenceCount + ":")
      val indices = 0 until sentence.size
      println("Raw: " + sentence.raw.zip(indices).mkString(" "))
      println("Tokens: " + sentence.words.zip(indices).mkString(" "))

      if(sentence.lemmas.isDefined)
        println("Lemmas: " + sentence.lemmas.get.zip(indices).mkString(" "))

      if(sentence.tags.isDefined)
        println("Tags: " + sentence.tags.get.zip(indices).mkString(" "))

      if(sentence.entities.isDefined)
        println("Entities: " + sentence.entities.get.zip(indices).mkString(" "))

      if(sentence.chunks.isDefined)
        println("Chunks: " + sentence.chunks.get.zip(indices).mkString(" "))

      sentence.universalBasicDependencies.foreach(dependencies => {
        println("Basic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })

      sentence.universalEnhancedDependencies.foreach(dependencies => {
        println("Enhanced dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })

      sentenceCount += 1
      println("\n")
    }
  }
}

object CluShell {
  def main(args:Array[String]): Unit = {
    val sh = new CluShell
    sh.shell()
  }
}
