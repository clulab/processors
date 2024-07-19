package org.clulab.processors.apps

import org.clulab.processors.{Document, Processor}
import org.clulab.struct.DirectedGraphEdgeIterator

object ProcessorsScalaExample extends App {
  val proc = Processor()

  // The actual work is done here.
  val doc: Document = proc.annotate("John Smith went to China. He visited Beijing on January 10th, 2013.")

  // Let's print the sentence-level annotations.
  for ((sentence, sentenceIndex) <- doc.sentences.zipWithIndex) {
    println("Sentence #" + sentenceIndex + ":")
    println("Tokens: " + mkString(sentence.words))
    println("Start character offsets: " + mkString(sentence.startOffsets))
    println("End character offsets: " + mkString(sentence.endOffsets))

    // These annotations are optional, so they are stored using Option objects,
    // hence the foreach statement.
    sentence.lemmas.foreach(lemmas => println("Lemmas: " + mkString(lemmas)))
    sentence.tags.foreach(tags => println("POS tags: " + mkString(tags)))
    sentence.chunks.foreach(chunks => println("Chunks: " + mkString(chunks)))
    sentence.entities.foreach(entities => println("Named entities: " + mkString(entities)))
    sentence.norms.foreach(norms => println("Normalized entities: " + mkString(norms)))
    sentence.dependencies.foreach { dependencies =>
      println("Syntactic dependencies:")
      val iterator = new DirectedGraphEdgeIterator[String](dependencies)
      iterator.foreach { dep =>
        // Note that we use offsets starting at 0 unlike CoreNLP, which uses offsets starting at 1.
        println(" head: " + dep._1 + " modifier: " + dep._2 + " label: " + dep._3)
      }
    }
    println()
  }

  def mkString[T](elems: Array[T]): String = elems.mkString(" ")
}
