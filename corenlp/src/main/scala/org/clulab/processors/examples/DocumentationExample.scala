package org.clulab.processors.examples

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.struct.DirectedGraphEdgeIterator

object DocumentationExample extends App {
  // Create the processor.  Any processor works here!
  // Try FastNLPProcessor or our own CluProcessor.
  val proc: Processor = new CoreNLPProcessor()

  // The actual work is done here.
  val doc: Document = proc.annotate("John Smith went to China. He visited Beijing on January 10th, 2013.")

  // You are basically done.  The rest of this code simply prints out the annotations.

  // Let's print the sentence-level annotations.
  for ((sentence, sentenceIndex) <- doc.sentences.zipWithIndex) {
    println("Sentence #" + sentenceIndex + ":")
    println("Tokens: " + sentence.words.mkString(" "))
    println("Start character offsets: " + sentence.startOffsets.mkString(" "))
    println("End character offsets: " + sentence.endOffsets.mkString(" "))

    // These annotations are optional, so they are stored using Option objects,
    // hence the foreach statement.
    sentence.lemmas.foreach(lemmas => println("Lemmas: " + lemmas.mkString(" ")))
    sentence.tags.foreach(tags => println("POS tags: " + tags.mkString(" ")))
    sentence.chunks.foreach(chunks => println("Chunks: " + chunks.mkString(" ")))
    sentence.entities.foreach(entities => println("Named entities: " + entities.mkString(" ")))
    sentence.norms.foreach(norms => println("Normalized entities: " + norms.mkString(" ")))
    sentence.dependencies.foreach { dependencies =>
      println("Syntactic dependencies:")
      val iterator = new DirectedGraphEdgeIterator[String](dependencies)
      iterator.foreach { dep =>
        // Note that we use offsets starting at 0 unlike CoreNLP, which uses offsets starting at 1.
        println(" head: " + dep._1 + " modifier: " + dep._2 + " label: " + dep._3)
      }
    }
    sentence.syntacticTree.foreach { syntacticTree =>
      // See the org.clulab.utils.Tree class for more information
      // on syntactic trees, including access to head phrases/words.
      println("Constituent tree: " + syntacticTree)
    }
    println()
    println()
  }

  // Let's print the coreference chains.
  doc.coreferenceChains.foreach { chains =>
    for (chain <- chains.getChains) {
      println("Found one coreference chain containing the following mentions:")
      for (mention <- chain) {
        val text = doc.sentences(mention.sentenceIndex).words
            .slice(mention.startOffset, mention.endOffset).mkString("[", " ", "]")
        // Note that all these offsets start at 0, too.
        println("\tsentenceIndex: " + mention.sentenceIndex +
            " headIndex: " + mention.headIndex +
            " startTokenOffset: " + mention.startOffset +
            " endTokenOffset: " + mention.endOffset +
            " text: " + text)
      }
    }
  }
}
