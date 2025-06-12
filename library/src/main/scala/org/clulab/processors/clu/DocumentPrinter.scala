package org.clulab.processors.clu

import org.clulab.processors.Document
import org.clulab.struct.DirectedGraphEdgeIterator

import java.io.PrintWriter

trait DocumentPrinter {
  def print(document: Document): Unit
}

class DocumentPrettyPrinter(printWriter: PrintWriter) extends DocumentPrinter {

  def println(string: String): Unit = printWriter.println(string)

  def print(document: Document): Unit = {
    // let's print the sentence-level annotations
    document.sentences.zipWithIndex.foreach { case (sentence, sentenceCount) =>
      println("Sentence #" + sentenceCount + ":")
      println("Tokens: " + sentence.words.zipWithIndex.mkString(" "))
      println("Start character offsets: " + sentence.startOffsets.mkString(" "))
      println("End character offsets: " + sentence.endOffsets.mkString(" "))

      // these annotations are optional, so they are stored using Option objects, hence the foreach statement
      sentence.lemmas.foreach(lemmas => println(s"Lemmas: ${lemmas.mkString(" ")}"))
      sentence.tags.foreach(tags => println(s"POS tags: ${tags.mkString(" ")}"))
      sentence.chunks.foreach(chunks => println(s"Chunks: ${chunks.mkString(" ")}"))
      sentence.entities.foreach(entities => println(s"Named entities: ${entities.mkString(" ")}"))
      sentence.norms.foreach(norms => println(s"Normalized entities: ${norms.mkString(" ")}"))
      sentence.universalBasicDependencies.foreach(dependencies => {
        println("Basic syntactic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next()
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })
      sentence.universalEnhancedDependencies.foreach(dependencies => {
        println("Enhanced syntactic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next()
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })
      sentence.semanticRoles.foreach(dependencies => {
        println("Semantic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next()
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })
      sentence.enhancedSemanticRoles.foreach(dependencies => {
        println("Enhanced semantic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while (iterator.hasNext) {
          val dep = iterator.next()
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })
      sentence.syntacticTree.foreach(tree => {
        println("Constituent tree: " + tree.toStringDepth(showHead = false))
        // see the org.clulab.struct.Tree class for more information
        // on syntactic trees, including access to head phrases/words
      })

      println("\n")
    }

    // let's print the coreference chains
    document.coreferenceChains.foreach(chains => {
      for (chain <- chains.getChains) {
        println("Found one coreference chain containing the following mentions:")
        for (mention <- chain) {
          // note that all these offsets start at 0 too
          println("\tsentenceIndex:" + mention.sentenceIndex +
              " headIndex:" + mention.headIndex +
              " startTokenOffset:" + mention.startOffset +
              " endTokenOffset:" + mention.endOffset +
              " text: " + document.sentences(mention.sentenceIndex).words.slice(mention.startOffset, mention.endOffset).mkString("[", " ", "]"))
        }
      }
    })
    printWriter.flush()
  }
}
