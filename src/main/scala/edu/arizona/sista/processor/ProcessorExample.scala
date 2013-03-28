package edu.arizona.sista.processor

import corenlp.CoreNLPProcessor
import edu.arizona.sista.utils.DirectedGraphEdgeIterator

/**
 * 
 * User: mihais
 * Date: 3/28/13
 */
object ProcessorExample {
  def main(args:Array[String]) {
    // create the processor
    val proc:Processor = new CoreNLPProcessor()

    // the actual work is done here
    val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")

    // you are basically done. the rest of this code simply prints out the annotations

    // let's print the sentence-level annotations
    var sentenceCount = 0
    for (sentence <- doc.sentences) {
      println("Sentence #" + sentenceCount + ":")
      println("Tokens: " + sentence.words.mkString(" "))
      println("Start character offsets: " + sentence.startOffsets.mkString(" "))
      println("End character offsets: " + sentence.endOffsets.mkString(" "))

      // these annotations are optional, so they are stored using Option objects, hence the foreach statement
      sentence.lemmas.foreach(lemmas => println("Lemmas: " + lemmas.mkString(" ")))
      sentence.tags.foreach(tags => println("POS tags: " + tags.mkString(" ")))
      sentence.entities.foreach(entities => println("Named entities: " + entities.mkString(" ")))
      sentence.norms.foreach(norms => println("Normalized entities: " + norms.mkString(" ")))
      sentence.dependencies.foreach(dependencies => {
        println("Syntactic dependencies:")
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        while(iterator.hasNext) {
          val dep = iterator.next
          // note that we use offsets starting at 0 (unlike CoreNLP, which uses offsets starting at 1)
          println(" head:" + dep._1 + " modifier:" + dep._2 + " label:" + dep._3)
        }
      })

      sentenceCount += 1
      println("\n")
    }

    // let's print the coreference chains
    doc.coreferenceChains.foreach(chains => {
      for (chain <- chains.getChains) {
        println("Found one coreference chain containing the following mentions:")
        for (mention <- chain) {
          // note that all these offsets start at 0 too
          println("\tsentenceIndex:" + mention.sentenceIndex +
            " headIndex:" + mention.headIndex +
            " startTokenOffset:" + mention.startOffset +
            " endTokenOffset:" + mention.endOffset +
            " text: " + doc.sentences(mention.sentenceIndex).words.slice(mention.startOffset, mention.endOffset).mkString("[", " ", "]"))
        }
      }
    })
  }
}
