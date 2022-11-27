package org.clulab.processors.examples

import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.struct.DirectedGraphEdgeIterator

object DocumentationExample extends App {
  // Create the processor.  Any processor works here!
  // Try FastNLPProcessor or our own CluProcessor.
  val proc: Processor = new CoreNLPProcessor()

  // val proc: Processor = new FastNLPProcessor()

  // val proc: Processor = new CluProcessor()

  // org.clulab.dynet.Utils.initializeDyNet(mem = "1024,1024,1024,1024")
  // val proc: Processor = new CluProcessor()

  // val proc = new CoreNLPProcessor(internStrings = false)

  // The actual work is done here.
  val doc: Document = proc.annotate("John Smith went to China. He visited Beijing on January 10th, 2013.")

  // val doc = proc.annotateFromSentences(List("John Smith went to China.", "He visited Beijing."))

  // val doc = proc.annotateFromTokens(List(
  //   List("John", "Smith", "went", "to", "China", "."),
  //   List("There", ",", "he", "visited", "Beijing", ".")
  // ))

  // val doc = proc.mkDocument("John Smith went to China. He visited Beijing on January 10th, 2013.")
  // proc.tagPartsOfSpeech(doc)
  // proc.lemmatize(doc)
  // proc.recognizeNamedEntities(doc)
  // doc.clear()

  /*
  {
    import java.io.BufferedReader
    import java.io.FileReader
    val someText = "This is some text."
    val printWriter = new java.io.PrintWriter(System.out)
    val bufferedReader = new BufferedReader(new FileReader("./file"))
    // saving to a PrintWriter
    val someAnnotation = proc.annotate(someText)
    val serializer = new org.clulab.serialization.DocumentSerializer
    serializer.save(someAnnotation, printWriter)

    // loading from a BufferedReader
    val someAnnotation2 = serializer.load(bufferedReader)
  }
  */

  /*
  {
    val someText = "This is some text."
    // saving to a String
    val someAnnotation = proc.annotate(someText)
    val serializer = new org.clulab.serialization.DocumentSerializer
    val string = serializer.save(someAnnotation)

    // loading from a String
    val someAnnotation2 = serializer.load(string)
  }
  */

  // Processor.clearStrings()

  // You are basically done.  The rest of this code simply prints out the annotations.

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

  def mkString[T](elems: Array[T]): String = elems.mkString(" ")
}
