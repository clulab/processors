package org.clulab.processors.examples

import java.io.PrintWriter

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.serialization.DocumentSerializer
import org.clulab.processors.{Document, Processor}
import org.clulab.struct.DirectedGraphEdgeIterator

/**
 * An example on how to fully annotate text using the CoreNLP or FastNLP processors
 * User: mihais
 * Date: 3/28/13
 */
object ProcessorExample {
  def main(args:Array[String]) {
    // create the processor
    val proc:Processor = new CoreNLPProcessor(withDiscourse = ShallowNLPProcessor.WITH_DISCOURSE)

    // for much faster processing, use FastNLPProcessor
    // this processor does not support constituent parsing and coreference resolution, and
    //   the generated dependencies are Stanford's "basic" rather "collapsed" dependencies
    // it should run at least an order of magnitude faster than CoreNLPProcessor
    //val proc:Processor = new FastNLPProcessor()
    
    // other processors supported:
    // BioNLPProcessor, and FastBioNLPProcessor - for the biomedical domain
    // CluProcessor - similar to FastNLPProcessor, but using tools licensed under the Apache license

    // the actual work is done here
    val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")

    // you are basically done. the rest of this code simply prints out the annotations
    printDoc(doc)

    // serialize the doc using our custom serializer
    val ser = new DocumentSerializer
    val out = ser.save(doc)
    println("SERIALIZED DOC:\n" + out)
  }

  def printDoc(doc:Document) { doc.prettyPrint(new PrintWriter(System.out)) }

}
