package org.clulab.processors.apps

import org.clulab.processors.{Document, Processor}

import java.io.PrintWriter
import org.clulab.serialization.DocumentSerializer

/**
 * An example on how to fully annotate text using processors
 * User: mihais
 * Date: 3/28/13
 */
object ProcessorsDocSerializerExample {
  def main(args:Array[String]): Unit = {
    // create the processor
    val proc = Processor()

    // the actual work is done here
    val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")

    // you are basically done. the rest of this code simply prints out the annotations
    printDoc(doc)

    // serialize the doc using our custom serializer
    val ser = new DocumentSerializer
    val out = ser.save(doc)
    println("SERIALIZED DOC:\n" + out)
  }

  def printDoc(doc:Document): Unit = { doc.prettyPrint(new PrintWriter(System.out)) }

}
