package org.clulab.processors.apps

import org.clulab.processors.clu.DocumentPrettyPrinter
import org.clulab.processors.{Document, Processor}
import org.clulab.serialization.DocumentSerializer

import java.io.PrintWriter


/**
 * An example on how to fully annotate text using processors
 * User: mihais
 * Date: 3/28/13
 */
object ProcessorsDocSerializerExample {
  def main(args:Array[String]): Unit = {
    val documentPrinter = new DocumentPrettyPrinter(new PrintWriter(System.out))
    // create the processor
    val proc = Processor()

    // the actual work is done here
    val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")

    // you are basically done. the rest of this code simply prints out the annotations
    documentPrinter.print(doc)

    // serialize the doc using our custom serializer
    val ser = new DocumentSerializer
    val out = ser.save(doc)
    println("SERIALIZED DOC:\n" + out)
  }
}
