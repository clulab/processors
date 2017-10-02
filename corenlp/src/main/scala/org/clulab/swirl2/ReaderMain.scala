package org.clulab.swirl2

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.struct.{GraphMap, DirectedGraph}
import org.slf4j.LoggerFactory
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import Reader._
import org.clulab.serialization.DocumentSerializer

object ReaderMain {
  def main(args:Array[String]) {
    val reader = new Reader
    val proc = new FastNLPProcessor()
    val file = new File(args(0))
    val outputFile = new File(args(0) + ".ser")

    val doc = reader.read(file, proc, verbose = false)
    val serializer = new DocumentSerializer
    val os = new PrintWriter(outputFile)
    serializer.save(doc, os)
    os.close()
  }
}
