package edu.arizona.sista.processors

import scala.io.StdIn
import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 * A simple interactive shell
 * User: mihais
 * Date: 3/13/14
 */
object ProcessorShell {
  def main(args:Array[String]) {
    // create the processor
    // val proc:Processor = new CoreNLPProcessor(withDiscourse = false) // this uses the slow but better discourse parser
    // val proc:Processor = new FastNLPProcessor(useMalt = false) // this uses the fast but slightly worse discourse parser
    val proc:Processor = new BioNLPProcessor(withDiscourse = false, removeFigTabReferences = true)

    while(true) {
      print("> ")
      var text = StdIn.readLine()
      val doc = proc.annotate(text)
      ProcessorExample.printDoc(doc)
    }
  }
}
