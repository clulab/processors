package edu.arizona.sista.processors

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import org.scalatest._


import scala.io.Source

/**
 * This is a pseudo test; I just wanted to run FastNLPProcessor on a big file
 * User: mihais
 * Date: 2/25/15
 */
class TestFastNLPProcessorSpeed extends FlatSpec with Matchers {
  var proc:Processor = new FastNLPProcessor(internStrings = true, withDiscourse = false, useMalt = false)

  "FastNLPProcessor" should "process a file successfully" in {
    val text = Source.fromFile("src/test/resources/edu/arizona/sista/processors/raw_text.txt").getLines.mkString(" ")
    val doc = proc.annotate(text)
    doc.sentences.length should be > 0
  }
}
