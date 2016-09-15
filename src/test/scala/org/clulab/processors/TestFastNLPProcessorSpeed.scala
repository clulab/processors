package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.scalatest._


import scala.io.Source

/**
 * This is a pseudo test; I just wanted to run FastNLPProcessor on a big file
 * User: mihais
 * Date: 2/25/15
 */
class TestFastNLPProcessorSpeed extends FlatSpec with Matchers {
  var proc:Processor = new FastNLPProcessor(internStrings = true, withDiscourse = ShallowNLPProcessor.NO_DISCOURSE, useMalt = false)

  "FastNLPProcessor" should "process a file successfully" in {
    val text = Source.fromFile("src/test/resources/org/clulab/processors/raw_text.txt").getLines.mkString(" ")
    val doc = proc.annotate(text)
    doc.sentences.length should be > 0
  }
}
