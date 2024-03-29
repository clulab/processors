package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.TestUtils
import org.clulab.utils.Test

/**
 * This is a pseudo test; I just wanted to run FastNLPProcessor on a big file
 * User: mihais
 * Date: 2/25/15
 */
class TestFastNLPProcessorSpeed extends Test {
  var proc:Processor = new FastNLPProcessor(internStrings = true, withDiscourse = ShallowNLPProcessor.NO_DISCOURSE)

  "FastNLPProcessor" should "process a file successfully" in {
    val text = TestUtils.readFile("org/clulab/processors/raw_text.txt")
    val doc = proc.annotate(text)
    doc.sentences.length should be > 0
  }
}
