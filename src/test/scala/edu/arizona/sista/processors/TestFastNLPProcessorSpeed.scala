package edu.arizona.sista.processors

import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import org.junit.{Test, Before}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._


import scala.io.Source

/**
 * This is a pseudo test; I just wanted to run FastNLPProcessor on a big file
 * User: mihais
 * Date: 2/25/15
 */
class TestFastNLPProcessorSpeed extends AssertionsForJUnit {
  var proc:Processor = null

  @Before def constructProcessor() {
    proc = new FastNLPProcessor(internStrings = true, withDiscourse = false, useMalt = false)
  }

  @Test def testParser1(): Unit = {
    val text = Source.fromFile("src/test/resources/edu/arizona/sista/processors/raw_text.txt").getLines.mkString(" ")
    println("Started parsing the big file...")
    val doc = proc.annotate(text)
    assertTrue(true)
  }

}
