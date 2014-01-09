package edu.arizona.sista.processor

import org.scalatest.junit.AssertionsForJUnit
import edu.arizona.sista.processor.Processor
import org.junit.{Test, Before}
import org.junit.Assert._
import edu.arizona.sista.processor.corenlp.CoreNLPProcessor
import edu.arizona.sista.processor.struct.DirectedGraphEdgeIterator
import edu.arizona.sista.processor.fastnlp.FastNLPProcessor

/**
 *
 * User: mihais
 * Date: 1/7/14
 */
class TestFastNLPProcessor extends AssertionsForJUnit {
  var proc:Processor = null

  @Before def constructProcessor() {
    proc = new FastNLPProcessor(internStrings = true)
  }

  @Test def testParser1() {
    val doc = proc.annotate("John Smith went to China.")

    // malt can generate only Stanford basic dependencies rather than collapsed ones
    // so, for example, we will see "prep" labels rather than "prep_to"
    //println(doc.sentences.head.dependencies)
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(1, 0, "nn"))
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(2, 1, "nsubj"))
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(2, 3, "prep"))
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(3, 4, "pobj"))

    val it = new DirectedGraphEdgeIterator[String](doc.sentences.head.dependencies.get)
    while(it.hasNext) {
      val d = it.next()
      println(d._1 + " " + d._2 + " " + d._3)
    }
  }

  @Test def testParser2() {
    val doc = proc.annotate("He bought some shoes.")

    //println(doc.sentences.head.dependencies)
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(1, 0, "nsubj"))
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(1, 3, "dobj"))
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(1, 4, "punct"))
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(3, 2, "det"))

    val it = new DirectedGraphEdgeIterator[String](doc.sentences.head.dependencies.get)
    while(it.hasNext) {
      val d = it.next()
      println(d._1 + " " + d._2 + " " + d._3)
    }
  }
}
