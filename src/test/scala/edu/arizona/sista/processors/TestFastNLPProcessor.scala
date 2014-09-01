package edu.arizona.sista.processors

import edu.arizona.sista.discourse.rstparser.RelationDirection
import edu.arizona.sista.struct.DirectedGraphEdgeIterator
import org.scalatest.junit.AssertionsForJUnit
import org.junit.{Test, Before}
import org.junit.Assert._
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor

/**
 *
 * User: mihais
 * Date: 1/7/14
 */
class TestFastNLPProcessor extends AssertionsForJUnit {
  var proc:Processor = null

  @Before def constructProcessor() {
    proc = new FastNLPProcessor(internStrings = true, withDiscourse = true)
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

  @Test def testDiscourse() {
    val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")
    doc.clear()

    val d = doc.discourseTree.get
    assertTrue(d.relationLabel == "elaboration")
    assertTrue(d.relationDirection == RelationDirection.LeftToRight)
    assertTrue(! d.isTerminal && d.children.length == 2)
  }
}
