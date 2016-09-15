package org.clulab.processors

import org.clulab.discourse.rstparser.RelationDirection
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.struct.DirectedGraphEdgeIterator
import org.scalatest._
import org.clulab.processors.fastnlp.FastNLPProcessor

/**
 *
 * User: mihais
 * Date: 1/7/14
 */
class TestFastNLPProcessor extends FlatSpec with Matchers {
  var proc:Processor = new FastNLPProcessor(internStrings = true, withDiscourse = ShallowNLPProcessor.WITH_DISCOURSE)

  "FastNLPProcessor" should "generate correct dependencies in test sentence 1" in {
    val doc = proc.annotate("John Smith went to China.")

    // malt can generate only Stanford basic dependencies rather than collapsed ones
    // so, for example, we will see "prep" labels rather than "prep_to"
    //println(doc.sentences.head.dependencies)
    doc.sentences.head.dependencies.get.hasEdge(1, 0, "nn") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(2, 1, "nsubj") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(2, 4, "prep_to") should be (true)

    /*
    val it = new DirectedGraphEdgeIterator[String](doc.sentences.head.dependencies.get)
    while(it.hasNext) {
      val d = it.next()
      println(d._1 + " " + d._2 + " " + d._3)
    }
    */
  }

  "FastNLPProcessor" should "generate correct dependencies in test sentence 2" in {
    val doc = proc.annotate("He bought some shoes.")

    //println(doc.sentences.head.dependencies)
    doc.sentences.head.dependencies.get.hasEdge(1, 0, "nsubj") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(1, 3, "dobj") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(1, 4, "punct") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(3, 2, "det") should be (true)
  }

  "FastNLPProcessor" should "generate correct discourse relations in test 3" in {
    val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")
    doc.clear()

    val d = doc.discourseTree.get
    d.relationLabel should be ("elaboration")
    d.relationDirection should be (RelationDirection.LeftToRight)
    d.isTerminal should be (false)
    d.children.length should be (2)
  }
}
