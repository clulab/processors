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

    doc.sentences.head.dependencies.get.hasEdge(1, 0, "compound") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(2, 1, "nsubj") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(2, 4, "nmod_to") should be (true)

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

  // For more information, see
  //   https://github.com/clulab/eidos/issues/261
  //   https://github.com/stanfordnlp/CoreNLP/issues/669
  //   https://github.com/stanfordnlp/CoreNLP/issues/83
  // This is fixed by props.put("maxAdditionalKnownLCWords", "0") in ShallowNLPProcessor.
  "FastNLPProcessor" should "have NER unaffected by state" in {
    val texts = Seq(
      "The highest potential areas for agricultural production are Western Equatoria and the southern half of Central Equatoria, or the so-called Green Belt, where annual rainfall ranges from 900 to 2,000 mm per year (Table 2.6).",
      "I have a green belt.",
      "The highest potential areas for agricultural production are Western Equatoria and the southern half of Central Equatoria, or the so-called Green Belt, where annual rainfall ranges from 900 to 2,000 mm per year (Table 2.6)."
    )

    def getEntitiesForWord(documents: Seq[Document], searchWord: String): Seq[String] = {
      val entities = for {
        doc <- documents
        sentence <- doc.sentences
        i <- sentence.words.indices
        if (sentence.words(i) == searchWord)
      }
      yield sentence.entities.get(i)

      entities
    }

    val docs = texts.map { proc.annotate(_) }
    val entities = getEntitiesForWord(docs, "Belt")

    entities.size >= 2 should be (true)
    entities.exists { entity => entity != entities.head } should be (false)
  }
}
