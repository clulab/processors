package org.clulab.processors

import org.clulab.dynet.Utils
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.scalatest._
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles

/**
 *
 * User: mihais
 * Date: 1/7/14
 */
class TestFastNLPProcessor extends FlatSpec with Matchers {
  Utils.initializeDyNet()
  var proc:Processor = new FastNLPProcessorWithSemanticRoles(internStrings = true, withRelationExtraction = true, withDiscourse = ShallowNLPProcessor.WITH_DISCOURSE)

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

  it should "run the dependency parser correctly on texts with parentheses" in {
    val doc = proc.mkDocumentFromSentences(List("the tyrosine phosphorylation of pp60(c-src) is closely associated with the activation of phosphatidylinositol 3-kinase (PIK)."), keepText = false)
    proc.annotate(doc)
    doc.clear()

    println(doc.sentences.head.universalBasicDependencies.get)

    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 6, "appos") should be (true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(16, 18, "appos") should be (true)
  }

  it should "recognize semantic roles correctly" in {
    val doc = proc.annotate("John Doe visited China.")

    doc.sentences.head.semanticRoles.get.hasEdge(2, 1, "A0") should be (true)
    doc.sentences.head.semanticRoles.get.hasEdge(2, 3, "A1") should be (true)
  }

  it should "create semantic dependencies of the correct length" in {
    val text = "John ate cake, zz zz zz."
    val doc = proc.annotate(text)

    val sent = doc.sentences.head

    sent.semanticRoles.get.outgoingEdges.length should be(sent.size)
    sent.semanticRoles.get.incomingEdges.length should be(sent.size)
    sent.enhancedSemanticRoles.get.outgoingEdges.length should be(sent.size)
    sent.enhancedSemanticRoles.get.incomingEdges.length should be(sent.size)
  }
}
