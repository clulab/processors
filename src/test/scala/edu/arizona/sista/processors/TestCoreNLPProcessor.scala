package edu.arizona.sista.processors

import edu.arizona.sista.discourse.rstparser.RelationDirection
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.{Before, Test}
import collection.JavaConversions.asJavaCollection
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

/**
 * 
 * User: mihais
 * Date: 3/3/13
 */
class TestCoreNLPProcessor extends AssertionsForJUnit {
  var proc:Processor = new CoreNLPProcessor(internStrings = true, withDiscourse = true)

  @Test def testTokenOffsets1() {
    val doc = proc.mkDocument("John Doe went to China. There, he visited Beijing.")
    doc.clear()

    assertEquals(doc.sentences(0).words(0), "John")
    assertEquals(doc.sentences(0).words(1), "Doe")
    assertEquals(doc.sentences(0).words(2), "went")
    assertEquals(doc.sentences(0).words(3), "to")
    assertEquals(doc.sentences(0).words(4), "China")
    assertEquals(doc.sentences(0).words(5), ".")
    assertEquals(doc.sentences(1).words(0), "There")
    assertEquals(doc.sentences(1).words(1), ",")
    assertEquals(doc.sentences(1).words(2), "he")
    assertEquals(doc.sentences(1).words(3), "visited")
    assertEquals(doc.sentences(1).words(4), "Beijing")
    assertEquals(doc.sentences(1).words(5), ".")

    assertEquals(doc.sentences(0).startOffsets(0), 0)
    assertEquals(doc.sentences(0).startOffsets(1), 5)
    assertEquals(doc.sentences(0).startOffsets(2), 9)
    assertEquals(doc.sentences(0).startOffsets(3), 14)
    assertEquals(doc.sentences(0).startOffsets(4), 17)
    assertEquals(doc.sentences(0).startOffsets(5), 22)
    assertEquals(doc.sentences(1).startOffsets(0), 24)
    assertEquals(doc.sentences(1).startOffsets(1), 29)
    assertEquals(doc.sentences(1).startOffsets(2), 31)
    assertEquals(doc.sentences(1).startOffsets(3), 34)
    assertEquals(doc.sentences(1).startOffsets(4), 42)
    assertEquals(doc.sentences(1).startOffsets(5), 49)
  }

  @Test def testTokenOffsets2() {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China.", "There, he visited Beijing."))
    doc.clear()

    assertEquals(doc.sentences(0).words(0), "John")
    assertEquals(doc.sentences(0).words(1), "Doe")
    assertEquals(doc.sentences(0).words(2), "went")
    assertEquals(doc.sentences(0).words(3), "to")
    assertEquals(doc.sentences(0).words(4), "China")
    assertEquals(doc.sentences(0).words(5), ".")
    assertEquals(doc.sentences(1).words(0), "There")
    assertEquals(doc.sentences(1).words(1), ",")
    assertEquals(doc.sentences(1).words(2), "he")
    assertEquals(doc.sentences(1).words(3), "visited")
    assertEquals(doc.sentences(1).words(4), "Beijing")
    assertEquals(doc.sentences(1).words(5), ".")

    assertEquals(doc.sentences(0).startOffsets(0), 0)
    assertEquals(doc.sentences(0).startOffsets(1), 5)
    assertEquals(doc.sentences(0).startOffsets(2), 9)
    assertEquals(doc.sentences(0).startOffsets(3), 14)
    assertEquals(doc.sentences(0).startOffsets(4), 17)
    assertEquals(doc.sentences(0).startOffsets(5), 22)
    assertEquals(doc.sentences(1).startOffsets(0), 24)
    assertEquals(doc.sentences(1).startOffsets(1), 29)
    assertEquals(doc.sentences(1).startOffsets(2), 31)
    assertEquals(doc.sentences(1).startOffsets(3), 34)
    assertEquals(doc.sentences(1).startOffsets(4), 42)
    assertEquals(doc.sentences(1).startOffsets(5), 49)
  }

  @Test def testTokenOffsets3() {
    val doc = proc.mkDocumentFromTokens(List(
      List("John", "Doe", "went", "to", "China", "."),
      List("There", ",", "he", "visited", "Beijing", ".")))
    doc.clear()

    assertEquals(doc.sentences(0).words(0), "John")
    assertEquals(doc.sentences(0).words(1), "Doe")
    assertEquals(doc.sentences(0).words(2), "went")
    assertEquals(doc.sentences(0).words(3), "to")
    assertEquals(doc.sentences(0).words(4), "China")
    assertEquals(doc.sentences(0).words(5), ".")
    assertEquals(doc.sentences(1).words(0), "There")
    assertEquals(doc.sentences(1).words(1), ",")
    assertEquals(doc.sentences(1).words(2), "he")
    assertEquals(doc.sentences(1).words(3), "visited")
    assertEquals(doc.sentences(1).words(4), "Beijing")
    assertEquals(doc.sentences(1).words(5), ".")

    assertEquals(doc.sentences(0).startOffsets(0), 0)
    assertEquals(doc.sentences(0).startOffsets(1), 5)
    assertEquals(doc.sentences(0).startOffsets(2), 9)
    assertEquals(doc.sentences(0).startOffsets(3), 14)
    assertEquals(doc.sentences(0).startOffsets(4), 17)
    assertEquals(doc.sentences(0).startOffsets(5), 23)
    assertEquals(doc.sentences(1).startOffsets(0), 25)
    assertEquals(doc.sentences(1).startOffsets(1), 31)
    assertEquals(doc.sentences(1).startOffsets(2), 33)
    assertEquals(doc.sentences(1).startOffsets(3), 36)
    assertEquals(doc.sentences(1).startOffsets(4), 44)
    assertEquals(doc.sentences(1).startOffsets(5), 52)
  }

  @Test def testPartsOfSpeech() {
    val doc = proc.mkDocument("John Doe went to China. There, he visited Beijing.")
    proc.tagPartsOfSpeech(doc)
    doc.clear()

    assertEquals(doc.sentences(0).tags.get(0), "NNP")
    assertEquals(doc.sentences(0).tags.get(1), "NNP")
    assertEquals(doc.sentences(0).tags.get(2), "VBD")
    assertEquals(doc.sentences(0).tags.get(3), "TO")
    assertEquals(doc.sentences(0).tags.get(4), "NNP")
    assertEquals(doc.sentences(0).tags.get(5), ".")
    assertEquals(doc.sentences(1).tags.get(0), "RB")
    assertEquals(doc.sentences(1).tags.get(1), ",")
    assertEquals(doc.sentences(1).tags.get(2), "PRP")
    assertEquals(doc.sentences(1).tags.get(3), "VBD")
    assertEquals(doc.sentences(1).tags.get(4), "NNP")
    assertEquals(doc.sentences(1).tags.get(5), ".")
  }

  @Test def testLemmas() {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China.", "There, he visited Beijing."))
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    doc.clear()

    assertEquals(doc.sentences(0).lemmas.get(0), "John")
    assertEquals(doc.sentences(0).lemmas.get(1), "Doe")
    assertEquals(doc.sentences(0).lemmas.get(2), "go")
    assertEquals(doc.sentences(0).lemmas.get(3), "to")
    assertEquals(doc.sentences(0).lemmas.get(4), "China")
    assertEquals(doc.sentences(0).lemmas.get(5), ".")
    assertEquals(doc.sentences(1).lemmas.get(0), "there")
    assertEquals(doc.sentences(1).lemmas.get(1), ",")
    assertEquals(doc.sentences(1).lemmas.get(2), "he")
    assertEquals(doc.sentences(1).lemmas.get(3), "visit")
    assertEquals(doc.sentences(1).lemmas.get(4), "Beijing")
    assertEquals(doc.sentences(1).lemmas.get(5), ".")
  }

  @Test def testNER() {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China on January 15th, 2001.", "There, he visited Beijing."))
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    doc.clear()

    assertEquals(doc.sentences(0).entities.get(0), "PERSON")
    assertEquals(doc.sentences(0).entities.get(1), "PERSON")
    assertEquals(doc.sentences(0).entities.get(2), "O")
    assertEquals(doc.sentences(0).entities.get(3), "O")
    assertEquals(doc.sentences(0).entities.get(4), "LOCATION")
    assertEquals(doc.sentences(0).entities.get(5), "O")
    assertEquals(doc.sentences(0).entities.get(6), "DATE")
    assertEquals(doc.sentences(0).entities.get(7), "DATE")
    assertEquals(doc.sentences(0).entities.get(8), "DATE")
    assertEquals(doc.sentences(0).entities.get(9), "DATE")
    assertEquals(doc.sentences(0).entities.get(10), "O")
    assertEquals(doc.sentences(1).entities.get(0), "O")
    assertEquals(doc.sentences(1).entities.get(1), "O")
    assertEquals(doc.sentences(1).entities.get(2), "O")
    assertEquals(doc.sentences(1).entities.get(3), "O")
    assertEquals(doc.sentences(1).entities.get(4), "LOCATION")
    assertEquals(doc.sentences(1).entities.get(5), "O")

    assertEquals(doc.sentences(0).norms.get(5), "O")
    assertEquals(doc.sentences(0).norms.get(6), "2001-01-15")
    assertEquals(doc.sentences(0).norms.get(7), "2001-01-15")
    assertEquals(doc.sentences(0).norms.get(8), "2001-01-15")
    assertEquals(doc.sentences(0).norms.get(9), "2001-01-15")
    assertEquals(doc.sentences(0).norms.get(10), "O")
  }

  @Test def testParse() {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China"))
    proc.parse(doc)
    doc.clear()

    assertTrue(doc.sentences.head.dependencies.get.hasEdge(1, 0, "nn"))
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(2, 1, "nsubj"))
    assertTrue(doc.sentences.head.dependencies.get.hasEdge(2, 4, "prep_to"))
    assertFalse(doc.sentences.head.dependencies.get.hasEdge(2, 3, "obj"))

    doc.sentences.head.syntacticTree.foreach(t => {
      println("Constituent parse tree: " + t)
      assertTrue(t.isUnary)
      assertTrue(t.value == "ROOT")
      val s = t.children.get(0)
      assertTrue(s.headOffset == 2)
      assertTrue(s.children.get.length == 2)
      assertTrue(s.head == 1)
      assertTrue(s.startOffset == 0)
      assertTrue(s.endOffset == 5)

      val vp = s.children.get(1)
      assertTrue(vp.value == "VP")
      assertTrue(vp.headOffset == 2)
      assertTrue(vp.children.get.length == 2)
      assertTrue(vp.head == 0)
      assertTrue(vp.startOffset == 2)
      assertTrue(vp.endOffset == 5)
    })
  }

  @Test def testCoreference() {
    val doc = proc.annotate("John Smith went to China. He visited Beijing.")
    assertTrue(doc.coreferenceChains != None)

    val mentions = doc.coreferenceChains.get.getMentions
    assertTrue(mentions.size == 4)

    val chain = doc.coreferenceChains.get.getChain(0, 1)
    assertTrue(chain != None)

    val john = new CorefMention(0, 1, 0, 2, -1)
    assertTrue(chain.get.contains(john))
    assertTrue(mentions.contains(john))

    val he = new CorefMention(1, 0, 0, 1, -1)
    assertTrue(chain.get.contains(he))
    assertTrue(mentions.contains(he))

    val china = new CorefMention(0, 4, 4, 5, -1)
    assertTrue(mentions.contains(china))

    val beijing = new CorefMention(1, 2, 2, 3, -1)
    assertTrue(mentions.contains(beijing))
  }

  @Test def testHeadWords() {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China"))
    proc.parse(doc)
    doc.clear()

    doc.sentences.head.syntacticTree.foreach(t => {
      val s = t.children.get.head
      println("S constituent is: " + s)
      assertTrue(s.head == 1)
      val np = s.children.get.head
      println("NP constituent is: " + np)
      assertTrue(np.head == 1)
      val vp = s.children.get(1)
      println("VP constituent is: " + vp)
      assertTrue(vp.head == 0)
    })
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
