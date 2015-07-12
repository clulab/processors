package edu.arizona.sista.processors

import edu.arizona.sista.discourse.rstparser.RelationDirection
import org.scalatest._
import collection.JavaConversions.asJavaCollection
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor

/**
 * 
 * User: mihais
 * Date: 3/3/13
 */
class TestCoreNLPProcessor extends FlatSpec with Matchers {
  var proc:Processor = new CoreNLPProcessor(internStrings = true, withDiscourse = true)

  "CoreNLPProcessor" should "tokenize raw text correctly" in {
    val doc = proc.mkDocument("John Doe went to China. There, he visited Beijing.", keepText = false)
    doc.clear()

    doc.sentences(0).words(0) should be ("John")
    doc.sentences(0).words(1) should be ("Doe")
    doc.sentences(0).words(2) should be ("went")
    doc.sentences(0).words(3) should be ("to")
    doc.sentences(0).words(4) should be ("China")
    doc.sentences(0).words(5) should be (".")
    doc.sentences(1).words(0) should be ("There")
    doc.sentences(1).words(1) should be (",")
    doc.sentences(1).words(2) should be ("he")
    doc.sentences(1).words(3) should be ("visited")
    doc.sentences(1).words(4) should be ("Beijing")
    doc.sentences(1).words(5) should be (".")

    doc.sentences(0).startOffsets(0) should be (0)
    doc.sentences(0).startOffsets(1) should be (5)
    doc.sentences(0).startOffsets(2) should be (9)
    doc.sentences(0).startOffsets(3) should be (14)
    doc.sentences(0).startOffsets(4) should be (17)
    doc.sentences(0).startOffsets(5) should be (22)
    doc.sentences(1).startOffsets(0) should be (24)
    doc.sentences(1).startOffsets(1) should be (29)
    doc.sentences(1).startOffsets(2) should be (31)
    doc.sentences(1).startOffsets(3) should be (34)
    doc.sentences(1).startOffsets(4) should be (42)
    doc.sentences(1).startOffsets(5) should be (49)
  }

  it should "tokenize a list of sentences correctly" in {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China.", "There, he visited Beijing."), keepText = false)
    doc.clear()

    doc.sentences(0).words(0) should be ("John")
    doc.sentences(0).words(1) should be ("Doe")
    doc.sentences(0).words(2) should be ("went")
    doc.sentences(0).words(3) should be ("to")
    doc.sentences(0).words(4) should be ("China")
    doc.sentences(0).words(5) should be (".")
    doc.sentences(1).words(0) should be ("There")
    doc.sentences(1).words(1) should be (",")
    doc.sentences(1).words(2) should be ("he")
    doc.sentences(1).words(3) should be ("visited")
    doc.sentences(1).words(4) should be ("Beijing")
    doc.sentences(1).words(5) should be (".")

    doc.sentences(0).startOffsets(0) should be (0)
    doc.sentences(0).startOffsets(1) should be (5)
    doc.sentences(0).startOffsets(2) should be (9)
    doc.sentences(0).startOffsets(3) should be (14)
    doc.sentences(0).startOffsets(4) should be (17)
    doc.sentences(0).startOffsets(5) should be (22)
    doc.sentences(1).startOffsets(0) should be (24)
    doc.sentences(1).startOffsets(1) should be (29)
    doc.sentences(1).startOffsets(2) should be (31)
    doc.sentences(1).startOffsets(3) should be (34)
    doc.sentences(1).startOffsets(4) should be (42)
    doc.sentences(1).startOffsets(5) should be (49)
  }

  it should "tokenize sequences of tokens correctly" in {
    val doc = proc.mkDocumentFromTokens(List(
      List("John", "Doe", "went", "to", "China", "."),
      List("There", ",", "he", "visited", "Beijing", ".")), keepText = false)
    doc.clear()

    doc.sentences(0).words(0) should be ("John")
    doc.sentences(0).words(1) should be ("Doe")
    doc.sentences(0).words(2) should be ("went")
    doc.sentences(0).words(3) should be ("to")
    doc.sentences(0).words(4) should be ("China")
    doc.sentences(0).words(5) should be (".")
    doc.sentences(1).words(0) should be ("There")
    doc.sentences(1).words(1) should be (",")
    doc.sentences(1).words(2) should be ("he")
    doc.sentences(1).words(3) should be ("visited")
    doc.sentences(1).words(4) should be ("Beijing")
    doc.sentences(1).words(5) should be (".")

    doc.sentences(0).startOffsets(0) should be (0)
    doc.sentences(0).startOffsets(1) should be (5)
    doc.sentences(0).startOffsets(2) should be (9)
    doc.sentences(0).startOffsets(3) should be (14)
    doc.sentences(0).startOffsets(4) should be (17)
    doc.sentences(0).startOffsets(5) should be (23)
    doc.sentences(1).startOffsets(0) should be (25)
    doc.sentences(1).startOffsets(1) should be (31)
    doc.sentences(1).startOffsets(2) should be (33)
    doc.sentences(1).startOffsets(3) should be (36)
    doc.sentences(1).startOffsets(4) should be (44)
    doc.sentences(1).startOffsets(5) should be (52)
  }

  it should "POS tag correctly" in {
    val doc = proc.mkDocument("John Doe went to China. There, he visited Beijing.", keepText = false)
    proc.tagPartsOfSpeech(doc)
    doc.clear()

    doc.sentences(0).tags.get(0) should be ("NNP")
    doc.sentences(0).tags.get(1) should be ("NNP")
    doc.sentences(0).tags.get(2) should be ("VBD")
    doc.sentences(0).tags.get(3) should be ("TO")
    doc.sentences(0).tags.get(4) should be ("NNP")
    doc.sentences(0).tags.get(5) should be (".")
    doc.sentences(1).tags.get(0) should be ("RB")
    doc.sentences(1).tags.get(1) should be (",")
    doc.sentences(1).tags.get(2) should be ("PRP")
    doc.sentences(1).tags.get(3) should be ("VBD")
    doc.sentences(1).tags.get(4) should be ("NNP")
    doc.sentences(1).tags.get(5) should be (".")
  }

  it should "lemmatize correctly" in {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China.", "There, he visited Beijing."), keepText = false)
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    doc.clear()

    doc.sentences(0).lemmas.get(0) should be ("John")
    doc.sentences(0).lemmas.get(1) should be ("Doe")
    doc.sentences(0).lemmas.get(2) should be ("go")
    doc.sentences(0).lemmas.get(3) should be ("to")
    doc.sentences(0).lemmas.get(4) should be ("China")
    doc.sentences(0).lemmas.get(5) should be (".")
    doc.sentences(1).lemmas.get(0) should be ("there")
    doc.sentences(1).lemmas.get(1) should be (",")
    doc.sentences(1).lemmas.get(2) should be ("he")
    doc.sentences(1).lemmas.get(3) should be ("visit")
    doc.sentences(1).lemmas.get(4) should be ("Beijing")
    doc.sentences(1).lemmas.get(5) should be (".")
  }

  it should "recognize NEs correctly" in {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China on January 15th, 2001.", "There, he visited Beijing."), keepText = false)
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    doc.clear()

    doc.sentences(0).entities.get(0) should be ("PERSON")
    doc.sentences(0).entities.get(1) should be ("PERSON")
    doc.sentences(0).entities.get(2) should be ("O")
    doc.sentences(0).entities.get(3) should be ("O")
    doc.sentences(0).entities.get(4) should be ("LOCATION")
    doc.sentences(0).entities.get(5) should be ("O")
    doc.sentences(0).entities.get(6) should be ("DATE")
    doc.sentences(0).entities.get(7) should be ("DATE")
    doc.sentences(0).entities.get(8) should be ("DATE")
    doc.sentences(0).entities.get(9) should be ("DATE")
    doc.sentences(0).entities.get(10) should be ("O")
    doc.sentences(1).entities.get(0) should be ("O")
    doc.sentences(1).entities.get(1) should be ("O")
    doc.sentences(1).entities.get(2) should be ("O")
    doc.sentences(1).entities.get(3) should be ("O")
    doc.sentences(1).entities.get(4) should be ("LOCATION")
    doc.sentences(1).entities.get(5) should be ("O")

    doc.sentences(0).norms.get(5) should be ("O")
    doc.sentences(0).norms.get(6) should be ("2001-01-15")
    doc.sentences(0).norms.get(7) should be ("2001-01-15")
    doc.sentences(0).norms.get(8) should be ("2001-01-15")
    doc.sentences(0).norms.get(9) should be ("2001-01-15")
    doc.sentences(0).norms.get(10) should be ("O")
  }

  it should "run the constituent parser correctly" in {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China"), keepText = false)
    proc.parse(doc)
    doc.clear()

    doc.sentences.head.dependencies.get.hasEdge(1, 0, "nn") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(2, 1, "nsubj") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(2, 4, "prep_to") should be (true)
    doc.sentences.head.dependencies.get.hasEdge(2, 3, "obj") should be (false)

    doc.sentences.head.syntacticTree.foreach(t => {
      //println("Constituent parse tree: " + t)
      t.isUnary should be (true)
      t.value should be ("ROOT")
      val s = t.children.get(0)
      s.headOffset should be (2)
      s.children.get.length should be (2)
      s.head should be (1)
      s.startOffset should be (0)
      s.endOffset should be (5)

      val vp = s.children.get(1)
      vp.value should be ("VP")
      vp.headOffset should be (2)
      vp.children.get.length should be (2)
      vp.head should be (0)
      vp.startOffset should be (2)
      vp.endOffset should be (5)
    })
  }

  it should "run the coreference resolver correctly" in {
    val doc = proc.annotate("John Smith went to China. He visited Beijing.")
    (doc.coreferenceChains != None) should be (true)

    val mentions = doc.coreferenceChains.get.getMentions
    mentions.size should be (4)

    val chain = doc.coreferenceChains.get.getChain(0, 1)
    (chain != None) should be (true)

    val john = new CorefMention(0, 1, 0, 2, -1)
    chain.get.contains(john) should be (true)
    mentions.contains(john) should be (true)

    val he = new CorefMention(1, 0, 0, 1, -1)
    chain.get.contains(he) should be (true)
    mentions.contains(he) should be (true)

    val china = new CorefMention(0, 4, 4, 5, -1)
    mentions.contains(china) should be (true)

    val beijing = new CorefMention(1, 2, 2, 3, -1)
    mentions.contains(beijing) should be (true)
  }

  it should "assign head words to constituent phrases correctly" in {
    val doc = proc.mkDocumentFromSentences(List("John Doe went to China"), keepText = false)
    proc.parse(doc)
    doc.clear()

    doc.sentences.head.syntacticTree.foreach(t => {
      val s = t.children.get.head
      //println("S constituent is: " + s)
      s.head should be (1)
      val np = s.children.get.head
      //println("NP constituent is: " + np)
      np.head should be (1)
      val vp = s.children.get(1)
      //println("VP constituent is: " + vp)
      vp.head should be (0)
    })
  }

  it should "parse discourse relations correctly" in {
    val doc = proc.annotate("John Smith went to China. He visited Beijing, on January 10th, 2013.")
    doc.clear()

    val d = doc.discourseTree.get
    d.relationLabel should be ("elaboration")
    d.relationDirection should be (RelationDirection.LeftToRight)
    d.isTerminal should be (false)
    d.children.length should be (2)
  }

  it should "create document text correctly" in {
    val doc = proc.annotateFromSentences(List("Sentence 1.", "Sentence 2."), keepText = true)
    doc.text.get should be ("Sentence 1. Sentence 2.")
  }
}
