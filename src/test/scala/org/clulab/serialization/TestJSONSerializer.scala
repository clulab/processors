package org.clulab.serialization

import org.clulab.odin.ExtractorEngine
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.serialization.json._
import org.scalatest._


class TestJSONSerializer extends FlatSpec with Matchers {

  val proc = new FastNLPProcessor
  val rules =
    """
      |# NE rules
      |
      |- name: "ner-person"
      |  label: [Person, PossiblePerson, Entity]
      |  priority: 1
      |  type: token
      |  pattern: |
      |   [entity="PERSON"]+
      |
      |# Events
      |
      |# optional location and date
      |- name: "marry-syntax-1"
      |  label: [Marry]
      |  priority: 3
      |  example: "He married Jane last June in Hawaii."
      |  type: dependency
      |  pattern: |
      |    trigger = [lemma="marry"]
      |    spouse: Entity+ = <xcomp? /^nsubj/ | dobj
    """.stripMargin
  val engine = ExtractorEngine(rules)
  val text = "Gonzo married Camilla."
  val doc = proc.annotate(text)
  val mentions = engine.extractFrom(doc)

  "JSONSerializer" should "serialize/deserialize a Document to/from json correctly" in {
    val d2 = JSONSerializer.toDocument(doc.jsonAST)
    d2.equivalenceHash should equal (doc.equivalenceHash)
  }

  it should "serialize/deserialize a Sentence to/from json correctly " in {
    val s2 = JSONSerializer.toSentence(doc.sentences.head.jsonAST)
    s2.equivalenceHash should equal (doc.sentences.head.equivalenceHash)
  }

  it should "serialize/deserialize a Mention to/from json correctly " in {
    val mns = JSONSerializer.toMentions(mentions.head.completeAST)
    mns should have size 1
    val m = mns.head
    m.document.equivalenceHash should equal (mentions.head.document.equivalenceHash)
    m.tokenInterval should equal (mentions.head.tokenInterval)
  }

  it should "serialize/deserialize a Seq[Mention] to/from json correctly " in {
    val mentions2 = JSONSerializer.toMentions(mentions.jsonAST)
    mentions2 should have size mentions.size

    mentions2.map(_.label) should equal (mentions.map(_.label))
    mentions2.map(_.document.equivalenceHash) should equal (mentions.map(_.document.equivalenceHash))
  }

  "A Document recovered from JSON" should "be equivalent to the original" in {

    val doc2 = JSONSerializer.toDocument(doc.jsonAST)

    doc.equivalenceHash should equal (doc2.equivalenceHash)
  }

  "A Sentence recovered from JSON" should "be equivalent to the original" in {

    val doc2 = JSONSerializer.toDocument(doc.jsonAST)

    doc.sentences should not be empty
    doc2.sentences should not be empty
    doc.sentences.head.equivalenceHash should equal (doc2.sentences.head.equivalenceHash)
  }

}
