package org.clulab.serialization

import org.clulab.TestUtils.jsonStringToDocument
import org.clulab.odin.ExtractorEngine
import org.clulab.serialization.json._
import org.json4s._
import org.scalatest._


class TestJSONSerializer extends FlatSpec with Matchers {

  val rules =
    """
      |# NE rules
      |
      |- name: "ner-person"
      |  label: [Person, PossiblePerson, Entity]
      |  priority: 1
      |  type: token
      |  pattern: |
      |   ([entity="PERSON"]+ | "Gonzo")
      |
      |# Events
      |
      |# optional location and date
      |- name: "marry-syntax-1"
      |  label: [Marry, Event]
      |  priority: 3
      |  example: "Gonzo married Camilla."
      |  type: dependency
      |  pattern: |
      |    trigger = [lemma="marry"]
      |    spouse: Entity+ = <xcomp? /^nsubj/ | dobj
    """.stripMargin
  val engine = ExtractorEngine(rules)
  val text = "Gonzo married Camilla."
  val doc = jsonStringToDocument(""" {"sentences":[{"words":["Gonzo","married","Camilla","."],"startOffsets":[0,6,14,21],"endOffsets":[5,13,21,22],"tags":["NNP","VBD","NNP","."],"lemmas":["Gonzo","marry","Camilla","."],"entities":["O","O","PERSON","O"],"norms":["O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]}}}]} """)
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

  "A Document with an ID" should "produce json with an \"id\" field" in {
    val d = jsonStringToDocument(""" {"sentences":[{"words":["Gonzo","married","Camilla","."],"startOffsets":[0,6,14,21],"endOffsets":[5,13,21,22],"tags":["NNP","VBD","NNP","."],"lemmas":["Gonzo","marry","Camilla","."],"entities":["O","O","PERSON","O"],"norms":["O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]}}}]} """)
    d.id = Some("this-is-an-id")
    (d.jsonAST \ "id") should equal (JString("this-is-an-id"))
  }

  "A Document without an ID" should "produce json without an \"id\" field" in {
    val d = jsonStringToDocument(""" {"sentences":[{"words":["Gonzo","married","Camilla","."],"startOffsets":[0,6,14,21],"endOffsets":[5,13,21,22],"tags":["NNP","VBD","NNP","."],"lemmas":["Gonzo","marry","Camilla","."],"entities":["O","O","PERSON","O"],"norms":["O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]}}}]} """)
    (d.jsonAST \ "id") should equal (JNothing)
  }

  "A Document with text" should "produce json with a \"text\" field" in {
    val d = jsonStringToDocument(""" {"sentences":[{"words":["Gonzo","married","Camilla","."],"startOffsets":[0,6,14,21],"endOffsets":[5,13,21,22],"tags":["NNP","VBD","NNP","."],"lemmas":["Gonzo","marry","Camilla","."],"entities":["O","O","PERSON","O"],"norms":["O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]}}}]} """)
    d.text = Some(text)
    (d.jsonAST \ "text") should equal (JString(text))
  }

  "A Document without text" should "produce json without a \"text\" field" in {
    val d = jsonStringToDocument(""" {"sentences":[{"words":["Gonzo","married","Camilla","."],"startOffsets":[0,6,14,21],"endOffsets":[5,13,21,22],"tags":["NNP","VBD","NNP","."],"lemmas":["Gonzo","marry","Camilla","."],"entities":["O","O","PERSON","O"],"norms":["O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]}}}]} """)
    (d.jsonAST \ "text") should equal (JNothing)
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

  "When non-empty, Mention.paths" should "be represented in Mention's json" in {
    (mentions.jsonAST \ "arguments" \\ "paths") should not equal JNothing
  }

  "serialization.json.MentionOps" should "produce an invariant id for a serialized/deserialzed Mention" in {
    val m = mentions.head
    val mns2 = JSONSerializer.toMentions(Seq(m).jsonAST)
    mns2 should not be empty
    mns2 should have size 1
    m.id should equal (mns2.head.id)
  }

}
