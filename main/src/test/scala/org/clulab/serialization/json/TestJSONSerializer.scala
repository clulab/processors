package org.clulab.serialization.json

import org.clulab.TestUtils.jsonStringToDocument
import org.json4s._
import org.scalatest._


class TestJSONSerializer extends FlatSpec with Matchers {

  val text = "Gonzo married Camilla."
  val doc = jsonStringToDocument(""" {"sentences":[{"words":["Gonzo","married","Camilla","."],"startOffsets":[0,6,14,21],"endOffsets":[5,13,21,22],"tags":["NNP","VBD","NNP","."],"lemmas":["Gonzo","marry","Camilla","."],"entities":["O","O","PERSON","O"],"norms":["O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"punct"}],"roots":[1]}}}]} """)

  "JSONSerializer" should "serialize/deserialize a Document to/from json correctly" in {
    val d2 = JSONSerializer.toDocument(doc.jsonAST)
    d2.equivalenceHash should equal (doc.equivalenceHash)
  }

  it should "serialize/deserialize a Sentence to/from json correctly " in {
    val s2 = JSONSerializer.toSentence(doc.sentences.head.jsonAST)
    s2.equivalenceHash should equal (doc.sentences.head.equivalenceHash)
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

}
