package org.clulab.odin

import org.clulab.TestUtils.jsonStringToDocument
import org.clulab.struct.Interval
import org.clulab.utils.Test

class TestMention extends Test {
  val rule =
    """
      |rules:
      | - name: test
      |   type: token
      |   label: TestMention
      |   pattern: |
      |     [lemma=I] []* [lemma=dance]
      |""".stripMargin

  val ee = ExtractorEngine(rule)

  behavior of "mention.text"

  // motivated by changes to the words field that replaced `'m` with `am`
  it should "properly reconstruct the original span" in {
    // I'm going to dance
    val json = """{"sentences":[{"words":["I","am","going","to","dance","."],"startOffsets":[0,1,4,10,13,18],"endOffsets":[1,3,9,12,18,19],"raw":["I","'m","going","to","dance","."],"tags":["PRP","VBP","VBG","TO","VB","."],"lemmas":["I","be","go","to","dance","."],"entities":["O","O","O","O","O","O"],"norms":["O","O","O","O","O","O"],"chunks":["B-NP","B-VP","I-VP","I-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"aux"},{"source":2,"destination":4,"relation":"xcomp"},{"source":2,"destination":5,"relation":"punct"},{"source":4,"destination":0,"relation":"nsubj:xsubj"},{"source":4,"destination":3,"relation":"mark"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"aux"},{"source":2,"destination":4,"relation":"xcomp"},{"source":2,"destination":5,"relation":"punct"},{"source":4,"destination":3,"relation":"mark"}],"roots":[2]}}}]}"""
    val doc = jsonStringToDocument(json)

    val mentions = ee.extractFrom(doc)
    mentions should have length (1)
    mentions.head.text shouldBe "I'm going to dance"
    val head = mentions.head.synHead
    mentions.head.distToRootOpt shouldBe (Some(0))
  }

  behavior of "Mention.getRootDistOpt"

  it should "get None when there are no roots" in {
    // 2 is wrapped to 2 once here so that it isn't a root.
    val json = """{
      |"sentences":[{
      |  "words":["I","am","going","to","dance","."],
      |  "startOffsets":[0,1,4,10,13,18],
      |  "endOffsets":[1,3,9,12,18,19],
      |  "raw":["I","'m","going","to","dance","."],
      |  "tags":["PRP","VBP","VBG","TO","VB","."],
      |  "lemmas":["I","be","go","to","dance","."],
      |  "entities":["O","O","O","O","O","O"],
      |  "norms":["O","O","O","O","O","O"],
      |  "chunks":["B-NP","B-VP","I-VP","I-VP","I-VP","O"],
      |  "graphs":{
      |    "universal-enhanced":{"edges":[],"roots":[]},
      |    "universal-basic":{"edges":[],"roots":[]}}
      |  }]
      |}"""
      .stripMargin
    val doc = jsonStringToDocument(json)
    val mention = ee.extractFrom(doc).head

    // The original value for the roots is incorrect.  They are recalculated during deserialization.
    // Without any edges, all six words are roots and the distance to the root is 0.
    mention.distToRootOpt shouldBe (Some(0))
  }

  it should "get None when the Interval is empty" in {
    val json = """{"sentences":[{"words":["I","am","going","to","dance","."],"startOffsets":[0,1,4,10,13,18],"endOffsets":[1,3,9,12,18,19],"raw":["I","'m","going","to","dance","."],"tags":["PRP","VBP","VBG","TO","VB","."],"lemmas":["I","be","go","to","dance","."],"entities":["O","O","O","O","O","O"],"norms":["O","O","O","O","O","O"],"chunks":["B-NP","B-VP","I-VP","I-VP","I-VP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"aux"},{"source":2,"destination":4,"relation":"xcomp"},{"source":2,"destination":5,"relation":"punct"},{"source":4,"destination":0,"relation":"nsubj:xsubj"},{"source":4,"destination":3,"relation":"mark"}],"roots":[2]},"universal-basic":{"edges":[{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"aux"},{"source":2,"destination":4,"relation":"xcomp"},{"source":2,"destination":5,"relation":"punct"},{"source":4,"destination":3,"relation":"mark"}],"roots":[2]}}}]}"""
    val doc = jsonStringToDocument(json)
    val mention = ee.extractFrom(doc).head.asInstanceOf[TextBoundMention].copy(tokenInterval = Interval(0, 0))

    mention.distToRootOpt shouldBe (None)
  }
}
