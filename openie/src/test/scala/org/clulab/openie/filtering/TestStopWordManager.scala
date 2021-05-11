package org.clulab.openie.filtering

import org.clulab.openie.IETestUtils.jsonStringToDocument
import org.clulab.struct.Interval
import org.scalatest.{FlatSpec, Matchers}

class TestStopWordManager extends FlatSpec with Matchers {

  behavior of "StopWordManager"
  val stopManager: StopWordManager = StopWordManager.fromConfig()

  it should "identify words as stops" in {
    val words = Seq("apple", "try", "with", "type")
    words.map(stopManager.isStopWord) shouldBe Seq(false, false, true, false)
  }

  it should "identify words as transparent" in {
    val words = Seq("apple", "try", "with", "type")
    words.map(stopManager.isTransparentWord) shouldBe Seq(false, false, false, true)
  }

  it should "identify words as stop or transparent" in {
    val words = Seq("apple", "try", "with", "type")
    words.map(stopManager.isStopOrTransparentWord) shouldBe Seq(false, false, true, true)
  }

  it should "identify stop entities if populated" in {
    val entities = Seq("O", "O", "PERSON", "DURATION")
    entities.map(stopManager.isStopNER) shouldBe Seq(false, false, false, true)
  }

  it should "identify stop entities from seq if populated" in {
    val entities = Some(Array("O", "O", "PERSON", "DURATION"))
    stopManager.isStopNER(entities, 0) shouldBe false
    stopManager.isStopNER(entities, 2) shouldBe false
    stopManager.isStopNER(entities, 3) shouldBe true
  }

  it should "not flag entity as stop if not populated" in {
    val entities: Option[Array[String]] = None
    stopManager.isStopNER(entities, 2) shouldBe false
  }

  it should "identify contentful tags vs non-contentful" in {
    val tags = Seq("NNP", "NNS", "VBZ", "JJS", "DT")
    tags.map(stopManager.isContentTag) shouldBe Seq(true, true, true, true, false)
  }

  it should "identify if there is non-stop contentwith Sentences" in {
    //sentence 1: "a sentence with content"
    //sentence 2: "type of level"
    val doc = jsonStringToDocument("""{"sentences":[{"words":["a","sentence","with","content","."],"startOffsets":[0,2,11,16,23],"endOffsets":[1,10,15,23,24],"raw":["a","sentence","with","content","."],"tags":["DT","NN","IN","NN","."],"lemmas":["a","sentence","with","content","."],"entities":["O","O","O","O","O"],"norms":["O","O","O","O","O"],"chunks":["B-NP","I-NP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":1,"destination":0,"relation":"det"},{"source":1,"destination":3,"relation":"nmod_with"},{"source":1,"destination":4,"relation":"punct"},{"source":3,"destination":2,"relation":"case"}],"roots":[1]},"universal-basic":{"edges":[{"source":1,"destination":0,"relation":"det"},{"source":1,"destination":3,"relation":"nmod"},{"source":1,"destination":4,"relation":"punct"},{"source":3,"destination":2,"relation":"case"}],"roots":[1]}}},{"words":["type","of","level","."],"startOffsets":[25,30,33,38],"endOffsets":[29,32,38,39],"raw":["type","of","level","."],"tags":["NN","IN","NN","."],"lemmas":["type","of","level","."],"entities":["O","O","O","O"],"norms":["O","O","O","O"],"chunks":["B-NP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":1,"relation":"case"},{"source":0,"destination":2,"relation":"nmod_of"},{"source":0,"destination":3,"relation":"punct"}],"roots":[0]},"universal-basic":{"edges":[{"source":2,"destination":1,"relation":"case"},{"source":0,"destination":2,"relation":"nmod"},{"source":0,"destination":3,"relation":"punct"}],"roots":[0]}}}]}""")
    val s1 = doc.sentences.head
    val s2 = doc.sentences.last
    stopManager.hasNonStopContent(s1, Interval(0, s1.size)) shouldBe(true)
    stopManager.hasNonStopContent(s2, Interval(0, s2.size)) shouldBe(false)
  }

  it should "identify if there is non-stop content with lists" in {
    //sentence 1: "a sentence with content"
    val l1 = Seq("a","sentence","with","content",".")
    val t1 = Seq("DT","NN","IN","NN",".")
    val e1 = Array("O","O","O","O","O")
    //sentence 2: "type of level"
    val l2 = Seq("type","of","level")
    val t2 = Seq("NN","IN","NN")
    val e2 = Array("O","O","O")
    stopManager.hasNonStopContent(l1, t1, Some(e1)) shouldBe(true)
    stopManager.hasNonStopContent(l2, t2, Some(e2)) shouldBe(false)
  }
}
