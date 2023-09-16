package org.clulab.processors

class TestVs extends CluTest {
  val sentences = Seq(
    // (?i)^vs\.?$
    "Someone vs someone else",
    "Someone vs. someone else",
    "Someone Vs someone else",
    "Someone Vs. someone else",
    "Someone VS someone else",
    "Someone VS. someone else",
    "Someone vS someone else",
    "Someone vS. someone else"
  )

  behavior of "BalaurProcessor"

  def test(sentence: String): Unit = {
    it should sentence in {
      val expectedTag = "CC"

      val doc = this.proc.annotate(sentence)
      val sent = doc.sentences.head
      val tags = sent.tags.get
      val actualTag = tags(1)

      actualTag should be (expectedTag)
    }
  }

  sentences.foreach(test)
}
