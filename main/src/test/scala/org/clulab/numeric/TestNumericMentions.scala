package org.clulab.numeric

import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Test

class TestNumericMentions extends Test {
  val processor = new CluProcessor()

  behavior of "MeasurementMention"

  it should "deal with a misspelled fourty five" in {
    val text = "(c) a retention licence, fourty five days; and  ."
    val sentence = processor.annotate(text).sentences.head

    sentence.norms.get(7) should be ("45.0 d")
    sentence.entities.get(7) should be ("B-MEASUREMENT-DURATION")
  }

  it should "deal with properly spelled forty five" in {
    val text = "(c) a retention licence, forty five days; and  ."
    val sentence = processor.annotate(text).sentences.head

    sentence.norms.get(7) should be ("45.0 d")
    sentence.entities.get(7) should be ("B-MEASUREMENT-DURATION")
  }

  it should "deal with a misspelled fourty-five" in {
    val text = "(c) a retention licence, fourty-five days; and  ."
    val sentence = processor.annotate(text).sentences.head

    sentence.norms.get(7) should be ("45.0 d")
    sentence.entities.get(7) should be ("B-MEASUREMENT-DURATION")
  }

  it should "deal with properly spelled forty-five" in {
    val text = "(c) a retention licence, forty-five days; and  ."
    val sentence = processor.annotate(text).sentences.head

    sentence.norms.get(7) should be ("45.0 d")
    sentence.entities.get(7) should be ("B-MEASUREMENT-DURATION")
  }

  it should "deal with a misspelled fourty five hundred" in {
    val text = "(c) a retention licence, fourty five hundred days; and  ."
    val sentence = processor.annotate(text).sentences.head

    sentence.norms.get(7) should be ("4500.0 d")
    sentence.entities.get(7) should be ("B-MEASUREMENT-DURATION")
  }

  it should "deal with properly spelled forty five hundred" in {
    val text = "(c) a retention licence, forty five hundred days; and  ."
    val sentence = processor.annotate(text).sentences.head

    sentence.norms.get(7) should be ("4500.0 d")
    sentence.entities.get(7) should be ("B-MEASUREMENT-DURATION")
  }

  it should "deal with a misspelled fourty-five hundred" in {
    val text = "(c) a retention licence, fourty-five hundred days; and  ."
    val sentence = processor.annotate(text).sentences.head

    sentence.norms.get(7) should be ("4500.0 d")
    sentence.entities.get(7) should be ("B-MEASUREMENT-DURATION")
  }

  it should "deal with properly spelled forty-five hundred" in {
    val text = "(c) a retention licence, forty-five hundred days; and  ."
    val sentence = processor.annotate(text).sentences.head

    sentence.norms.get(7) should be ("4500.0 d")
    sentence.entities.get(7) should be ("B-MEASUREMENT-DURATION")
  }

  behavior of "PercentageMention"

  it should "not throw an uncaught exception and crash" in {
    val text = "pdf 75 Some of the major sectoral plans include the national energy policy, transport policy, forest and wildlife policy, national environmental sanitation strategy etc. 76 https://www.thegasconsortium.com/documents/GMP-Final-Jun16.pdf 77 http://www.energycom.gov.gh/files/Renewable-Energy-Masterplan-February-2019.pdf 78 http://energycom.gov.gh/files/SE4ALL-GHANA%20ACTION%20PLAN.pdf 79 http://www.energycom.gov.gh/files/Ghana%20Integrated%20Power%20System%20Master%20Plan%20_Volume%202.pdf Page | 80 In 2017, the Ministry of Energy started a comprehensive review of the National Energy Policy, which is still ongoing."
    val document = processor.annotate(text)

    document
  }

  behavior of "processors"

  // We would like them to be the same, but they aren't.  The tests pass because of the nots,
  // and that will help with regression testing, but we would really like better behavior.
  // Because they aren't the exactly the same, grammar rules may apply differently so that
  // norms turn out differently.  So far, that hasn't affected the tests above.
  it should "treat forty one and fourty one the same" in {
    val text1 = "I turned forty one today."
    val text2 = "I turned fourty one today."
    val sentence1 = processor.annotate(text1).sentences.head
    val sentence2 = processor.annotate(text2).sentences.head

    sentence1.tags.get should contain theSameElementsInOrderAs(sentence2.tags.get)
    sentence1.entities.get should contain theSameElementsInOrderAs(sentence2.entities.get)
    sentence1.norms.get should contain theSameElementsInOrderAs(sentence2.norms.get)
    sentence1.chunks.get should contain theSameElementsInOrderAs(sentence2.chunks.get)
  }

  it should "treat forty-one and fourty-one the same" in {
    val text1 = "I turned forty-one today."
    val text2 = "I turned fourty-one today."
    val sentence1 = processor.annotate(text1).sentences.head
    val sentence2 = processor.annotate(text2).sentences.head

    sentence1.tags.get should not contain theSameElementsInOrderAs(sentence2.tags.get)
    sentence1.entities.get should contain theSameElementsInOrderAs(sentence2.entities.get)
    sentence1.norms.get should contain theSameElementsInOrderAs(sentence2.norms.get)
    sentence1.chunks.get should not contain theSameElementsInOrderAs(sentence2.chunks.get)
  }
}
