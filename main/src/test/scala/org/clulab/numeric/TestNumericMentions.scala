package org.clulab.numeric

import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Test

class TestNumericMentions extends Test {
  val processor = new CluProcessor()

  behavior of "MeasurementMention"

  it should "not throw an exception" in {
    val text = "(c) a retention licence, fourty five days; and  ."
    val document = processor.annotate(text)

    document
  }
}
