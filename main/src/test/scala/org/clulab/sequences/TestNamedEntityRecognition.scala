package org.clulab.sequences

import org.clulab.dynet.Utils
import org.clulab.processors.clu.CluProcessor
import org.clulab.utils.Test

class TestNamedEntityRecognition extends Test {
  Utils.initializeDyNet()

  val processor = new CluProcessor()

  behavior of "NamedEntity"

  def isValid(entities: Array[String]): Boolean = {
    entities.indices.forall { index =>
      val valid = if (index == 0)
        !entities(index).startsWith("I-")
      else
        if (entities(index).startsWith("I-")) {
          val tail = entities(index).drop(2)

          entities(index - 1) == "B-" + tail || entities(index - 1) == "I-" + tail
        }
        else true

      valid
    }
  }

  it should "use proper BIO notation" in {
    val text = "Table 1 : Results of LBA credit committees ( North Zone ) for the 2022 hot dry season source : Lba / Zone Nord , February 2022 . "
    val doc = processor.annotate(text)
    val namedEntities = doc.sentences.head.entities.get

    isValid(namedEntities) should be (true)
  }
}
