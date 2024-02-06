package org.clulab.sequences

import org.clulab.processors.clu.BalaurProcessor
import org.clulab.utils.Test

class TestNamedEntityRecognition extends Test {
  val processor = new BalaurProcessor()

  behavior of "named entity recognition"

  it should "use proper BIO notation" in {
    val text = "Table 1 : Results of LBA credit committees ( North Zone ) for the 2022 hot dry season source : Lba / Zone Nord , February 2022 . "
    val doc = processor.annotate(text)
    val namedEntities = doc.sentences.head.entities.get

    // NamedEntity.patch(namedEntities)
    NamedEntity.isValid(namedEntities) should be (true)
  }
}
