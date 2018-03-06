package org.clulab.processors

import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.scalatest._

import org.clulab.processors.corenlp.CoreNLPProcessor

class TestCoreNLPCoref extends FlatSpec with Matchers {

  val text = "When a RICO TRO is being sought, the prosecutor is required, at the earliest appropriate time, to state publicly that the government's request for a TRO, and eventual forfeiture, is made in full recognition of the rights of third parties -- that is, in requesting the TRO, the government will not seek to disrupt the normal, legitimate business activities of the defendant; will not seek through use of the relation-back doctrine to take from third parties assets legitimately transferred to them; will not seek to vitiate legitimate business transactions occurring between the defendant and third parties; and will, in all other respects, assist the court in ensuring that the rights of third parties are protected, through proceeding under {RICO} and otherwise. "

  val proc = new CoreNLPProcessor(internStrings = true, withDiscourse = ShallowNLPProcessor.WITH_DISCOURSE)

  "CoreNLPProcessor" should "not crash" in {

    val doc = proc.mkDocument(text, keepText = false)
    val annotation = proc.annotate(doc)
    annotation should not be(null)

  }


}
