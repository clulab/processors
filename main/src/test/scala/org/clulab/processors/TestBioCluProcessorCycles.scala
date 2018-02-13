package org.clulab.processors

import org.clulab.processors.clu.BioCluProcessor
import org.scalatest.{FlatSpec, Matchers}

/**
  * Makes sure that BioClu does not produce syntax graphs with cycles
  * User: mihais
  * Date: 2/12/18
  */
class TestBioCluProcessorCycles extends FlatSpec with Matchers {
  var proc:Processor = new BioCluProcessor()

  def annotate(doc:Document) {
    proc.lemmatize(doc)
    proc.tagPartsOfSpeech(doc)
    proc.parse(doc)
    doc.clear()
  }

  "BioCluProcessor" should "not produce dependency graphs with cycles (a)" in {
    val doc = proc.mkDocument("Ala, Arg, Asn, Asp, Cys, Gln, Glu, Gly, His, Ile, Leu, Lys, Met, Phe, Pro, Ser, Thr, Trp, Tyr, and Val are now labeled as Sites.")
    annotate(doc)

    val deps = doc.sentences.head.universalEnhancedDependencies
    deps.isDefined should be (true)
    deps.get.containsCycles() should be (false)
  }

  it should "not produce dependency graphs with cycles (b)" in {
    val doc = proc.mkDocument("The docking protein Gab1 is the primary mediator of EGF-stimulated activation of the PI-3K/Akt cell survival pathway")
    annotate(doc)

    val deps = doc.sentences.head.universalEnhancedDependencies
    deps.isDefined should be (true)
    println(deps.get)
    deps.get.containsCycles() should be (false)

  }

}
