package org.clulab.processors

import org.clulab.processors.clu.BioCluProcessor
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * User: mihais
  * Date: 9/23/17
  */
class TestBioCluProcessor extends FlatSpec with Matchers {
  var proc:Processor = new BioCluProcessor()

  "BioCluProcessor" should "fixup POS tags correctly for bio-verbs" in {
    val doc = proc.mkDocument(
      "AAA can acetylate     BBB and CCC acetylates DDD. " +
        "AAA can farnesylate   BBB and CCC farnesylates DDD. " +
        "AAA can glycosylate   BBB and CCC glycosylates DDD. " +
        "AAA can hydrolyze     BBB and CCC hydrolyzes DDD. " +
        "AAA can hydroxylate   BBB and CCC hydroxylates DDD. " +
        "AAA can methylate     BBB and CCC methylates DDD. " +
        "AAA can phosphorylate BBB and CCC phosphorylates DDD. " +
        "AAA can ribosylate    BBB and CCC ribosylates DDD. " +
        "AAA can sumoylate     BBB and CCC sumoylates DDD. " +
        "AAA can translocate   BBB and CCC translocates DDD. " +
        "AAA can ubiquitinate  BBB and CCC ubiquitinates DDD", 
      keepText = false)
    annotate(doc)

    doc.sentences.foreach { sent =>
      sent.tags.get(2) should be ("VB")
      sent.tags.get(6) should be ("VBZ")
    }
  }

  it should "fixup POS tags correctly for reverse bio-verbs" in {
    val doc = proc.mkDocument(
      "AAA can deacetylate     BBB and CCC deacetylates DDD. " +
        "AAA can defarnesylate   BBB and CCC defarnesylates DDD. " +
        "AAA can deglycosylate   BBB and CCC deglycosylates DDD. " +
        "AAA can dehydrolyze     BBB and CCC dehydrolyzes DDD. " +
        "AAA can dehydroxylate   BBB and CCC dehydroxylates DDD. " +
        "AAA can demethylate     BBB and CCC demethylates DDD. " +
        "AAA can dephosphorylate BBB and CCC dephosphorylates DDD. " +
        "AAA can deribosylate    BBB and CCC deribosylates DDD. " +
        "AAA can desumoylate     BBB and CCC desumoylates DDD. " +
        "AAA can detranslocate   BBB and CCC detranslocates DDD. " +
        "AAA can deubiquitinate  BBB and CCC deubiquitinates DDD. " +
        "AAA can de-acetylate     BBB and CCC de-acetylates DDD. " +
        "AAA can de-farnesylate   BBB and CCC de-farnesylates DDD. " +
        "AAA can de-glycosylate   BBB and CCC de-glycosylates DDD. " +
        "AAA can de-hydrolyze     BBB and CCC de-hydrolyzes DDD. " +
        "AAA can de-hydroxylate   BBB and CCC de-hydroxylates DDD. " +
        "AAA can de-methylate     BBB and CCC de-methylates DDD. " +
        "AAA can de-phosphorylate BBB and CCC de-phosphorylates DDD. " +
        "AAA can de-ribosylate    BBB and CCC de-ribosylates DDD. " +
        "AAA can de-sumoylate     BBB and CCC de-sumoylates DDD. " +
        "AAA can de-translocate   BBB and CCC de-translocates DDD. " +
        "AAA can de-ubiquitinate  BBB and CCC de-ubiquitinates DDD",
      keepText = false)
    annotate(doc)

    doc.sentences.foreach { sent =>
      sent.tags.get(2) should be ("VB")
      sent.tags.get(6) should be ("VBZ")
    }
  }

  it should "Correct the tags of His and Pro to NN" in {
    val doc = proc.mkDocument("AAA promotes His phosphorylation of BBB. AAA promotes Pro phosphorylation of BBB.",
      keepText = false)
    annotate(doc)

    doc.sentences.foreach { sent =>
      sent.tags.get(2) should be ("NN")
    }
  }

  it should "tokenize complexes correctly" in {
    var doc = proc.mkDocument("The Mek-Ras complex", keepText = false)
    annotate(doc)

    doc.sentences(0).words.length should be (5)
    doc.sentences(0).words(1) should be ("Mek")
    doc.sentences(0).words(2) should be ("and")
    doc.sentences(0).words(3) should be ("Ras")

    doc = proc.mkDocument("The Mek/Ras1 complex", keepText = false)
    annotate(doc)

    doc.sentences(0).words.length should be (5)
    doc.sentences(0).words(1) should be ("Mek")
    doc.sentences(0).words(2) should be ("and")
    doc.sentences(0).words(3) should be ("Ras1")

    /*
    doc = proc.mkDocument("We analyze the Mek/Ras/Akt1 complex in light of recent work on the Mek, Ras, and Akt2 proteins.", keepText = false)
    annotate(doc)

    doc.sentences(0).words(3) should be ("Mek")
    doc.sentences(0).startOffsets(3) should be (15)
    doc.sentences(0).words(4) should be (",")
    doc.sentences(0).words(5) should be ("Ras")
    doc.sentences(0).words(6) should be ("and")
    doc.sentences(0).startOffsets(6) should be (22)
    doc.sentences(0).endOffsets(6) should be (23)
    doc.sentences(0).words(7) should be ("Akt1")
    */

    doc = proc.mkDocument("We analyze the Mek-Ras-Akt1 complex in light of recent work on the Mek, Ras, and Akt1 proteins.", keepText = false)
    annotate(doc)

    println(s"""Words: ${doc.sentences(0).words.mkString(" ")}""")
    doc.sentences(0).words(3) should be ("Mek")
    doc.sentences(0).startOffsets(3) should be (15)
    doc.sentences(0).words(4) should be (",")
    doc.sentences(0).words(5) should be ("Ras")
    doc.sentences(0).words(6) should be (",")
    doc.sentences(0).words(7) should be ("and")
    doc.sentences(0).startOffsets(7) should be (22)
    doc.sentences(0).endOffsets(7) should be (23)
    doc.sentences(0).words(8) should be ("Akt1")
  }

  it should "not produce empty tokens" in {
    val doc = proc.mkDocument("These interactions may be quite complex - partially antagonistic, partially superadditive - and they surely will not be limited to interactions between two genes respectively, but there will be interactions between multiple genes.")
    annotate(doc)

    for (w <- doc.sentences(0).words) {
      w.length > 0 should be (true)
    }
  }

  def annotate(doc:Document) {
    proc.lemmatize(doc)
    proc.tagPartsOfSpeech(doc)
    proc.recognizeNamedEntities(doc)
    doc.clear()
  }
}
