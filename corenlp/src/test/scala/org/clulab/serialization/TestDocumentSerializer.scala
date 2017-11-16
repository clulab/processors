package org.clulab.serialization

import org.clulab.processors.Processor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.scalatest._

/**
  * Author: Mihai Surdeanu and Tom Hicks.
  * Last Modified: Update for fix to text field serialization: add tests of larger, multi-line text.
  */
class TestDocumentSerializer extends FlatSpec with Matchers {
  val proc: Processor = new CoreNLPProcessor(withDiscourse = ShallowNLPProcessor.WITH_DISCOURSE)
  val ser = new DocumentSerializer

  val text1 = "Z"
  val text = "John Doe went to China. There, he visited Beijing."
  val bigText = """
Phosphorylation of ASPP2 by RAS/MAPK Pathway Is Critical for Its Full Pro-Apoptotic Function.We reported recently that apoptosis-stimulating protein of p53 (ASPP) 2, an activator of p53, co-operates with oncogenic RAS to enhance the transcription and apoptotic function of p53. However, the detailed mechanism remains unknown. Here we show that ASPP2 is a novel substrate of mitogen-activated protein kinase (MAPK). Phosphorylation of ASPP2 by MAPK is required for RAS-induced increased binding to p53 and increased transactivation of pro-apoptotic genes. In contrast, an ASPP2 phosphorylation mutant exhibits reduced p53 binding and fails to enhance transactivation and apoptosis. Thus phosphorylation of ASPP2 by RAS/MAPK pathway provides a novel link between RAS and p53 in regulating apoptosis.Introduction.p53 is the most commonly mutated tumour suppressor protein thus far identified. Although p53 is mutated in more than 50% of tumours, its mutation rate varies significantly between different types of human cancer, with a particularly high incidence in colorectal and pancreatic tumours. Interestingly, the mediator of signal transduction RAS is also commonly mutated in these particular tumour types. It remains unclear why there exists such a tight association between the p53 and RAS mutation status [ XREF_BIBR ]. We reported recently that apoptosis-stimulating protein of p53 (ASPP) 2 co-operates with oncogenic RAS to enhance the transcription and apoptotic function of p53 in cancer cells [ XREF_BIBR ].
This may be achieved via the ability of active RAS to induce ASPP2, thereby promoting ASPP2's interaction with p53 and enhancing the activity of p53. However, the detailed mechanism underlying this observation remains to be elucidated.  Activated RAS promotes the protein kinase activity of RAF, which phosphorylates and activates MEK (also known as MAPKK). MEK phosphorylates and activates a mitogen-activated protein kinase (MAPK/ERK), a serine/threonine-selective protein kinase. The MAPK enzymes require a specific phosphorylation sequence where a serine or threonine is followed by proline (S/TP) [ XREF_BIBR ]. It was shown that endogenous RAS is necessary for the full apoptotic activity of ASPP2, which suggests that RAS signalling may modify ASPP2, potentially via a phosphorylation event. Phosphorylation by RAS/MAPK modulates the activation of most of their substrates and in some cases the phosphorylation mediates changes in subcellular localisation [ XREF_BIBR ].ASPP2 belongs to an evolutionarily conserved ASPP family of proteins, alongside ASPP1 and iASPP. All three contain signature sequences in their C-termini; ankyrin repeats, SH3 domain and proline rich sequences [ XREF_BIBR ]. ASPP2 binds to RAS through its N-terminus [ XREF_BIBR , XREF_BIBR ]. The functions of ASPP2 are potentially controlled by its binding partners and localisation. When ASPP2 locates at the cell-cell junctions, it binds and co-localises with PAR3 via its N-terminus to maintain the integrity of cell polarity and adherence junction [ XREF_BIBR , XREF_BIBR ], whereas in the cytosol/nucleus, ASPP2 enhances p53-induced apoptosis in cancer cells [ XREF_BIBR ]. It also binds ATG5 and inhibits RAS-induced autophagy, independently of p53 [ XREF_BIBR ]. Thus it is important to find the molecular event that controls the localisation of ASPP2.  Here we show that ASPP2 is a novel substrate of RAS/MAPK.
Phosphorylation of ASPP2 by MAPK is required for the RAS-induced translocation of ASPP2, which results in the increased binding to p53. Consequently, the pro-apoptotic activity of ASPP2 is increased by the RAS/Raf/MAPK signalling cascade as ASPP2 phosphorylation mutant fails to do so.
"""

  "DocumentSerializer" should "save/load very small document correctly, keeping text" in {
    val doc1 = proc.annotate("Z", true)     // explicit keep text
    // println("Created a document with " + doc1.sentences.size + " sentences.")
    val out1 = ser.save(doc1, keepText=true)
    // println(out1)                          // DEBUGGING

    val doc2 = ser.load(out1)
    (doc2.sentences.size) should be (doc1.sentences.size)
    (doc2.text) should not be (empty)
    (doc2.text.get.size) should be (1)
    (doc2.text.get) should be ("Z")

    val out2 = ser.save(doc2, keepText=true)
    // println(out2)                          // DEBUGGING
    (out2) should be (out1)
  }

  "DocumentSerializer" should "save/load small document correctly, with defaults" in {
    val doc1 = proc.annotate(text)
    // println("Created a document with " + doc1.sentences.size + " sentences.")
    val out1 = ser.save(doc1)
    // println(out1)                           // DEBUGGING

    val doc2 = ser.load(out1)
    (doc2.sentences.size) should be (doc1.sentences.size)
    (doc2.text) should be (empty)

    val out2 = ser.save(doc2)
    (out2) should be (out1)
  }

  "DocumentSerializer" should "save/load small document correctly, keeping text" in {
    val doc1 = proc.annotate(text, true)    // explicit keep text
    // println("Created a document with " + doc1.sentences.size + " sentences.")
    val out1 = ser.save(doc1, keepText=true)
    // println(out1)                          // DEBUGGING

    val doc2 = ser.load(out1)
    (doc2.sentences.size) should be (doc1.sentences.size)
    (doc2.text) should not be (empty)
    (doc2.text.get.size) should be (text.size)
    (doc2.text.get) should be (text)

    val out2 = ser.save(doc2, keepText=true)
    // println(out2)                          // DEBUGGING
    (out2) should be (out1)
  }

  "DocumentSerializer" should "save/load large document correctly, with defaults" in {
    val doc1 = proc.annotate(bigText)
    // println("Created a document with " + doc1.sentences.size + " sentences.")
    val out1 = ser.save(doc1)
    // println(out1)                          // DEBUGGING

    val doc2 = ser.load(out1)
    (doc2.sentences.size) should be (doc1.sentences.size)
    (doc2.text) should be (empty)

    val out2 = ser.save(doc2)
    // println(out2)                          // DEBUGGING
    (out2) should be (out1)
  }

  "DocumentSerializer" should "save/load larger document correctly, keeping text" in {
    val doc1 = proc.annotate(bigText, true)    // explicit keep text
    // println("Created a document with " + doc1.sentences.size + " sentences.")
    val out1 = ser.save(doc1, keepText=true)
    // println(out1)                         // DEBUGGING

    val doc2 = ser.load(out1)
    (doc2.sentences.size) should be (doc1.sentences.size)
    (doc2.text) should not be (empty)
    (doc2.text.get.size) should be (bigText.size)
    (doc2.text.get) should be (bigText)

    val out2 = ser.save(doc2, keepText=true)
    // println(out2)                          // DEBUGGING
    (out2) should be (out1)
  }

}
