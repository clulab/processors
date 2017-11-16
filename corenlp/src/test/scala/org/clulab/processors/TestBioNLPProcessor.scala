package org.clulab.processors

import org.clulab.processors.bionlp.BioNLPProcessor
import org.scalatest._

/**
  * Tests BioNLPProcessor
  * User: mihais
  * Date: 10/29/14
  * Last Modified: Update for BE KBs.
  */
class TestBioNLPProcessor extends FlatSpec with Matchers {
  var proc:Processor = new BioNLPProcessor()

  "BioNLPProcessor" should "recognize some tricky entity names" in {
    var doc = proc.mkDocument("We tested the level of neurofibromin present in the sample")
    annotate(doc)

    var es = doc.sentences(0).entities.get
    println(s"Tricky entities: ${es.mkString(", ")}")
    es(5) should be ("B-Gene_or_gene_product")

    doc = proc.mkDocument("XRCC1 stimulates DNA-PK enzymatic activity")
    annotate(doc)

    es = doc.sentences(0).entities.get
    println(s"Tricky entities: ${es.mkString(", ")}")
    es(0) should be ("B-Gene_or_gene_product")
    es(2) should be ("B-Gene_or_gene_product")
  }

  "BioNLPProcessor" should "recognize correct NEs in text 1" in {
    val doc = proc.mkDocumentFromSentences(List(
      "Co-immunoprecipitation analysis confirmed that Bis interacted with Bcl-2 in vivo.",
      "The Ras protein is phosphorylated by TBRI."), keepText = false)

    annotate(doc)

    var i = 0
    for(s <- doc.sentences) {
      println(s"Labels for sentence #$i: " + s.entities.get.mkString(" "))
      i += 1
    }

    doc.sentences(0).entities.get(4) should be ("B-Gene_or_gene_product")
    doc.sentences(0).entities.get(7) should be ("B-Gene_or_gene_product")
    doc.sentences(0).entities.get(8) should be ("O")

    doc.sentences(1).entities.get(1) should be ("B-Family")
    doc.sentences(1).entities.get(2) should be ("O")
    doc.sentences(1).entities.get(6) should be ("B-Gene_or_gene_product")

  }

  it should "recognize correct NEs in text 2" in {
    val doc = proc.mkDocument(
      "Identification of complex formation between two intracellular tyrosine kinase substrates: human c-Rel and the p105 precursor of p50 NF-kappa B. " +
        "Immune complexes of the product of the c-rel protooncogene and of p105, the p50 NF-kappa B precursor, isolated from human T-lymphoblastoid cell lines are comprised of multiple proteins. " +
        "Only p105 and human c-Rel (hc-Rel) are common to complexes precipitated with antiserum directed against either p105 or hc-Rel. " +
        "Both proteins are inducible by phytohemagglutinin (PHA) and phorbol 12-myristate 13-acetate (PMA) and their subcellular distribution is affected by this induction. " +
        "We demonstrate that the Rel immune complex contains a protein with a molecular weight in the 40 kDa range (p40) which apparently is exclusively cytoplasmic. " +
        "We were not able to detect p40 in the p105 immune complex, though hc-Rel is present. " +
        "This indicates that hc-Rel exists in different multi-protein complexes and fits a model of functional regulation mediated by differential protein-protein interaction. " +
        "We also demonstrate considerable isoform diversity of both hc-Rel and p105. " +
        "We show that this heterogeneity is, in part, the result of phosphorylation. " +
        "Furthermore, we demonstrate that p105 and hc-Rel are tyrosine kinase substrates. " +
        "This finding indicates a role for both proteins in intracellular signal transduction pathways which are modulated by modification of their phosphorylation status.", keepText = false)

    annotate(doc)

    /*
    val s = doc.sentences(0)
    for(i <- 0 until s.size) {
      println(s"${s.words(i)} ${s.tags.get(i)} ${s.lemmas.get(i)} ${s.entities.get(i)}")
    }
    */

    var i = 0
    for(s <- doc.sentences) {
      println(s"Labels for sentence #$i: " + s.entities.get.mkString(" "))
      i += 1
    }

    doc.sentences(0).entities.get(7) should be ("B-Site")
    doc.sentences(0).entities.get(11) should be ("B-Species")
    doc.sentences(0).entities.get(12) should be ("B-Gene_or_gene_product")
    doc.sentences(1).entities.get(1) should be ("B-Gene_or_gene_product")
  }

  it should "recognize correct species in text 3" in {
    val doc = proc.mkDocument(
      "Human RAS is different from Anthrax bacterium RAS.", keepText = false)

    annotate(doc)

    val s = doc.sentences(0)
    for(i <- 0 until s.size) {
      println(s"${s.words(i)} ${s.tags.get(i)} ${s.lemmas.get(i)} ${s.entities.get(i)}")
    }

    var i = 0
    for(s <- doc.sentences) {
      println(s"Labels for sentence #$i: " + s.entities.get.mkString(" "))
      i += 1
    }

    doc.sentences(0).entities.get(0) should be ("B-Species")
    doc.sentences(0).entities.get(5) should be ("B-Species")
    doc.sentences(0).entities.get(6) should be ("I-Species")
  }

  it should "recognize protein families" in {
    val doc = proc.mkDocument("Mek is a protein!")

    annotate(doc)

    var i = 0
    for(s <- doc.sentences) {
      println(s"Labels for sentence #$i: " + s.entities.get.mkString(" "))
      i += 1
    }

    // TODO: uncomment this after the rules are fixed
    // doc.sentences(0).entities.get(0) should be ("B-Family")
  }

  it should "fixup POS tags correctly for bio-verbs" in {
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
    proc.tagPartsOfSpeech(doc)
    doc.clear()

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
    proc.tagPartsOfSpeech(doc)
    doc.clear()

    doc.sentences.foreach { sent =>
      sent.tags.get(2) should be ("VB")
      sent.tags.get(6) should be ("VBZ")
    }
  }

  it should "Correct the tags of His and Pro to NN" in {
    val doc = proc.mkDocument("AAA promotes His phosphorylation of BBB. AAA promotes Pro phosphorylation of BBB.",
      keepText = false)
    proc.tagPartsOfSpeech(doc)
    doc.clear()

    doc.sentences.foreach { sent =>
      sent.tags.get(2) should be ("NN")
    }
  }

  it should "not annotate NEs from the stop list" in {
    val doc = proc.mkDocument("A cell should not have the role of blot.", keepText = false)
    annotate(doc)

    /*
    val s = doc.sentences(0)
    for(i <- 0 until s.size) {
      println(s"${s.words(i)} ${s.tags.get(i)} ${s.lemmas.get(i)} ${s.entities.get(i)}")
    }
    */

    var i = 0
    for(s <- doc.sentences) {
      println(s"Labels for sentence #$i: " + s.entities.get.mkString(" "))
      i += 1
    }

    doc.sentences(0).entities.get(1) should be ("O")
    doc.sentences(0).entities.get(6) should be ("O")
    doc.sentences(0).entities.get(8) should be ("O")
  }

  it should "not annotate NEs from the stop list when upper initial" in {
    val doc = proc.mkDocument("Role of blot.", keepText = false)
    annotate(doc)

    /*
    val s = doc.sentences(0)
    for(i <- 0 until s.size) {
      println(s"${s.words(i)} ${s.tags.get(i)} ${s.lemmas.get(i)} ${s.entities.get(i)}")
    }
    */

    var i = 0
    for(s <- doc.sentences) {
      println(s"Labels for sentence #$i: " + s.entities.get.mkString(" "))
      i += 1
    }

    doc.sentences(0).entities.get(0) should be ("O")
    doc.sentences(0).entities.get(1) should be ("O")
    doc.sentences(0).entities.get(2) should be ("O")
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

  it should "override named entities" in {
    val doc = proc.mkDocument("ROS p85 p110", keepText = false)
    annotate(doc)

    doc.sentences(0).entities.get(0) should be ("B-Simple_chemical")
    doc.sentences(0).entities.get(1) should be ("B-Family")
    doc.sentences(0).entities.get(2) should be ("B-Family")
  }

  it should "recognize 'Smad 2' as a protein" in {
    val doc = proc.mkDocument("Smad 2 is doing something.", keepText = false)
    annotate(doc)

    doc.sentences(0).entities.get(0) should be ("B-Gene_or_gene_product")
    doc.sentences(0).entities.get(1) should be ("I-Gene_or_gene_product")
    doc.sentences(0).entities.get(2) should be ("O")
  }

  it should "recognize 'Smad' as a family" in {
    val doc = proc.mkDocument("Smad is doing something.", keepText = false)
    annotate(doc)

    doc.sentences(0).entities.get(0) should be ("B-Family")
    doc.sentences(0).entities.get(1) should be ("O")
  }

  it should "not label XREFs as entities" in {
    val doc = proc.mkDocument("XREF_BIBR and XREF_FIG are not proteins.", keepText = false)
    annotate(doc)

    doc.sentences(0).entities.get(0) should be ("O")
    doc.sentences(0).entities.get(2) should be ("O")
  }

  it should "label E2F as a family" in {
    val doc = proc.mkDocument("E2F is doing something.", keepText = false)
    annotate(doc)

    doc.sentences(0).entities.get(0) should be ("B-Family")
  }

  it should "not produce empty tokens" in {
    val doc = proc.mkDocument("These interactions may be quite complex - partially antagonistic, partially superadditive - and they surely will not be limited to interactions between two genes respectively, but there will be interactions between multiple genes.")
    annotate(doc)

    for (w <- doc.sentences(0).words) {
      w.length > 0 should be (true)
    }
  }

  it should "recognize \"insulin receptor substrate-1\" as an entity" in {
    val doc = proc.mkDocument("We now show that mTOR inhibition induces insulin receptor substrate-1 expression and abrogates feedback inhibition of the pathway.")
    annotate(doc)

    val es = doc.sentences(0).entities.get
    es(7) should be ("B-Gene_or_gene_product")
    es(8) should be ("I-Gene_or_gene_product")
    es(9) should be ("I-Gene_or_gene_product")

  }

  it should "NOT recognize \"Mdm2 binding\" as a protein family" in {
    val doc = proc.mkDocument("FOXO3a phosphorylation by ERK through an unknown mechanism induces Mdm2 binding to FOXO3a .")
    annotate(doc)

    val es = doc.sentences(0).entities.get
    println("MDM2 ENTS: " + es.mkString(", "))

    es(10) should be ("O")
  }

  def annotate(doc:Document) {
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    doc.clear()
  }

}
