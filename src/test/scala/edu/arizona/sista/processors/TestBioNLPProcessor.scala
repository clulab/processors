package edu.arizona.sista.processors

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import org.scalatest._

/**
  *
  * User: mihais
  * Date: 10/29/14
  */
class TestBioNLPProcessor extends FlatSpec with Matchers {
  var proc:Processor = new BioNLPProcessor()

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
    doc.sentences(0).entities.get(7) should be ("B-Family")
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

    doc.sentences(0).entities.get(7) should be ("B-Simple_chemical")
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
        "AAA can acetylate     BBB and CCC acetylates DDD" +
        "AAA can farnesylate   BBB and CCC farnesylates DDD" +
        "AAA can glycosylate   BBB and CCC glycosylates DDD" +
        "AAA can hydrolyze     BBB and CCC hydrolyzes DDD" +
        "AAA can hydroxylate   BBB and CCC hydroxylates DDD" +
        "AAA can methylate     BBB and CCC methylates DDD" +
        "AAA can phosphorylate BBB and CCC phosphorylates DDD" +
        "AAA can ribosylate    BBB and CCC ribosylates DDD" +
        "AAA can sumoylate     BBB and CCC sumoylates DDD" +
        "AAA can translocate   BBB and CCC translocates DDD" +
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
        "AAA can deacetylate     BBB and CCC deacetylates DDD" +
        "AAA can defarnesylate   BBB and CCC defarnesylates DDD" +
        "AAA can deglycosylate   BBB and CCC deglycosylates DDD" +
        "AAA can dehydrolyze     BBB and CCC dehydrolyzes DDD" +
        "AAA can dehydroxylate   BBB and CCC dehydroxylates DDD" +
        "AAA can demethylate     BBB and CCC demethylates DDD" +
        "AAA can dephosphorylate BBB and CCC dephosphorylates DDD" +
        "AAA can deribosylate    BBB and CCC deribosylates DDD" +
        "AAA can desumoylate     BBB and CCC desumoylates DDD" +
        "AAA can detranslocate   BBB and CCC detranslocates DDD" +
        "AAA can deubiquitinate  BBB and CCC deubiquitinates DDD" +
        "AAA can de-acetylate     BBB and CCC de-acetylates DDD" +
        "AAA can de-farnesylate   BBB and CCC de-farnesylates DDD" +
        "AAA can de-glycosylate   BBB and CCC de-glycosylates DDD" +
        "AAA can de-hydrolyze     BBB and CCC de-hydrolyzes DDD" +
        "AAA can de-hydroxylate   BBB and CCC de-hydroxylates DDD" +
        "AAA can de-methylate     BBB and CCC de-methylates DDD" +
        "AAA can de-phosphorylate BBB and CCC de-phosphorylates DDD" +
        "AAA can de-ribosylate    BBB and CCC de-ribosylates DDD" +
        "AAA can de-sumoylate     BBB and CCC de-sumoylates DDD" +
        "AAA can de-translocate   BBB and CCC de-translocates DDD" +
        "AAA can de-ubiquitinate  BBB and CCC de-ubiquitinates DDD",
      keepText = false)
    proc.tagPartsOfSpeech(doc)
    doc.clear()

    doc.sentences.foreach { sent =>
      sent.tags.get(2) should be ("VB")
      sent.tags.get(6) should be ("VBZ")
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

  def annotate(doc:Document) {
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    doc.clear()
  }

}
