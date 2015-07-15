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

    /*
    var i = 0
    for(s <- doc.sentences) {
      println(s"Labels for sentence #$i: " + s.entities.get.mkString(" "))
      i += 1
    }
    */

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
    var i = 0
    for(s <- doc.sentences) {
      println(s"Labels for sentence #$i: " + s.entities.get.mkString(" "))
      i += 1
    }
    */

    doc.sentences(0).entities.get(7) should be ("B-Simple_chemical")
    doc.sentences(0).entities.get(12) should be ("B-Gene_or_gene_product")
    doc.sentences(1).entities.get(1) should be ("B-Gene_or_gene_product")
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

  def annotate(doc:Document) {
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    proc.recognizeNamedEntities(doc)
    doc.clear()
  }

}
