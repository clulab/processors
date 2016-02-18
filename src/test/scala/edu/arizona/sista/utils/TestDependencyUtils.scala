package edu.arizona.sista.utils

import edu.arizona.sista.processors.bionlp.BioNLPProcessor
import edu.arizona.sista.utils.DependencyUtils._
import edu.arizona.sista.struct.Interval
import org.scalatest._

/**
  * Tests dependency utilities such as findHeadStrict
  * Date: 1/25/16
  */
class TestDependencyUtils extends FlatSpec with Matchers {
  val proc = new BioNLPProcessor
  val text1 = "Because the substrates of Shp2 are for the most part unknown, we were additionally interested in " +
    "examining the state of EGFR tyrosine phosphorylation following treatment with EGF in order to determine if the " +
    "failure of Gab1 to bind p85, and potentially recruit Shp2, would influence levels of EGFR autophosphorylation."
  val doc1 = proc.annotate(text1, keepText = true)
  val sent1 = doc1.sentences.head
  text1 should "produce 'substrates' as the head of 'the substrates of Shp2'" in {
    findHeadStrict(Interval(1, 5), sent1).get should be (2)
  }
  it should "produce 'are' as the head of 'the substrates of Shp2 are'" in {
    findHeadStrict(Interval(1, 6), sent1).get should be (5)
  }
  it should "produce None for an empty Interval" in {
    findHeadStrict(Interval(0, 0), sent1) should be (None)
  }

  val text2 = "The docking protein Gab1 is the primary mediator of EGF-stimulated activation of the PI-3K/Akt cell survival pathway"
  val doc2 = proc.annotate(text2, keepText = true)
  val sent2 = doc2.sentences.head
  text2 should "have the same getHeadStrict as roots" in {
    val head = findHeadStrict(Interval(0, 20), sent2).get
    val root = sent2.dependencies.get.roots.head
    head == root should be (true)
  }
  it should "produce only one head for getHeadsStrict" in {
    findHeadsStrict(Interval(0, 20), sent2).length should be (1)
  }
  it should "produce None for an empty Interval" in {
    findHeadStrict(Interval(0, 0), sent2) should be (None)
  }
  it should "produce two heads for 'docking protein' using getHeadsLocal" in {
    findHeadsLocal(Interval(1, 3), sent2.dependencies.get).length should be (2)
  }
  it should "think 'docking protein' is nested in 'docking protein Gab1'" in {
    nested(Interval(1, 3), Interval(1, 4), sent2, sent2) should be (true)
  }
  it should "think 'the' is nested in 'Gab1'" in {
    nested(Interval(0, 1), Interval(3, 4), sent2, sent2) should be (true)
  }
  it should "not think 'is' is nested in 'Gab1'" in {
    nested(Interval(4, 5), Interval(3, 4), sent2, sent2) should be (false)
  }
  it should "think Interval(3,3) is nested in 'Gab1'" in {
    nested(Interval(3, 3), Interval(3, 4), sent2, sent2) should be (true)
  }
  it should "not think 'pathway' is nested in 'Gab1'" in {
    nested(Interval(3, 4), Interval(19, 20), sent2, sent2) should be (false)
  }

  val text3 = "."
  val doc3 = proc.annotate(text3, keepText = true)
  val sent3 = doc3.sentences.head
  text3 should "produce one head using findHeads" in {
    findHeads(Interval(0, 1), sent3.dependencies.get) should have size (1)
  }
  text3 should "produce no heads using findHeadsStrict" in {
    findHeadsStrict(Interval(0, 1), sent3) should have size (0)
  }
}
