package org.clulab.utils

import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.utils.DependencyUtils._
import org.clulab.struct.{DirectedGraph, Interval}
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
    val result = findHeadStrict(Interval(1, 5), sent1)
    result shouldBe defined
    result.get should be (2)
  }
  it should "produce 'are' as the head of 'the substrates of Shp2 are'" in {
    val result = findHeadStrict(Interval(1, 6), sent1)
    result shouldBe defined
    result.get should be (5)
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

  "DependencyUtils" should "handle cycles in the dependencyGraph correctly" in {
    val edges = List((1, 0, "det"),(1,3,"rcmod"),(3,1,"nsubj"),(3,6,"prep_at"),(6,5,"nn"),
      (8,1,"nsubj"),(8,7,"advmod"),(8,12,"dobj"),(8,20,"prep_in"),(12,9,"det"),(12,10,"nn"),
      (12,11,"nn"),(12,13,"partmod"),(13,16,"prep_for"),(16,15,"nn"),(20,19,"amod"))
    val depGraph = new DirectedGraph[String](DirectedGraph.triplesToEdges[String](edges), Set(8))
    val tokenInterval = Interval(0, 2)
    noException shouldBe thrownBy (DependencyUtils.findHeads(tokenInterval, depGraph))
  }

  it should "handle roots with incoming dependencies" in {
    val edges = List(
      (7, 1, "advmod"), (7, 2, "nsubj"), (7, 3, "cop"), (7, 4, "det"), (7, 5, "amod"),
      (7, 6, "nn"), (7, 9, "rcmod"), (9, 7, "nsubj"), (9, 10, "dobj"), (10, 13, "prep_for"),
      (10, 15, "prep_for"), (13, 12, "det"), (13, 15, "conj_and"), (13, 18, "prep_of"),
      (18, 17, "det")
    )
    val graph = DirectedGraph(DirectedGraph.triplesToEdges[String](edges), Set(7))
    val interval = Interval(4, 8)
    noException shouldBe thrownBy (DependencyUtils.findHeads(interval, graph))
  }

  // this test comes from sentence 23556 in file /data/nlp/corpora/agiga/data/xml/afp_eng_199405.xml.gz
  // the dependency parse has an erroneous loop in token 11 that makes it impossible to reach the root from the given interval
  it should "throw a DependencyUtilsException if the dependency parse is malformed" in {
    val edges = List(
      (3, 0, "det"), (3, 1, "nn"), (3, 2, "nn"), (5, 3, "nsubj"), (5, 4, "dep"), (5, 10, "ccomp"),
      (8, 7, "det"), (10, 6, "tmod"), (10, 8, "nsubj"), (10, 9, "dep"), (10, 14, "prep_with"), (10, 27, "neg"),
      (10, 30, "prep_in"), (10, 32, "prepc_about"),
      (11, 11, "conj_and"), // note this loop in the graph
      (11, 22, "prep_about"), (11, 26, "prep_with"), (14, 13, "det"), (14, 16, "prep_of"), (16, 19, "prep_in"),
      (19, 18, "det"), (22, 21, "amod"), (26, 25, "det"), (30, 29, "det"), (32, 34, "dobj"), (34, 33, "amod")
    )
    val graph = DirectedGraph(DirectedGraph.triplesToEdges[String](edges), Set(5))
    val interval = Interval(21, 23)
    a [DependencyUtilsException] shouldBe thrownBy (DependencyUtils.findHeads(interval, graph))
  }

}
