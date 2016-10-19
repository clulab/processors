package org.clulab.utils

import org.clulab.TestUtils.jsonStringToDocument
import org.clulab.struct.{DirectedGraph, Interval}
import org.clulab.utils.DependencyUtils._
import org.scalatest._

/**
  * Tests dependency utilities such as findHeadStrict
  * Date: 1/25/16
  */
class TestDependencyUtils extends FlatSpec with Matchers {
  val text1 = "Because the substrates of Shp2 are for the most part unknown, we were additionally interested in " +
    "examining the state of EGFR tyrosine phosphorylation following treatment with EGF in order to determine if the " +
    "failure of Gab1 to bind p85, and potentially recruit Shp2, would influence levels of EGFR autophosphorylation."
  val doc1 = jsonStringToDocument(""" {"sentences":[{"words":["Because","the","substrates","of","Shp2","are","for","the","most","part","unknown",",","we","were","additionally","interested","in","examining","the","state","of","EGFR","tyrosine","phosphorylation","following","treatment","with","EGF","in","order","to","determine","if","the","failure","of","Gab1","to","bind","p85",",","and","potentially","recruit","Shp2",",","would","influence","levels","of","EGFR","autophosphorylation","."],"startOffsets":[0,8,12,23,26,31,35,39,43,48,53,60,62,65,70,83,94,97,107,111,117,120,125,134,150,160,170,175,179,182,188,191,201,204,208,216,219,224,227,232,235,237,241,253,261,265,267,273,283,290,293,298,317],"endOffsets":[7,11,22,25,30,34,38,42,47,52,60,61,64,69,82,93,96,106,110,116,119,124,133,149,159,169,174,178,181,187,190,200,203,207,215,218,223,226,231,235,236,240,252,260,265,266,272,282,289,292,297,317,318],"tags":["IN","DT","NNS","IN","NN","VBP","IN","DT","JJS","NN","JJ",",","PRP","VBD","RB","JJ","IN","VBG","DT","NN","IN","NN","NN","NN","VBG","NN","IN","NN","IN","NN","TO","VB","IN","DT","NN","IN","NN","TO","VB","NN",",","CC","RB","VB","NN",",","MD","VB","NNS","IN","NN","NN","."],"lemmas":["because","the","substrate","of","shp2","be","for","the","most","part","unknown",",","we","be","additionally","interested","in","examine","the","state","of","egfr","tyrosine","phosphorylation","follow","treatment","with","egf","in","order","to","determine","if","the","failure","of","gab1","to","bind","p85",",","and","potentially","recruit","shp2",",","would","influence","level","of","egfr","autophosphorylation","."],"entities":["O","O","O","O","B-Gene_or_gene_product","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","B-Gene_or_gene_product","B-Site","O","O","O","O","B-Gene_or_gene_product","O","O","O","O","O","O","O","O","B-Gene_or_gene_product","O","O","B-Family","O","O","O","O","B-Gene_or_gene_product","O","O","O","O","O","B-Gene_or_gene_product","O","O"],"chunks":["B-SBAR","B-NP","I-NP","B-PP","B-NP","B-VP","B-PP","B-NP","I-NP","I-NP","B-ADJP","O","B-NP","B-VP","B-ADJP","I-ADJP","B-PP","B-VP","B-NP","I-NP","B-PP","B-NP","I-NP","I-NP","I-NP","I-NP","B-PP","B-NP","B-SBAR","O","B-VP","I-VP","B-SBAR","B-NP","I-NP","B-PP","B-NP","B-VP","I-VP","B-NP","O","O","B-VP","I-VP","B-NP","O","B-VP","I-VP","B-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":2,"destination":1,"relation":"det"},{"source":2,"destination":3,"relation":"prep"},{"source":3,"destination":4,"relation":"pobj"},{"source":5,"destination":0,"relation":"mark"},{"source":5,"destination":2,"relation":"nsubj"},{"source":5,"destination":6,"relation":"prep"},{"source":6,"destination":9,"relation":"pobj"},{"source":9,"destination":7,"relation":"det"},{"source":9,"destination":8,"relation":"amod"},{"source":9,"destination":10,"relation":"amod"},{"source":15,"destination":31,"relation":"advcl"},{"source":15,"destination":16,"relation":"prep"},{"source":15,"destination":5,"relation":"advcl"},{"source":15,"destination":12,"relation":"nsubj"},{"source":15,"destination":13,"relation":"cop"},{"source":15,"destination":14,"relation":"advmod"},{"source":16,"destination":17,"relation":"pcomp"},{"source":17,"destination":19,"relation":"dobj"},{"source":19,"destination":18,"relation":"det"},{"source":19,"destination":20,"relation":"prep"},{"source":20,"destination":23,"relation":"pobj"},{"source":23,"destination":21,"relation":"nn"},{"source":23,"destination":22,"relation":"nn"},{"source":23,"destination":24,"relation":"prep"},{"source":24,"destination":25,"relation":"pobj"},{"source":25,"destination":26,"relation":"prep"},{"source":26,"destination":27,"relation":"pobj"},{"source":31,"destination":47,"relation":"advcl"},{"source":31,"destination":28,"relation":"mark"},{"source":31,"destination":29,"relation":"dep"},{"source":31,"destination":30,"relation":"aux"},{"source":34,"destination":33,"relation":"det"},{"source":34,"destination":35,"relation":"prep"},{"source":35,"destination":36,"relation":"pobj"},{"source":36,"destination":38,"relation":"vmod"},{"source":38,"destination":37,"relation":"aux"},{"source":38,"destination":39,"relation":"dobj"},{"source":38,"destination":41,"relation":"cc"},{"source":38,"destination":43,"relation":"conj"},{"source":43,"destination":42,"relation":"advmod"},{"source":43,"destination":44,"relation":"dobj"},{"source":47,"destination":32,"relation":"mark"},{"source":47,"destination":48,"relation":"dobj"},{"source":47,"destination":34,"relation":"nsubj"},{"source":47,"destination":46,"relation":"aux"},{"source":48,"destination":49,"relation":"prep"},{"source":49,"destination":51,"relation":"pobj"},{"source":51,"destination":50,"relation":"nn"}],"roots":[15]},"stanford-collapsed":{"edges":[{"source":2,"destination":1,"relation":"det"},{"source":2,"destination":4,"relation":"prep_of"},{"source":5,"destination":0,"relation":"mark"},{"source":5,"destination":2,"relation":"nsubj"},{"source":5,"destination":9,"relation":"prep_for"},{"source":9,"destination":7,"relation":"det"},{"source":9,"destination":8,"relation":"amod"},{"source":9,"destination":10,"relation":"amod"},{"source":15,"destination":31,"relation":"advcl"},{"source":15,"destination":17,"relation":"prepc_in"},{"source":15,"destination":5,"relation":"advcl"},{"source":15,"destination":12,"relation":"nsubj"},{"source":15,"destination":13,"relation":"cop"},{"source":15,"destination":14,"relation":"advmod"},{"source":17,"destination":19,"relation":"dobj"},{"source":19,"destination":18,"relation":"det"},{"source":19,"destination":23,"relation":"prep_of"},{"source":23,"destination":21,"relation":"nn"},{"source":23,"destination":22,"relation":"nn"},{"source":23,"destination":25,"relation":"prep_following"},{"source":25,"destination":27,"relation":"prep_with"},{"source":31,"destination":47,"relation":"advcl"},{"source":31,"destination":28,"relation":"mark"},{"source":31,"destination":29,"relation":"dep"},{"source":31,"destination":30,"relation":"aux"},{"source":34,"destination":33,"relation":"det"},{"source":34,"destination":36,"relation":"prep_of"},{"source":36,"destination":38,"relation":"vmod"},{"source":36,"destination":43,"relation":"vmod"},{"source":38,"destination":37,"relation":"aux"},{"source":38,"destination":39,"relation":"dobj"},{"source":38,"destination":43,"relation":"conj_and"},{"source":43,"destination":42,"relation":"advmod"},{"source":43,"destination":44,"relation":"dobj"},{"source":47,"destination":32,"relation":"mark"},{"source":47,"destination":48,"relation":"dobj"},{"source":47,"destination":34,"relation":"nsubj"},{"source":47,"destination":46,"relation":"aux"},{"source":48,"destination":51,"relation":"prep_of"},{"source":51,"destination":50,"relation":"nn"}],"roots":[15]}}}]} """)
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
  val doc2 = jsonStringToDocument(""" {"sentences":[{"words":["The","docking","protein","Gab1","is","the","primary","mediator","of","EGF","stimulated","activation","of","the","PI-3K","and","Akt","cell","survival","pathway"],"startOffsets":[0,4,12,20,25,28,32,40,49,52,56,67,78,81,85,90,91,95,100,109],"endOffsets":[3,11,19,24,27,31,39,48,51,55,66,77,80,84,90,91,94,99,108,116],"tags":["DT","NN","NN","NN","VBZ","DT","JJ","NN","IN","NN","VBD","NN","IN","DT","NN","CC","NN","NN","NN","NN"],"lemmas":["the","docking","protein","gab1","be","the","primary","mediator","of","egf","stimulate","activation","of","the","pi-3k","and","akt","cell","survival","pathway"],"entities":["O","B-Family","O","B-Gene_or_gene_product","O","O","O","O","O","B-Gene_or_gene_product","O","O","O","O","B-Gene_or_gene_product","O","B-Gene_or_gene_product","B-BioProcess","I-BioProcess","O"],"chunks":["B-NP","I-NP","I-NP","I-NP","B-VP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","I-NP","I-NP","I-NP","I-NP"],"graphs":{"stanford-basic":{"edges":[{"source":3,"destination":0,"relation":"det"},{"source":3,"destination":1,"relation":"nn"},{"source":3,"destination":2,"relation":"nn"},{"source":7,"destination":3,"relation":"nsubj"},{"source":7,"destination":4,"relation":"cop"},{"source":7,"destination":5,"relation":"det"},{"source":7,"destination":6,"relation":"amod"},{"source":7,"destination":8,"relation":"prep"},{"source":8,"destination":9,"relation":"pobj"},{"source":10,"destination":19,"relation":"nsubj"},{"source":10,"destination":7,"relation":"dep"},{"source":10,"destination":11,"relation":"dobj"},{"source":11,"destination":12,"relation":"prep"},{"source":12,"destination":14,"relation":"pobj"},{"source":14,"destination":15,"relation":"cc"},{"source":14,"destination":18,"relation":"conj"},{"source":14,"destination":13,"relation":"det"},{"source":18,"destination":16,"relation":"nn"},{"source":18,"destination":17,"relation":"nn"}],"roots":[10]},"stanford-collapsed":{"edges":[{"source":3,"destination":0,"relation":"det"},{"source":3,"destination":1,"relation":"nn"},{"source":3,"destination":2,"relation":"nn"},{"source":7,"destination":3,"relation":"nsubj"},{"source":7,"destination":4,"relation":"cop"},{"source":7,"destination":5,"relation":"det"},{"source":7,"destination":6,"relation":"amod"},{"source":7,"destination":9,"relation":"prep_of"},{"source":10,"destination":19,"relation":"nsubj"},{"source":10,"destination":7,"relation":"dep"},{"source":10,"destination":11,"relation":"dobj"},{"source":11,"destination":18,"relation":"prep_of"},{"source":11,"destination":14,"relation":"prep_of"},{"source":14,"destination":18,"relation":"conj_and"},{"source":14,"destination":13,"relation":"det"},{"source":18,"destination":16,"relation":"nn"},{"source":18,"destination":17,"relation":"nn"}],"roots":[10]}}}]} """)
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
  val doc3 = jsonStringToDocument(""" {"sentences":[{"words":["."],"startOffsets":[0],"endOffsets":[1],"tags":["."],"lemmas":["."],"entities":["O"],"norms":["O"],"chunks":["O"],"graphs":{"stanford-basic":{"edges":[],"roots":[0]},"stanford-collapsed":{"edges":[],"roots":[0]}}}]} """)
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
