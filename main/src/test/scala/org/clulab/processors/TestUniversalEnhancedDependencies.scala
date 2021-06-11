package org.clulab.processors

class TestUniversalEnhancedDependencies extends FatdynetTest {

  "CluProcessor" should "parse some basic sentences correctly" in {
    var doc = proc.annotate("Ras1 is associated with cancer.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubjpass") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "auxpass") should be(true)

    doc = proc.annotate("Ras1 has phosphorylated Mek2.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)

    doc = proc.annotate("John has travelled to China.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 4, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 3, "case") should be(true)

    doc = proc.annotate("John is traveling to China.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 4, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 3, "case") should be(true)

    doc = proc.annotate("John Doe will travel to China.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 1, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(1, 0, "compound") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 2, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 5, "nmod") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(5, 4, "case") should be(true)

    doc = proc.annotate("Mary has been reading a book")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 1, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 2, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(3, 5, "dobj") should be(true)

    doc = proc.annotate("Mary is reading a book.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 1, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 4, "dobj") should be(true)

    doc = proc.annotate("Paul and Mary are reading a book.")
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(0, 2, "conj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 3, "aux") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 6, "dobj") should be(true)
  }

  it should "collapse prepositions" in {
    val doc = proc.annotate("Mary traveled to China")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(1, 3, "nmod_to") should be(true) 
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(1, 2, "nmod") should be(false)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(3, 2, "case") should be(true)
  }

  it should "identify agents in passive voice" in {
    val doc = proc.annotate("Mary was congratulated by Jane")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 4, "nmod_by") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 0, "nsubjpass") should be(true)
  }

  it should "capture raised subjects" in {
    val doc = proc.annotate("Mary wants to buy a book")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(3, 0, "nsubj") should be(true)
  }

  it should "propagate subjects and objects in conjoined verbs" in {
    var doc = proc.annotate("The store buys and sells cameras.")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 1, "nsubj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(4, 1, "nsubj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(4, 5, "dobj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 5, "dobj") should be(true)

    doc = proc.annotate("Cameras are bought and sold by the store")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 0, "nsubjpass") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(4, 0, "nsubjpass") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 7, "nmod_by") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(4, 7, "nmod_by") should be(true)

    doc = proc.annotate("She was watching a movie or reading a book")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 0, "nsubj") should be(true)
    //doc.sentences.head.universalEnhancedDependencies.get.hasEdge(6, 0, "nsubj") should be(true) // TODO: this currently fails with CluProcessor, because the conj is incorrectly assigned between 4 and 6 (rather than 2 and 6)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 4, "dobj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(6, 8, "dobj") should be(true)

    doc = proc.annotate("She was watching a movie or reading")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(6, 4, "dobj") should be(false)
  }

  it should "propagate conjoined subjects and objects to same verb" in {
    var doc = proc.annotate("Paul and Mary are reading a book")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(4, 0, "nsubj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(4, 2, "nsubj") should be(true)

    doc = proc.annotate("John is reading a book and a newspaper")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 4, "dobj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 7, "dobj") should be(true)

    doc = proc.annotate("Mary and John wanted to buy a hat")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(3, 0, "nsubj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(3, 2, "nsubj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(5, 0, "nsubj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(5, 2, "nsubj") should be(true)
  }

  it should "push subjects/objects inside relative clauses" in {
    var doc = proc.annotate("the boy who lived") // FAIL
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(3, 1, "nsubj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(3, 2, "nsubj") should be(false)

    doc = proc.annotate("the book, which I read, was great.")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(5, 4, "nsubj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(5, 1, "dobj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(5, 3, "dobj") should be(false)
  }

  it should "generate correct conj_ dependencies" in {
    var doc = proc.annotate("Disagreements over land rights for crop cultivation and livestock grazing continue to be a major source of conflict.")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(6, 9, "conj_and") should be(true)

    doc = proc.annotate("Because of this inadequate transportation infrastructure, it is difficult and expensive for subsistence farmers to transport surpluses to markets.")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(9, 11, "conj_and") should be(true)

    doc = proc.annotate("Use of improved cultivars and mechanization will be increased and use of critical interventions may lead to increases in productivity and efficient use of resources.")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(3, 5, "conj_and") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(8, 15, "ccomp") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(19, 21, "conj_and") should be(true) // this is not great, but better than nothing...
  }

  it should "create xsubj dependencies" in {
    val doc = proc.annotate("Disagreements over land rights for crop cultivation and livestock grazing continue to be a major source of conflict.")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(0, 15, "nsubj:xsubj") should be(true)
  }

  it should "replicate copulative nsubj across conjunctions" in {
    val doc = proc.annotate("Because of this inadequate transportation infrastructure, it is difficult and expensive for subsistence farmers to transport surpluses to markets.")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(9, 7, "nsubj") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(11, 7, "nsubj") should be(true)
  }

  it should "replicate nmod_ dependencies across conjunctions" in {
    var doc = proc.annotate("Conflict and economic decline have led to violence and displacement.")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(5, 7, "nmod_to") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(5, 9, "nmod_to") should be(true)

    doc = proc.annotate("Use of improved cultivars and mechanization will be increased and use of critical interventions may lead to increases in productivity and efficient use of resources.")
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(0, 3, "nmod_of") should be(true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(0, 5, "nmod_of") should be(true)
  }
}
