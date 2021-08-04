package org.clulab.processors

/**
  * Unit tests for CluProcessor
  * User: mihais
  * Date: 6/17/17
  */
class TestCluProcessor extends FatdynetTest {

  "CluProcessor" should "tokenize raw text correctly" in {
    val doc = proc.mkDocument("John Doe went to China. There, he visited Beijing.")
    doc.clear()

    doc.sentences(0).words(0) should be ("John")
    doc.sentences(0).words(1) should be ("Doe")
    doc.sentences(0).words(2) should be ("went")
    doc.sentences(0).words(3) should be ("to")
    doc.sentences(0).words(4) should be ("China")
    doc.sentences(0).words(5) should be (".")
    doc.sentences(1).words(0) should be ("There")
    doc.sentences(1).words(1) should be (",")
    doc.sentences(1).words(2) should be ("he")
    doc.sentences(1).words(3) should be ("visited")
    doc.sentences(1).words(4) should be ("Beijing")
    doc.sentences(1).words(5) should be (".")

    doc.sentences(0).startOffsets(0) should be (0)
    doc.sentences(0).startOffsets(1) should be (5)
    doc.sentences(0).startOffsets(2) should be (9)
    doc.sentences(0).startOffsets(3) should be (14)
    doc.sentences(0).startOffsets(4) should be (17)
    doc.sentences(0).startOffsets(5) should be (22)
    doc.sentences(1).startOffsets(0) should be (24)
    doc.sentences(1).startOffsets(1) should be (29)
    doc.sentences(1).startOffsets(2) should be (31)
    doc.sentences(1).startOffsets(3) should be (34)
    doc.sentences(1).startOffsets(4) should be (42)
    doc.sentences(1).startOffsets(5) should be (49)
    println("Tokenization is fine.")
  }

  it should "POS tag correctly" in {
    val doc = proc.mkDocument("John Doe went to China. There, he visited Beijing.")
    proc.mkConstEmbeddings(doc)
    proc.lemmatize(doc)
    proc.tagPartsOfSpeech(doc)
    doc.clear()
    
    doc.sentences(0).tags.get(0) should be ("NNP")
    doc.sentences(0).tags.get(1) should be ("NNP")
    doc.sentences(0).tags.get(2) should be ("VBD")
    doc.sentences(0).tags.get(3) should be ("TO")
    doc.sentences(0).tags.get(4) should be ("NNP")
    doc.sentences(0).tags.get(5) should be (".")
    // doc.sentences(1).tags.get(0) should be ("RB")
    doc.sentences(1).tags.get(1) should be (",")
    doc.sentences(1).tags.get(2) should be ("PRP")
    doc.sentences(1).tags.get(3) should be ("VBD")
    doc.sentences(1).tags.get(4) should be ("NNP")
    doc.sentences(1).tags.get(5) should be (".")
    println("POS tagging is fine.")
  }

  it should "POS tag parentheses correctly" in {
    val doc = proc.mkDocument("This is a test (of parentheses).")
    proc.mkConstEmbeddings(doc)
    proc.lemmatize(doc)
    proc.tagPartsOfSpeech(doc)

    doc.sentences(0).tags.get(4) should be ("-LRB-")
    doc.sentences(0).tags.get(7) should be ("-RRB-")
  }

  it should "recognize syntactic chunks correctly" in {
    val doc = proc.mkDocument("He reckons the current account deficit will narrow to only 1.8 billion.")
    proc.mkConstEmbeddings(doc)
    proc.lemmatize(doc)
    proc.tagPartsOfSpeech(doc)
    proc.chunking(doc)
    doc.clear()

    doc.sentences(0).chunks.get(0) should be ("B-NP")
    doc.sentences(0).chunks.get(1) should be ("B-VP")
    doc.sentences(0).chunks.get(2) should be ("B-NP")
    doc.sentences(0).chunks.get(3) should be ("I-NP")
    doc.sentences(0).chunks.get(4) should be ("I-NP")
    doc.sentences(0).chunks.get(5) should be ("I-NP")
    doc.sentences(0).chunks.get(6) should be ("B-VP")
    doc.sentences(0).chunks.get(7) should be ("I-VP")
    doc.sentences(0).chunks.get(8) should be ("B-PP")
    doc.sentences(0).chunks.get(9) should be ("B-NP")
    doc.sentences(0).chunks.get(10) should be ("I-NP")
    doc.sentences(0).chunks.get(11) should be ("I-NP")
  }

  it should "lemmatize text correctly" in {
    val doc = proc.mkDocument("John Doe went to the shops.")
    proc.mkConstEmbeddings(doc)
    proc.lemmatize(doc)
    doc.clear()

    doc.sentences(0).lemmas.get(0) should be ("john")
    doc.sentences(0).lemmas.get(2) should be ("go")
    doc.sentences(0).lemmas.get(5) should be ("shop")
    println("Lemmatization is fine.")
  }

  it should "recognize semantic roles correctly" in {
    val doc = proc.annotate("John Doe visited China.")

    doc.sentences.head.semanticRoles.get.hasEdge(2, 1, "A0") should be (true)
    doc.sentences.head.semanticRoles.get.hasEdge(2, 3, "A1") should be (true)
  }

  it should "parse text correctly" in {
    val sent = "John likes cake that contains chocolate."
    val doc = proc.annotate(sent)

    println(s"Basic universal dependencies for sentence: $sent")
    println(doc.sentences.head.universalBasicDependencies.get)

    doc.sentences.head.universalBasicDependencies.get.hasEdge(1, 0, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(1, 2, "dobj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(2, 4, "acl:relcl") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 3, "nsubj") should be(true)
    doc.sentences.head.universalBasicDependencies.get.hasEdge(4, 5, "dobj") should be(true)
    println("Parsing is fine.")
  }

  it should "create semantic dependencies of the correct length" in {
    val text = "John ate cake, zz zz zz."
    val doc = proc.annotate(text)

    val sent = doc.sentences.head

    sent.semanticRoles.get.outgoingEdges.length should be(sent.size)
    sent.semanticRoles.get.incomingEdges.length should be(sent.size)
    sent.enhancedSemanticRoles.get.outgoingEdges.length should be(sent.size)
    sent.enhancedSemanticRoles.get.incomingEdges.length should be(sent.size)
  }

  it should "parse MWEs correctly" in {
    var doc = proc.mkDocument("Foods such as icecream are tasty.")
    println(s"WORDS: ${doc.sentences.head.words.mkString(", ")}")

    proc.annotate(doc)

    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(0, 3, "nmod_such_as") should be (true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(0, 3, "nmod") should be (false)

    doc = proc.mkDocument("There was famine due to drought.")
    println(s"WORDS: ${doc.sentences.head.words.mkString(", ")}")

    proc.annotate(doc)

    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 5, "nmod_due_to") should be (true)
    doc.sentences.head.universalEnhancedDependencies.get.hasEdge(2, 5, "nmod") should be (false)
  }

  it should "recognize dates" in {
    var sent = proc.annotate("It was 12 January, 2021.").sentences.head
    sent.entities.get(2) should be ("B-DATE")
    sent.entities.get(3) should be ("I-DATE")
    sent.entities.get(4) should be ("I-DATE")
    sent.entities.get(5) should be ("I-DATE")
    sent.norms.get(2) should be ("2021-01-12")

    sent = proc.annotate("It was January 12.").sentences.head
    sent.entities.get(2) should be ("B-DATE")
    sent.entities.get(3) should be ("I-DATE")
    sent.norms.get(2) should be ("XXXX-01-12")
  }

  it should "recognize date ranges" in {
    val sent = proc.annotate("It happened between 12 January, 2021 and Feb. 2022.").sentences.head
    sent.entities.get(2) should be ("B-DATE-RANGE")
    sent.entities.get(3) should be ("I-DATE-RANGE")
    sent.entities.get(4) should be ("I-DATE-RANGE")
    sent.entities.get(5) should be ("I-DATE-RANGE")
    sent.entities.get(6) should be ("I-DATE-RANGE")
    sent.entities.get(7) should be ("I-DATE-RANGE")
    sent.entities.get(8) should be ("I-DATE-RANGE")
    sent.entities.get(9) should be ("I-DATE-RANGE")
    sent.norms.get(2) should be ("2021-01-12 -- 2022-02-XX")
  }

  it should "recognize custom names entities" in {
    val sent = proc.annotate("Jack Doe and John Doe were friends in yyy zzz.").sentences.head
    sent.entities.get(0) should be ("B-D")
    sent.entities.get(1) should be ("I-D")
    sent.entities.get(3) should be ("B-PER")
    sent.entities.get(4) should be ("I-PER")
    sent.entities.get(8) should be ("B-D")
    sent.entities.get(9) should be ("I-D")
  }

  /* // TODO
  it should "parse a long sentence correctly" in {
    val doc = proc.annotate("Her T score of 63 on the Attention Problems scale is in the At Risk range suggesting that she sometimes daydreams or is easily distracted and unable to concentrate more than momentarily .")
    //println(s"Sentence: ${doc.sentences(0).words.mkString(" ")}")
    //println("Basic universal dependencies:")
    //println(doc.sentences.head.universalBasicDependencies.get)

    doc.sentences.head.universalBasicDependencies.isDefined should be (true)
    val deps = doc.sentences.head.universalBasicDependencies.get

    (deps.incomingEdges != null) should be (true)
    (deps.outgoingEdges != null) should be (true)

    deps.incomingEdges.length == 33 should be (true)
    deps.outgoingEdges.length == 33 should be (true)

    deps.hasEdge(2, 0, "nmod:poss") should be (true)
    deps.hasEdge(2, 1, "compound") should be (true)
    deps.hasEdge(2, 9, "nmod") should be (true)
  }
  */
}
