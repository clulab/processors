package org.clulab.processors

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.scalatest.{FlatSpec, Matchers}

/**
  * Makes sure that we process parentheses correctly
  * The corpora used to train the dependency parsers and the NERs keep parentheses NOT normalized
  * The original Treebank used to train the constituent parser normalized them, e.g., "(" becomes "-LRB-"
  * So, we'll keep parens NOT normalized since this is more common, and normalize them inside the constituent parser
  *     (see CoreNLPProcessor)
  */
class TestParenthesesInCore extends FlatSpec with Matchers {
  val fast = new FastNLPProcessor()

  "CluProcessor" should "tokenize, lemmatize, and POS tag parentheses correctly" in {
    val doc = fast.mkDocument("Moreover, in von Willebrand factor-stimulated platelets, the tyrosine phosphorylation of pp60(c-src) is closely associated with the activation of phosphatidylinositol 3-kinase (PIK), and two adhesion receptors, glycoprotein (Gp)Ib and GpIIb/IIIa(alpha-IIb-beta(3)), are involved. ")
    fast.tagPartsOfSpeech(doc)
    fast.lemmatize(doc)
    doc.clear()

    val s = doc.sentences(0)
    for(i <- s.indices) {
      println(s"$i: ${s.words(i)} ${s.lemmas.get(i)} ${s.tags.get(i)}")
    }

    for(i <- List(13, 25, 35, 43, 45)) {
      s.words(i) should be("(")
      s.lemmas.get(i) should be("(")
      s.tags.get(i) should be("-LRB-")
    }

    for(i <- List(15, 27, 37, 47, 48)) {
      s.words(i) should be(")")
      s.lemmas.get(i) should be(")")
      s.tags.get(i) should be("-RRB-")
    }
  }
}
