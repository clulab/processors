package org.clulab.odin

import org.scalatest._
import org.clulab.TestUtils.jsonStringToDocument


class TestMultiSentencePatterns extends FlatSpec with Matchers {

  val text1 = "Barack Obama is the 44th President of the United States. He was born on August 4, 1961 in Honolulu, Hawaii."

  text1 should """produce a RelationMention with the label "Coref"""" in {

    val doc = jsonStringToDocument(""" {"sentences":[{"words":["Barack","Obama","is","the","44th","President","of","the","United","States","."],"startOffsets":[0,7,13,16,20,25,35,38,42,49,55],"endOffsets":[6,12,15,19,24,34,37,41,48,55,56],"tags":["NNP","NNP","VBZ","DT","JJ","NN","IN","DT","NNP","NNPS","."],"lemmas":["Barack","Obama","be","the","44th","president","of","the","United","States","."],"entities":["PERSON","PERSON","O","O","ORDINAL","O","O","O","LOCATION","LOCATION","O"],"norms":["O","O","O","O","44.0","O","O","O","O","O","O"],"chunks":["B-NP","I-NP","B-VP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","O"],"graphs":{}},{"words":["He","was","born","on","August","4",",","1961","in","Honolulu",",","Hawaii","."],"startOffsets":[57,60,64,69,72,79,80,82,87,90,98,100,106],"endOffsets":[59,63,68,71,78,80,81,86,89,98,99,106,107],"tags":["PRP","VBD","VBN","IN","NNP","CD",",","CD","IN","NNP",",","NNP","."],"lemmas":["he","be","bear","on","August","4",",","1961","in","Honolulu",",","Hawaii","."],"entities":["O","O","O","O","DATE","DATE","DATE","DATE","O","LOCATION","O","LOCATION","O"],"norms":["O","O","O","O","1961-08-04","1961-08-04","1961-08-04","1961-08-04","O","O","O","O","O"],"chunks":["B-NP","B-VP","I-VP","B-PP","B-NP","I-NP","O","B-NP","B-PP","B-NP","O","B-NP","O"],"graphs":{}}]} """)

    // try with rule1
    val rule1 =
      """
        |- name: coref-example
        |  label: Coref
        |  priority: 2
        |  # the maximum number of sentences to search to the left of the anchor
        |  left-window: 1
        |  # don't attempt to match anywhere to the right of the anchor
        |  right-window: -1
        |  example: "Barack Obama is the 44th President of the United States. He was born on August 4, 1961 in Honolulu, Hawaii."
        |  type: cross-sentence
        |  pattern: |
        |    # start here at the pattern's anchor
        |    # the pronoun to be resolved
        |    anaphor: Entity = [lemma="he"]
        |    antecedent: Entity = Barack Obama
      """.stripMargin
    val ee1 = ExtractorEngine(rule1)
    val mentions1 = ee1.extractFrom(doc)
    mentions1 should have size (1)
    (mentions1.head matches "Coref") should be (true)

    // try with rule2
    val rule2 =
      """
        |- name: coref-example
        |  label: Coref
        |  priority: 2
        |  # the maximum number of sentences to search to the left of the anchor
        |  left-window: 1
        |  # don't attempt to match anywhere to the right of the anchor
        |  example: "Barack Obama is the 44th President of the United States. He was born on August 4, 1961 in Honolulu, Hawaii."
        |  type: cross-sentence
        |  pattern: |
        |    # start here at the pattern's anchor
        |    # the pronoun to be resolved
        |    anaphor: Entity = [lemma="he"]
        |    antecedent: Entity = Barack Obama
      """.stripMargin
    val ee2 = ExtractorEngine(rule2)
    val mentions2 = ee2.extractFrom(doc)
    mentions2 should have size (1)
    (mentions2.head matches "Coref") should be (true)

    // try with rule3
    val rule3 =
      """
        |- name: coref-example
        |  label: Coref
        |  priority: 2
        |  # the maximum number of sentences to search to the left of the anchor
        |  left-window: 1
        |  # only attempt neighbor matches to the right of the anchor in the current sentence
        |  right-window: 0
        |  example: "Barack Obama is the 44th President of the United States. He was born on August 4, 1961 in Honolulu, Hawaii."
        |  type: cross-sentence
        |  pattern: |
        |    # start here at the pattern's anchor
        |    # the pronoun to be resolved
        |    anaphor: Entity = [lemma="he"]
        |    antecedent: Entity = Barack Obama
      """.stripMargin
    val ee3 = ExtractorEngine(rule3)
    val mentions3 = ee3.extractFrom(doc)
    mentions3 should have size (1)
    (mentions3.head matches "Coref") should be (true)

  }

}
