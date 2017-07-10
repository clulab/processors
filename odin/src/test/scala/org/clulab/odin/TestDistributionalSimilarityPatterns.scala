package org.clulab.odin

import org.clulab.TestUtils._
import org.clulab.TestUtils.jsonStringToDocument
import org.scalatest.{FlatSpec, Matchers}


class TestDistributionalSimilarityPatterns extends FlatSpec with Matchers {

  // the rule file containing the path to the embeddings resources
  val rf = "org/clulab/odin/grammars/embeddings.yml"

  val rules = readFile(rf)
  val ee = ExtractorEngine(rules)

  val felineText = "I saw a leopard climbing a tree."

  felineText should "produce a mention with the label \"Feline\" for the word \"leopard\"" in {
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["I","saw","a","leopard","climbing","a","tree","."],"startOffsets":[0,2,6,8,16,25,27,31],"endOffsets":[1,5,7,15,24,26,31,32],"tags":["PRP","VBD","DT","NN","VBG","DT","NN","."],"lemmas":["I","see","a","leopard","climb","a","tree","."],"entities":["O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","B-VP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"dobj"},{"source":1,"destination":7,"relation":"punct"},{"source":3,"destination":2,"relation":"det"},{"source":3,"destination":4,"relation":"vmod"},{"source":4,"destination":6,"relation":"dobj"},{"source":6,"destination":5,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"dobj"},{"source":1,"destination":7,"relation":"punct"},{"source":3,"destination":2,"relation":"det"},{"source":3,"destination":4,"relation":"vmod"},{"source":4,"destination":6,"relation":"dobj"},{"source":6,"destination":5,"relation":"det"}],"roots":[1]}}}]} """)
    val mentions = ee.extractFrom(doc)
    val canines = mentions filter(_ matches "Canine")
    val felines = mentions filter(_ matches "Feline")
    // we shouldn't find any Canines
    canines should be (empty)
    felines should have size (1)
    felines.head.text should equal("leopard")
  }

}
