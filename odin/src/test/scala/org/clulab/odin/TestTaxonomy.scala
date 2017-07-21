package org.clulab.odin

import org.clulab.TestUtils._
import org.scalatest.{ FlatSpec, Matchers }


/**
  * Tests for taxonomy inclusion in Mention labels.
  */
class TestTaxonomy extends FlatSpec with Matchers {

  val rules =
    """
      |taxonomy:
      |  - Ninja:
      |    - MutantRat
      |    - MutantTurtle
      |
      |rules:
      |  - name: "turtle-power"
      |    label: MutantTurtle
      |    type: token
      |    pattern: |
      |      [word=/(?i)^(leonardo|donatello|raphael|michelangelo)$/]
      |
      |  - name: "rat-power"
      |    label: MutantRat
      |    type: token
      |    pattern: |
      |      Master? [word=/(?i)^splinter$/]
    """.stripMargin

  "taxonomy" should "inform a rule's labels" in {
    //    Leonardo leads, Donatello does machines (That's a fact, jack!)
    //    Raphael is cool but (c)rude (Gimme a break!)
    //    Michelangelo is a party dude (Party!)
    val doc1 = jsonStringToDocument(""" {"sentences":[{"words":["Leonardo","leads",",","Donatello","does","machines","-LRB-","That","'s","a","fact",",","jack","!","-RRB-"],"startOffsets":[1,10,15,17,27,32,41,42,46,49,51,55,57,61,62],"endOffsets":[9,15,16,26,31,40,42,46,48,50,55,56,61,62,63],"tags":["NNP","VBZ",",","NNP","VBZ","NNS","-LRB-","DT","VBZ","DT","NN",",","NN",".","-RRB-"],"lemmas":["Leonardo","lead",",","Donatello","do","machine","-lrb-","that","be","a","fact",",","jack","!","-rrb-"],"entities":["PERSON","O","O","PERSON","O","O","O","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","O","B-NP","B-VP","B-NP","O","B-NP","B-VP","B-NP","I-NP","O","B-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"punct"},{"source":1,"destination":5,"relation":"conj"},{"source":5,"destination":3,"relation":"nn"},{"source":5,"destination":4,"relation":"aux"},{"source":5,"destination":10,"relation":"dep"},{"source":10,"destination":6,"relation":"punct"},{"source":10,"destination":7,"relation":"nsubj"},{"source":10,"destination":8,"relation":"cop"},{"source":10,"destination":9,"relation":"det"},{"source":10,"destination":11,"relation":"punct"},{"source":10,"destination":12,"relation":"appos"},{"source":10,"destination":14,"relation":"punct"},{"source":12,"destination":13,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"punct"},{"source":1,"destination":5,"relation":"conj"},{"source":5,"destination":3,"relation":"nn"},{"source":5,"destination":4,"relation":"aux"},{"source":5,"destination":10,"relation":"dep"},{"source":10,"destination":6,"relation":"punct"},{"source":10,"destination":7,"relation":"nsubj"},{"source":10,"destination":8,"relation":"cop"},{"source":10,"destination":9,"relation":"det"},{"source":10,"destination":11,"relation":"punct"},{"source":10,"destination":12,"relation":"appos"},{"source":10,"destination":14,"relation":"punct"},{"source":12,"destination":13,"relation":"punct"}],"roots":[1]}}},{"words":["Raphael","is","cool","but","-LRB-","c","-RRB-","rude","-LRB-","Gim","me","a","break","!","-RRB-"],"startOffsets":[64,72,75,80,84,85,86,87,92,93,96,99,101,106,107],"endOffsets":[71,74,79,83,85,86,87,91,93,96,98,100,106,107,108],"tags":["NNP","VBZ","JJ","CC","-LRB-","LS","-RRB-","JJ","-LRB-","NNP","PRP","DT","NN",".","-RRB-"],"lemmas":["Raphael","be","cool","but","-lrb-","c","-rrb-","rude","-lrb-","Gim","I","a","break","!","-rrb-"],"entities":["PERSON","O","O","O","O","O","O","O","O","PERSON","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-ADJP","O","O","O","O","O","O","O","B-NP","B-NP","I-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":2,"destination":3,"relation":"cc"},{"source":2,"destination":7,"relation":"conj"},{"source":2,"destination":14,"relation":"punct"},{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"cop"},{"source":5,"destination":4,"relation":"punct"},{"source":5,"destination":6,"relation":"punct"},{"source":7,"destination":5,"relation":"dep"},{"source":7,"destination":9,"relation":"dep"},{"source":9,"destination":8,"relation":"punct"},{"source":9,"destination":12,"relation":"dep"},{"source":9,"destination":13,"relation":"punct"},{"source":12,"destination":10,"relation":"dep"},{"source":12,"destination":11,"relation":"det"}],"roots":[2]},"stanford-collapsed":{"edges":[{"source":2,"destination":7,"relation":"conj_but"},{"source":2,"destination":14,"relation":"punct"},{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"cop"},{"source":5,"destination":4,"relation":"punct"},{"source":5,"destination":6,"relation":"punct"},{"source":7,"destination":5,"relation":"dep"},{"source":7,"destination":9,"relation":"dep"},{"source":7,"destination":0,"relation":"nsubj"},{"source":9,"destination":8,"relation":"punct"},{"source":9,"destination":12,"relation":"dep"},{"source":9,"destination":13,"relation":"punct"},{"source":12,"destination":10,"relation":"dep"},{"source":12,"destination":11,"relation":"det"}],"roots":[2]}}},{"words":["Michelangelo","is","a","party","dude","-LRB-","Party","!","-RRB-"],"startOffsets":[109,122,125,127,133,138,139,144,145],"endOffsets":[121,124,126,132,137,139,144,145,146],"tags":["NNP","VBZ","DT","NN","NN","-LRB-","NNP",".","-RRB-"],"lemmas":["Michelangelo","be","a","party","dude","-lrb-","Party","!","-rrb-"],"entities":["PERSON","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","O","B-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":6,"destination":5,"relation":"punct"},{"source":6,"destination":7,"relation":"punct"},{"source":6,"destination":8,"relation":"punct"},{"source":4,"destination":6,"relation":"dep"},{"source":4,"destination":0,"relation":"nsubj"},{"source":4,"destination":1,"relation":"cop"},{"source":4,"destination":2,"relation":"det"},{"source":4,"destination":3,"relation":"nn"}],"roots":[4]},"stanford-collapsed":{"edges":[{"source":6,"destination":5,"relation":"punct"},{"source":6,"destination":7,"relation":"punct"},{"source":6,"destination":8,"relation":"punct"},{"source":4,"destination":6,"relation":"dep"},{"source":4,"destination":0,"relation":"nsubj"},{"source":4,"destination":1,"relation":"cop"},{"source":4,"destination":2,"relation":"det"},{"source":4,"destination":3,"relation":"nn"}],"roots":[4]}}}]} """)

    val ee = ExtractorEngine(rules)
    val res1 = ee.extractFrom(doc1)
    res1 should have size (4)
    res1.foreach { m =>
      m.label shouldBe "MutantTurtle"
      m.labels should have size 2
      m.labels should contain allOf ("Ninja", "MutantTurtle")
    }

    //    Splinter taught them to be ninja teens (He's a radical rat!)
    val doc2 = jsonStringToDocument(""" {"sentences":[{"words":["Splinter","taught","them","to","be","ninja","teens","-LRB-","He","'s","a","radical","rat","!","-RRB-"],"startOffsets":[0,9,16,21,24,27,33,39,40,42,45,47,55,58,59],"endOffsets":[8,15,20,23,26,32,38,40,42,44,46,54,58,59,60],"tags":["NN","VBD","PRP","TO","VB","JJ","NNS","-LRB-","PRP","VBZ","DT","JJ","NN",".","-RRB-"],"lemmas":["splinter","teach","they","to","be","ninja","teens","-lrb-","he","be","a","radical","rat","!","-rrb-"],"entities":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","B-VP","I-VP","B-NP","I-NP","O","B-NP","B-VP","B-NP","I-NP","I-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":6,"relation":"xcomp"},{"source":6,"destination":3,"relation":"aux"},{"source":6,"destination":4,"relation":"cop"},{"source":6,"destination":5,"relation":"amod"},{"source":6,"destination":12,"relation":"dep"},{"source":12,"destination":7,"relation":"punct"},{"source":12,"destination":8,"relation":"nsubj"},{"source":12,"destination":9,"relation":"cop"},{"source":12,"destination":10,"relation":"det"},{"source":12,"destination":11,"relation":"amod"},{"source":12,"destination":13,"relation":"punct"},{"source":12,"destination":14,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":6,"relation":"xcomp"},{"source":6,"destination":3,"relation":"aux"},{"source":6,"destination":4,"relation":"cop"},{"source":6,"destination":5,"relation":"amod"},{"source":6,"destination":12,"relation":"dep"},{"source":12,"destination":7,"relation":"punct"},{"source":12,"destination":8,"relation":"nsubj"},{"source":12,"destination":9,"relation":"cop"},{"source":12,"destination":10,"relation":"det"},{"source":12,"destination":11,"relation":"amod"},{"source":12,"destination":13,"relation":"punct"},{"source":12,"destination":14,"relation":"punct"}],"roots":[1]}}}]} """)
    val res2 = ee.extractFrom(doc2)
    res2 should have size (1)
    res2.foreach { m =>
      m.label shouldBe "MutantRat"
      m.labels should have size 2
      m.labels should contain allOf ("Ninja", "MutantRat")
    }

  }
}
