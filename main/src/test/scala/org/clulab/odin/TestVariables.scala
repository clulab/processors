package org.clulab.odin

import scala.io.Source

import org.clulab.TestUtils._
import org.scalatest.{ Matchers, FlatSpec }


class TestVariables extends FlatSpec with Matchers {

  def readResource(filename: String): String = {
    val source = Source.fromURL(getClass.getResource(filename))
    val data = source.mkString
    source.close()
    data
  }

  "variables" should "allow for whitespace" in {

    val mf =
      """
        |vars:
        |  rule_name_prefix: "test"
        |
        |rules:
        |  - name: "${ rule_name_prefix     }-1"
        |    label: Test
        |    type: token
        |    pattern: test
      """.stripMargin

    val ee = ExtractorEngine(mf)
    ee.extractors should have size (1)
    ee.extractors.head.name should startWith ("test")
  }

  it should "be applied to imports" in {

    val mf =
      """
        |vars:
        |  path_prefix: org/clulab/odin/variables
        |  my_prefix: "highest-precedence"
        |
        |rules:
        |  - import: ${ path_prefix }/test_imports.yml
      """.stripMargin

    val ee = ExtractorEngine(mf)
    ee.extractors should have size (1)
  }

  it should "resolve variables in order of precedence" in {

    val mf =
      """
        |vars:
        |  path_prefix: org/clulab/odin/variables
        |  my_prefix: "highest-precedence"
        |
        |rules:
        |  - import: ${ path_prefix }/test_imports.yml
        |    vars:
        |      name_prefix: ${my_prefix } # should be able to resolve from top-level vars block
      """.stripMargin

    val ee = ExtractorEngine.fromRules(mf)
    ee.extractors should have size (1)
    ee.extractors.head.name should startWith ("highest-precedence")
  }

  it should "support nested resolution" in {

    val mf =
      """
        |vars:
        |  n1: n2
        |  n2: "success"
        |
        |rules:
        |  - name: "${ ${ n1}     }-1"  # value of name should be "success-1" after nested substitutions
        |    label: Test
        |    type: token
        |    pattern: test
      """.stripMargin

    val ee = ExtractorEngine.fromRules(mf)
    ee.extractors should have size (1)
    println(ee.extractors.head.name)
    ee.extractors.head.name should startWith ("success")
  }

  it should "disallow recursive definitions" in {

    val mf =
      """
        |vars:
        |  v1: ${v1}
        |
        |rules:
        |  - name: "${v1}"
        |    label: Test
        |    type: token
        |    pattern: test
      """.stripMargin

    an [java.lang.IllegalStateException] should be thrownBy ExtractorEngine.fromRules(mf)

  }

  it should "allow vars in lists" in {

    val rules =
      """
        |vars:
        |  turtle:
        |     - leonardo
        |     - donatello
        |     - raphael
        |     - michelangelo
        |
        |rules:
        |  - name: "turtle-power-var"
        |    label: MutantTurtle
        |    type: token
        |    pattern: |
        |      [word=/(?i)^${turtle}/]
      """.stripMargin

    //    Leonardo leads, Donatello does machines (That's a fact, jack!)
    //    Raphael is cool but (c)rude (Gimme a break!)
    //    Michelangelo is a party dude (Party!)
    val doc1 = jsonStringToDocument(""" {"sentences":[{"raw":["Leonardo","leads",",","Donatello","does","machines","-LRB-","That","'s","a","fact",",","jack","!","-RRB-"],"words":["Leonardo","leads",",","Donatello","does","machines","-LRB-","That","'s","a","fact",",","jack","!","-RRB-"],"startOffsets":[1,10,15,17,27,32,41,42,46,49,51,55,57,61,62],"endOffsets":[9,15,16,26,31,40,42,46,48,50,55,56,61,62,63],"tags":["NNP","VBZ",",","NNP","VBZ","NNS","-LRB-","DT","VBZ","DT","NN",",","NN",".","-RRB-"],"lemmas":["Leonardo","lead",",","Donatello","do","machine","-lrb-","that","be","a","fact",",","jack","!","-rrb-"],"entities":["PERSON","O","O","PERSON","O","O","O","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","O","B-NP","B-VP","B-NP","O","B-NP","B-VP","B-NP","I-NP","O","B-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"punct"},{"source":1,"destination":5,"relation":"conj"},{"source":5,"destination":3,"relation":"nn"},{"source":5,"destination":4,"relation":"aux"},{"source":5,"destination":10,"relation":"dep"},{"source":10,"destination":6,"relation":"punct"},{"source":10,"destination":7,"relation":"nsubj"},{"source":10,"destination":8,"relation":"cop"},{"source":10,"destination":9,"relation":"det"},{"source":10,"destination":11,"relation":"punct"},{"source":10,"destination":12,"relation":"appos"},{"source":10,"destination":14,"relation":"punct"},{"source":12,"destination":13,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"punct"},{"source":1,"destination":5,"relation":"conj"},{"source":5,"destination":3,"relation":"nn"},{"source":5,"destination":4,"relation":"aux"},{"source":5,"destination":10,"relation":"dep"},{"source":10,"destination":6,"relation":"punct"},{"source":10,"destination":7,"relation":"nsubj"},{"source":10,"destination":8,"relation":"cop"},{"source":10,"destination":9,"relation":"det"},{"source":10,"destination":11,"relation":"punct"},{"source":10,"destination":12,"relation":"appos"},{"source":10,"destination":14,"relation":"punct"},{"source":12,"destination":13,"relation":"punct"}],"roots":[1]}}},{"raw":["Raphael","is","cool","but","-LRB-","c","-RRB-","rude","-LRB-","Gim","me","a","break","!","-RRB-"], "words":["Raphael","is","cool","but","-LRB-","c","-RRB-","rude","-LRB-","Gim","me","a","break","!","-RRB-"],"startOffsets":[64,72,75,80,84,85,86,87,92,93,96,99,101,106,107],"endOffsets":[71,74,79,83,85,86,87,91,93,96,98,100,106,107,108],"tags":["NNP","VBZ","JJ","CC","-LRB-","LS","-RRB-","JJ","-LRB-","NNP","PRP","DT","NN",".","-RRB-"],"lemmas":["Raphael","be","cool","but","-lrb-","c","-rrb-","rude","-lrb-","Gim","I","a","break","!","-rrb-"],"entities":["PERSON","O","O","O","O","O","O","O","O","PERSON","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-ADJP","O","O","O","O","O","O","O","B-NP","B-NP","I-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":2,"destination":3,"relation":"cc"},{"source":2,"destination":7,"relation":"conj"},{"source":2,"destination":14,"relation":"punct"},{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"cop"},{"source":5,"destination":4,"relation":"punct"},{"source":5,"destination":6,"relation":"punct"},{"source":7,"destination":5,"relation":"dep"},{"source":7,"destination":9,"relation":"dep"},{"source":9,"destination":8,"relation":"punct"},{"source":9,"destination":12,"relation":"dep"},{"source":9,"destination":13,"relation":"punct"},{"source":12,"destination":10,"relation":"dep"},{"source":12,"destination":11,"relation":"det"}],"roots":[2]},"stanford-collapsed":{"edges":[{"source":2,"destination":7,"relation":"conj_but"},{"source":2,"destination":14,"relation":"punct"},{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"cop"},{"source":5,"destination":4,"relation":"punct"},{"source":5,"destination":6,"relation":"punct"},{"source":7,"destination":5,"relation":"dep"},{"source":7,"destination":9,"relation":"dep"},{"source":7,"destination":0,"relation":"nsubj"},{"source":9,"destination":8,"relation":"punct"},{"source":9,"destination":12,"relation":"dep"},{"source":9,"destination":13,"relation":"punct"},{"source":12,"destination":10,"relation":"dep"},{"source":12,"destination":11,"relation":"det"}],"roots":[2]}}},{"raw":["Michelangelo","is","a","party","dude","-LRB-","Party","!","-RRB-"],"words":["Michelangelo","is","a","party","dude","-LRB-","Party","!","-RRB-"],"startOffsets":[109,122,125,127,133,138,139,144,145],"endOffsets":[121,124,126,132,137,139,144,145,146],"tags":["NNP","VBZ","DT","NN","NN","-LRB-","NNP",".","-RRB-"],"lemmas":["Michelangelo","be","a","party","dude","-lrb-","Party","!","-rrb-"],"entities":["PERSON","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","O","B-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":6,"destination":5,"relation":"punct"},{"source":6,"destination":7,"relation":"punct"},{"source":6,"destination":8,"relation":"punct"},{"source":4,"destination":6,"relation":"dep"},{"source":4,"destination":0,"relation":"nsubj"},{"source":4,"destination":1,"relation":"cop"},{"source":4,"destination":2,"relation":"det"},{"source":4,"destination":3,"relation":"nn"}],"roots":[4]},"stanford-collapsed":{"edges":[{"source":6,"destination":5,"relation":"punct"},{"source":6,"destination":7,"relation":"punct"},{"source":6,"destination":8,"relation":"punct"},{"source":4,"destination":6,"relation":"dep"},{"source":4,"destination":0,"relation":"nsubj"},{"source":4,"destination":1,"relation":"cop"},{"source":4,"destination":2,"relation":"det"},{"source":4,"destination":3,"relation":"nn"}],"roots":[4]}}}]} """)
    val ee = ExtractorEngine(rules)
    val res1 = ee.extractFrom(doc1)

    res1 should have size (4)

    res1.exists(m => m.text == "Leonardo") should be (true)
    res1.exists(m => m.text == "Donatello") should be (true)
    res1.exists(m => m.text == "Raphael") should be (true)
    res1.exists(m => m.text == "Michelangelo") should be (true)
  }

  it should "allow vars in imported lists" in {

    val rules =
      """
        |vars: org/clulab/odin/variables/test_import_lists.yml
        |
        |rules:
        |  - name: "turtle-power-var"
        |    label: MutantTurtle
        |    type: token
        |    pattern: |
        |      [word=/(?i)^${turtle}/]
      """.stripMargin

    //    Leonardo leads, Donatello does machines (That's a fact, jack!)
    //    Raphael is cool but (c)rude (Gimme a break!)
    //    Michelangelo is a party dude (Party!)
    val doc1 = jsonStringToDocument(""" {"sentences":[{"raw":["Leonardo","leads",",","Donatello","does","machines","-LRB-","That","'s","a","fact",",","jack","!","-RRB-"],"words":["Leonardo","leads",",","Donatello","does","machines","-LRB-","That","'s","a","fact",",","jack","!","-RRB-"],"startOffsets":[1,10,15,17,27,32,41,42,46,49,51,55,57,61,62],"endOffsets":[9,15,16,26,31,40,42,46,48,50,55,56,61,62,63],"tags":["NNP","VBZ",",","NNP","VBZ","NNS","-LRB-","DT","VBZ","DT","NN",",","NN",".","-RRB-"],"lemmas":["Leonardo","lead",",","Donatello","do","machine","-lrb-","that","be","a","fact",",","jack","!","-rrb-"],"entities":["PERSON","O","O","PERSON","O","O","O","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","O","B-NP","B-VP","B-NP","O","B-NP","B-VP","B-NP","I-NP","O","B-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"punct"},{"source":1,"destination":5,"relation":"conj"},{"source":5,"destination":3,"relation":"nn"},{"source":5,"destination":4,"relation":"aux"},{"source":5,"destination":10,"relation":"dep"},{"source":10,"destination":6,"relation":"punct"},{"source":10,"destination":7,"relation":"nsubj"},{"source":10,"destination":8,"relation":"cop"},{"source":10,"destination":9,"relation":"det"},{"source":10,"destination":11,"relation":"punct"},{"source":10,"destination":12,"relation":"appos"},{"source":10,"destination":14,"relation":"punct"},{"source":12,"destination":13,"relation":"punct"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"punct"},{"source":1,"destination":5,"relation":"conj"},{"source":5,"destination":3,"relation":"nn"},{"source":5,"destination":4,"relation":"aux"},{"source":5,"destination":10,"relation":"dep"},{"source":10,"destination":6,"relation":"punct"},{"source":10,"destination":7,"relation":"nsubj"},{"source":10,"destination":8,"relation":"cop"},{"source":10,"destination":9,"relation":"det"},{"source":10,"destination":11,"relation":"punct"},{"source":10,"destination":12,"relation":"appos"},{"source":10,"destination":14,"relation":"punct"},{"source":12,"destination":13,"relation":"punct"}],"roots":[1]}}},{"raw":["Raphael","is","cool","but","-LRB-","c","-RRB-","rude","-LRB-","Gim","me","a","break","!","-RRB-"], "words":["Raphael","is","cool","but","-LRB-","c","-RRB-","rude","-LRB-","Gim","me","a","break","!","-RRB-"],"startOffsets":[64,72,75,80,84,85,86,87,92,93,96,99,101,106,107],"endOffsets":[71,74,79,83,85,86,87,91,93,96,98,100,106,107,108],"tags":["NNP","VBZ","JJ","CC","-LRB-","LS","-RRB-","JJ","-LRB-","NNP","PRP","DT","NN",".","-RRB-"],"lemmas":["Raphael","be","cool","but","-lrb-","c","-rrb-","rude","-lrb-","Gim","I","a","break","!","-rrb-"],"entities":["PERSON","O","O","O","O","O","O","O","O","PERSON","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-ADJP","O","O","O","O","O","O","O","B-NP","B-NP","I-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":2,"destination":3,"relation":"cc"},{"source":2,"destination":7,"relation":"conj"},{"source":2,"destination":14,"relation":"punct"},{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"cop"},{"source":5,"destination":4,"relation":"punct"},{"source":5,"destination":6,"relation":"punct"},{"source":7,"destination":5,"relation":"dep"},{"source":7,"destination":9,"relation":"dep"},{"source":9,"destination":8,"relation":"punct"},{"source":9,"destination":12,"relation":"dep"},{"source":9,"destination":13,"relation":"punct"},{"source":12,"destination":10,"relation":"dep"},{"source":12,"destination":11,"relation":"det"}],"roots":[2]},"stanford-collapsed":{"edges":[{"source":2,"destination":7,"relation":"conj_but"},{"source":2,"destination":14,"relation":"punct"},{"source":2,"destination":0,"relation":"nsubj"},{"source":2,"destination":1,"relation":"cop"},{"source":5,"destination":4,"relation":"punct"},{"source":5,"destination":6,"relation":"punct"},{"source":7,"destination":5,"relation":"dep"},{"source":7,"destination":9,"relation":"dep"},{"source":7,"destination":0,"relation":"nsubj"},{"source":9,"destination":8,"relation":"punct"},{"source":9,"destination":12,"relation":"dep"},{"source":9,"destination":13,"relation":"punct"},{"source":12,"destination":10,"relation":"dep"},{"source":12,"destination":11,"relation":"det"}],"roots":[2]}}},{"raw":["Michelangelo","is","a","party","dude","-LRB-","Party","!","-RRB-"],"words":["Michelangelo","is","a","party","dude","-LRB-","Party","!","-RRB-"],"startOffsets":[109,122,125,127,133,138,139,144,145],"endOffsets":[121,124,126,132,137,139,144,145,146],"tags":["NNP","VBZ","DT","NN","NN","-LRB-","NNP",".","-RRB-"],"lemmas":["Michelangelo","be","a","party","dude","-lrb-","Party","!","-rrb-"],"entities":["PERSON","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","I-NP","I-NP","O","B-NP","O","O"],"graphs":{"stanford-basic":{"edges":[{"source":6,"destination":5,"relation":"punct"},{"source":6,"destination":7,"relation":"punct"},{"source":6,"destination":8,"relation":"punct"},{"source":4,"destination":6,"relation":"dep"},{"source":4,"destination":0,"relation":"nsubj"},{"source":4,"destination":1,"relation":"cop"},{"source":4,"destination":2,"relation":"det"},{"source":4,"destination":3,"relation":"nn"}],"roots":[4]},"stanford-collapsed":{"edges":[{"source":6,"destination":5,"relation":"punct"},{"source":6,"destination":7,"relation":"punct"},{"source":6,"destination":8,"relation":"punct"},{"source":4,"destination":6,"relation":"dep"},{"source":4,"destination":0,"relation":"nsubj"},{"source":4,"destination":1,"relation":"cop"},{"source":4,"destination":2,"relation":"det"},{"source":4,"destination":3,"relation":"nn"}],"roots":[4]}}}]} """)
    val ee = ExtractorEngine(rules)
    val res1 = ee.extractFrom(doc1)

    res1 should have size (4)

    res1.exists(m => m.text == "Leonardo") should be (true)
    res1.exists(m => m.text == "Donatello") should be (true)
    res1.exists(m => m.text == "Raphael") should be (true)
    res1.exists(m => m.text == "Michelangelo") should be (true)
  }

}
