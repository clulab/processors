package org.clulab.odin

import org.clulab.TestUtils.jsonStringToDocument
import org.clulab.struct.Interval
import org.scalatest._

class TestGraphPattern extends FlatSpec with Matchers {

  "GraphPattern" should "support multiline patterns" in {

    val text = "I saw Kermit at the pond."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["I","saw","Kermit","at","the","pond","."],"startOffsets":[0,2,6,13,16,20,24],"endOffsets":[1,5,12,15,19,24,25],"tags":["PRP","VBD","NNP","IN","DT","NN","."],"lemmas":["I","see","Kermit","at","the","pond","."],"entities":["O","O","PERSON","O","O","O","O"],"norms":["O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-NP","B-PP","B-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":3,"relation":"prep"},{"source":1,"destination":6,"relation":"punct"},{"source":3,"destination":5,"relation":"pobj"},{"source":5,"destination":4,"relation":"det"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"dobj"},{"source":1,"destination":5,"relation":"prep_at"},{"source":1,"destination":6,"relation":"punct"},{"source":5,"destination":4,"relation":"det"}],"roots":[1]}}}]} """)

    val rule = """
      |- name: testRule
      |  label: Sight
      |  pattern: |
      |    trigger = saw
      |    participants:Entity+ = # both participants (+)
      |      nsubj                # the nominal subject
      |        |                  # and
      |      dobj                 # the direct object
      |    location:Place = prep_at
      |""".stripMargin

    val mentions = Seq(
      new TextBoundMention("Entity", Interval(0), 0, doc, false, "<test>"),
      new TextBoundMention("Entity", Interval(2), 0, doc, false, "<test>"),
      new TextBoundMention("Place", Interval(5), 0, doc, false, "<test>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc, state)
    results should have size (1)
    results.head.arguments should contain key ("participants")
    results.head.arguments should contain key ("location")

  }

  it should "support quantified arguments" in {

    val text = "The binding of Ras, TGFBR1, MEK and TGFBR2."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["The","binding","of","Ras",",","TGFBR1",",","MEK","and","TGFBR2","."],"startOffsets":[0,4,12,15,18,20,26,28,32,36,42],"endOffsets":[3,11,14,18,19,26,27,31,35,42,43],"tags":["DT","NN","IN","NN",",","NN",",","NN","CC","NN","."],"lemmas":["the","binding","of","ra",",","tgfbr1",",","mek","and","tgfbr2","."],"entities":["O","O","O","PERSON","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","I-NP","B-PP","B-NP","O","B-NP","I-NP","I-NP","I-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"det"},{"source":1,"destination":2,"relation":"prep"},{"source":1,"destination":10,"relation":"punct"},{"source":2,"destination":3,"relation":"pobj"},{"source":3,"destination":4,"relation":"punct"},{"source":3,"destination":5,"relation":"conj"},{"source":3,"destination":6,"relation":"punct"},{"source":3,"destination":7,"relation":"conj"},{"source":3,"destination":8,"relation":"cc"},{"source":3,"destination":9,"relation":"conj"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"det"},{"source":1,"destination":3,"relation":"prep_of"},{"source":1,"destination":5,"relation":"prep_of"},{"source":1,"destination":7,"relation":"prep_of"},{"source":1,"destination":9,"relation":"prep_of"},{"source":1,"destination":10,"relation":"punct"},{"source":3,"destination":4,"relation":"punct"},{"source":3,"destination":5,"relation":"conj_and"},{"source":3,"destination":6,"relation":"punct"},{"source":3,"destination":7,"relation":"conj_and"},{"source":3,"destination":9,"relation":"conj_and"}],"roots":[1]}}}]} """)

    val rule = """
      |- name: testRule
      |  label: Binding
      |  pattern: |
      |    trigger = binding
      |    theme:Protein{2} = prep_of conj?
      |""".stripMargin

    val mentions = Seq(
      new TextBoundMention("Protein", Interval(3), 0, doc, false, "<test>"),
      new TextBoundMention("Protein", Interval(5), 0, doc, false, "<test>"),
      new TextBoundMention("Protein", Interval(7), 0, doc, false, "<test>"),
      new TextBoundMention("Protein", Interval(9), 0, doc, false, "<test>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc, state)

    results should have size (6)

  }

  it should "handle multitoken triggers" in {
    val text = "We found that prolonged expression of active Ras resulted in upregulation of the MKP3 gene."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["We","found","that","prolonged","expression","of","active","Ras","resulted","in","upregulation","of","the","MKP3","gene","."],"startOffsets":[0,3,9,14,24,35,38,45,49,58,61,74,77,81,86,90],"endOffsets":[2,8,13,23,34,37,44,48,57,60,73,76,80,85,90,91],"tags":["PRP","VBD","IN","JJ","NN","IN","JJ","NN","VBD","IN","NN","IN","DT","NN","NN","."],"lemmas":["we","find","that","prolonged","expression","of","active","ra","result","in","upregulation","of","the","mkp3","gene","."],"entities":["O","O","O","O","O","O","O","PERSON","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-SBAR","B-NP","I-NP","B-PP","B-NP","I-NP","B-VP","B-PP","B-NP","B-PP","B-NP","I-NP","I-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":15,"relation":"punct"},{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":8,"relation":"ccomp"},{"source":4,"destination":3,"relation":"amod"},{"source":4,"destination":5,"relation":"prep"},{"source":5,"destination":7,"relation":"pobj"},{"source":7,"destination":6,"relation":"amod"},{"source":8,"destination":2,"relation":"mark"},{"source":8,"destination":4,"relation":"nsubj"},{"source":8,"destination":9,"relation":"prep"},{"source":9,"destination":10,"relation":"pobj"},{"source":10,"destination":11,"relation":"prep"},{"source":11,"destination":14,"relation":"pobj"},{"source":14,"destination":12,"relation":"det"},{"source":14,"destination":13,"relation":"nn"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":15,"relation":"punct"},{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":8,"relation":"ccomp"},{"source":4,"destination":3,"relation":"amod"},{"source":4,"destination":7,"relation":"prep_of"},{"source":7,"destination":6,"relation":"amod"},{"source":8,"destination":2,"relation":"mark"},{"source":8,"destination":4,"relation":"nsubj"},{"source":8,"destination":10,"relation":"prep_in"},{"source":10,"destination":14,"relation":"prep_of"},{"source":14,"destination":12,"relation":"det"},{"source":14,"destination":13,"relation":"nn"}],"roots":[1]}}}]} """)

    val rule = """
      |- name: Positive_activation_syntax_8_verb
      |  priority: 2
      |  label: Positive_activation
      |  pattern: |
      |    trigger = [lemma=result] in [word=/(?i)^(upregul)/]
      |    controlled:Protein = prep_of nn
      |    controller:Protein = nsubj prep_of
      |""".stripMargin

    val mentions = Seq(
      new TextBoundMention("Protein", Interval(7), 0, doc, false, "<test>"),
      new TextBoundMention("Protein", Interval(13), 0, doc, false, "<test>")
    )

    val state = State(mentions)
    val ee = ExtractorEngine(rule)
    val results = ee.extractFrom(doc, state)

    results.filter(_ matches "Positive_activation") should have size (1)

  }

  "GraphPattern" should "support 'type: graph'" in {

    // "In 1987, Phillip Jeffries disappeared in a Buenos Aires hotel."
    val doc = jsonStringToDocument(""" { "sentences": [ { "endOffsets": [ 2, 7, 8, 16, 25, 37, 40, 42, 49, 55, 61, 62 ], "entities": [ "O", "DATE", "O", "PERSON", "PERSON", "O", "O", "O", "LOCATION", "LOCATION", "O", "O" ], "graphs": { "stanford-basic": { "edges": [ { "destination": 1, "relation": "pobj", "source": 0 }, { "destination": 3, "relation": "nn", "source": 4 }, { "destination": 0, "relation": "prep", "source": 5 }, { "destination": 2, "relation": "punct", "source": 5 }, { "destination": 4, "relation": "nsubj", "source": 5 }, { "destination": 6, "relation": "prep", "source": 5 }, { "destination": 11, "relation": "punct", "source": 5 }, { "destination": 10, "relation": "pobj", "source": 6 }, { "destination": 7, "relation": "det", "source": 10 }, { "destination": 8, "relation": "nn", "source": 10 }, { "destination": 9, "relation": "nn", "source": 10 } ], "roots": [ 5 ] }, "stanford-collapsed": { "edges": [ { "destination": 3, "relation": "nn", "source": 4 }, { "destination": 1, "relation": "prep_in", "source": 5 }, { "destination": 2, "relation": "punct", "source": 5 }, { "destination": 4, "relation": "nsubj", "source": 5 }, { "destination": 10, "relation": "prep_in", "source": 5 }, { "destination": 11, "relation": "punct", "source": 5 }, { "destination": 7, "relation": "det", "source": 10 }, { "destination": 8, "relation": "nn", "source": 10 }, { "destination": 9, "relation": "nn", "source": 10 } ], "roots": [ 5 ] } }, "lemmas": [ "in", "1987", ",", "Phillip", "Jeffries", "disappear", "in", "a", "Buenos", "Aires", "hotel", "." ], "startOffsets": [ 0, 3, 7, 9, 17, 26, 38, 41, 43, 50, 56, 61 ], "tags": [ "IN", "CD", ",", "NNP", "NNP", "VBD", "IN", "DT", "NNP", "NNP", "NN", "." ], "words": [ "In", "1987", ",", "Phillip", "Jeffries", "disappeared", "in", "a", "Buenos", "Aires", "hotel", "." ] } ], "text": "In 1987, Phillip Jeffries disappeared in a Buenos Aires hotel." } """)

    val mentions = Seq(
      new TextBoundMention("Person", Interval(3, 5), 0, doc, false, "<test>"),
      new TextBoundMention("Location", Interval(8, 10), 0, doc, false, "<test>"),
      new TextBoundMention("Date", Interval(1), 0, doc, false, "<test>")
    )

    val state = State(mentions)

    val rule1 = """
                  |- name: "date-graph-default-1"
                  |  label: Event
                  |  type: graph
                  |  pattern: |
                  |    trigger = [lemma="disappear"]
                  |    person: Person = nsubj
                  |    location: Location = prep_in nn?
                  |
                  |""".stripMargin


    val ee1 = ExtractorEngine(rule1)
    val results1 = ee1.extractFrom(doc, state)
    results1 should have size (1)
  }

  "GraphPattern" should "support 'graph: ' key" in {

    // "In 1987, Phillip Jeffries disappeared in a Buenos Aires hotel."
    val doc = jsonStringToDocument(""" { "sentences": [ { "endOffsets": [ 2, 7, 8, 16, 25, 37, 40, 42, 49, 55, 61, 62 ], "entities": [ "O", "DATE", "O", "PERSON", "PERSON", "O", "O", "O", "LOCATION", "LOCATION", "O", "O" ], "graphs": { "stanford-basic": { "edges": [ { "destination": 1, "relation": "pobj", "source": 0 }, { "destination": 3, "relation": "nn", "source": 4 }, { "destination": 0, "relation": "prep", "source": 5 }, { "destination": 2, "relation": "punct", "source": 5 }, { "destination": 4, "relation": "nsubj", "source": 5 }, { "destination": 6, "relation": "prep", "source": 5 }, { "destination": 11, "relation": "punct", "source": 5 }, { "destination": 10, "relation": "pobj", "source": 6 }, { "destination": 7, "relation": "det", "source": 10 }, { "destination": 8, "relation": "nn", "source": 10 }, { "destination": 9, "relation": "nn", "source": 10 } ], "roots": [ 5 ] }, "stanford-collapsed": { "edges": [ { "destination": 3, "relation": "nn", "source": 4 }, { "destination": 1, "relation": "prep_in", "source": 5 }, { "destination": 2, "relation": "punct", "source": 5 }, { "destination": 4, "relation": "nsubj", "source": 5 }, { "destination": 10, "relation": "prep_in", "source": 5 }, { "destination": 11, "relation": "punct", "source": 5 }, { "destination": 7, "relation": "det", "source": 10 }, { "destination": 8, "relation": "nn", "source": 10 }, { "destination": 9, "relation": "nn", "source": 10 } ], "roots": [ 5 ] } }, "lemmas": [ "in", "1987", ",", "Phillip", "Jeffries", "disappear", "in", "a", "Buenos", "Aires", "hotel", "." ], "startOffsets": [ 0, 3, 7, 9, 17, 26, 38, 41, 43, 50, 56, 61 ], "tags": [ "IN", "CD", ",", "NNP", "NNP", "VBD", "IN", "DT", "NNP", "NNP", "NN", "." ], "words": [ "In", "1987", ",", "Phillip", "Jeffries", "disappeared", "in", "a", "Buenos", "Aires", "hotel", "." ] } ], "text": "In 1987, Phillip Jeffries disappeared in a Buenos Aires hotel." } """)

    val rule1 = """
                 |- name: "date-token-collapsed-1"
                 |  label: Date
                 |  type: token
                 |  pattern: |
                 |    [entity=DATE & incoming=prep_in]
                 |""".stripMargin

    val ee1 = ExtractorEngine(rule1)
    val results1 = ee1.extractFrom(doc)
    results1 should have size (1)

    val rule2 = """
                  |- name: "date-token-collapsed-2"
                  |  label: Date
                  |  graph: "stanford-collapsed"
                  |  type: token
                  |  pattern: |
                  |    [entity=DATE & incoming=prep_in]
                  |""".stripMargin

    val ee2 = ExtractorEngine(rule2)
    val results2 = ee2.extractFrom(doc)
    results2 should have size (1)

    val rule3 = """
                  |- name: "date-token-basic-1"
                  |  label: Date
                  |  graph: "stanford-basic"
                  |  type: token
                  |  pattern: |
                  |    [entity=DATE & incoming=pobj]
                  |""".stripMargin

    val ee3 = ExtractorEngine(rule3)
    val results3 = ee3.extractFrom(doc)
    results3 should have size (1)



    val mentions = Seq(
      new TextBoundMention("Person", Interval(3, 5), 0, doc, false, "<test>"),
      new TextBoundMention("Location", Interval(8, 10), 0, doc, false, "<test>"),
      new TextBoundMention("Date", Interval(1), 0, doc, false, "<test>")
    )

    val state = State(mentions)

    val rule4 = """
                  |- name: "date-graph-collapsed-1"
                  |  label: Event
                  |  graph: "stanford-collapsed"
                  |  pattern: |
                  |    trigger = [lemma="disappear"]
                  |    person: Person = nsubj
                  |    location: Location = prep_in nn?
                  |
                  |""".stripMargin


    val ee4 = ExtractorEngine(rule4)
    val results4 = ee4.extractFrom(doc, state)
    results4 should have size (1)

    val rule5 = """
                  |- name: "date-graph-collapsed-default-1"
                  |  label: Event
                  |  pattern: |
                  |    trigger = [lemma="disappear"]
                  |    person: Person = nsubj
                  |    location: Location = prep_in nn?
                  |
                  |""".stripMargin


    val ee5 = ExtractorEngine(rule5)
    val results5 = ee5.extractFrom(doc, state)
    results5 should have size (1)

    val rule6 = """
                  |- name: "date-graph-basic-1"
                  |  label: Event
                  |  graph: "stanford-basic"
                  |  pattern: |
                  |    trigger = [lemma="disappear"]
                  |    person: Person = nsubj
                  |    location: Location = prep [lemma="in"] pobj nn?
                  |    date: Date = prep [lemma="in"] pobj
                  |""".stripMargin

    val ee6 = ExtractorEngine(rule6)
    val results6 = ee6.extractFrom(doc, state)
    results6 should have size (1)
  }

  //'type: graph'
}
