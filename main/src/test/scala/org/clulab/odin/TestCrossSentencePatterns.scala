package org.clulab.odin

import org.scalatest._
import org.clulab.TestUtils.jsonStringToDocument
import org.clulab.struct.Interval


class TestCrossSentencePatterns extends FlatSpec with Matchers {

  val text1 = "Barack Obama is the 44th President of the United States. He was born on August 4, 1961 in Honolulu, Hawaii."

  text1 should """produce a RelationMention with the label "Coref"""" in {

    val doc = jsonStringToDocument(""" {"sentences":[{"words":["Barack","Obama","is","the","44th","President","of","the","United","States","."],"startOffsets":[0,7,13,16,20,25,35,38,42,49,55],"endOffsets":[6,12,15,19,24,34,37,41,48,55,56],"tags":["NNP","NNP","VBZ","DT","JJ","NN","IN","DT","NNP","NNPS","."],"lemmas":["Barack","Obama","be","the","44th","president","of","the","United","States","."],"entities":["PERSON","PERSON","O","O","ORDINAL","O","O","O","LOCATION","LOCATION","O"],"norms":["O","O","O","O","44.0","O","O","O","O","O","O"],"chunks":["B-NP","I-NP","B-VP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","O"],"graphs":{}},{"words":["He","was","born","on","August","4",",","1961","in","Honolulu",",","Hawaii","."],"startOffsets":[57,60,64,69,72,79,80,82,87,90,98,100,106],"endOffsets":[59,63,68,71,78,80,81,86,89,98,99,106,107],"tags":["PRP","VBD","VBN","IN","NNP","CD",",","CD","IN","NNP",",","NNP","."],"lemmas":["he","be","bear","on","August","4",",","1961","in","Honolulu",",","Hawaii","."],"entities":["O","O","O","O","DATE","DATE","DATE","DATE","O","LOCATION","O","LOCATION","O"],"norms":["O","O","O","O","1961-08-04","1961-08-04","1961-08-04","1961-08-04","O","O","O","O","O"],"chunks":["B-NP","B-VP","I-VP","B-PP","B-NP","I-NP","O","B-NP","B-PP","B-NP","O","B-NP","O"],"graphs":{}}]} """)

    // entities
    val entity1 = new TextBoundMention("Entity", Interval(0, 2), 0, doc, false, "ne-rule")
    val entity2 = new TextBoundMention("Entity", Interval(0), 1, doc, false, "pronominal-rule")

    val state = State(Seq(entity1, entity2))

    // try with rule1
    val rule1 =
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
    val ee1 = ExtractorEngine(rule1)
    val mentions1 = ee1.extractFrom(doc, state)
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
    val ee2 = ExtractorEngine(rule2)
    val mentions2 = ee2.extractFrom(doc, state)
    mentions2 should have size (1)
    (mentions2.head matches "Coref") should be (true)

  }

  "Anchor and neighbor" should "point to existing mentions with the given label in the state" in {

    val text = "Akt interacts with Raf and phosphorylates it at Ser259 .  Furthermore , phosphorylation of Raf by Akt inhibits activation of the Raf-MEK-ERK signaling pathway and has been shown to alter the cellular response in a human breast cancer cell line from cell cycle arrest to proliferation ."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["Akt","interacts","with","Raf","and","phosphorylates","it","at","Ser259","."],"startOffsets":[0,4,14,19,23,27,42,45,48,55],"endOffsets":[3,13,18,22,26,41,44,47,54,56],"tags":["NN","VBZ","IN","NN","CC","VBZ","PRP","IN","NN","."],"lemmas":["akt","interact","with","raf","and","phosphorylate","it","at","ser259","."],"entities":["O","O","O","PERSON","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","B-VP","B-PP","B-NP","O","B-VP","B-NP","B-PP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":2,"relation":"prep"},{"source":1,"destination":4,"relation":"cc"},{"source":1,"destination":5,"relation":"conj"},{"source":1,"destination":9,"relation":"punct"},{"source":2,"destination":3,"relation":"pobj"},{"source":5,"destination":6,"relation":"dobj"},{"source":5,"destination":7,"relation":"prep"},{"source":7,"destination":8,"relation":"pobj"}],"roots":[1]},"stanford-collapsed":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":3,"relation":"prep_with"},{"source":1,"destination":5,"relation":"conj_and"},{"source":1,"destination":9,"relation":"punct"},{"source":5,"destination":0,"relation":"nsubj"},{"source":5,"destination":6,"relation":"dobj"},{"source":5,"destination":8,"relation":"prep_at"}],"roots":[1]}}},{"words":["Furthermore",",","phosphorylation","of","Raf","by","Akt","inhibits","activation","of","the","Raf-MEK-ERK","signaling","pathway","and","has","been","shown","to","alter","the","cellular","response","in","a","human","breast","cancer","cell","line","from","cell","cycle","arrest","to","proliferation","."],"startOffsets":[58,70,72,88,91,95,98,102,111,122,125,129,141,151,159,163,167,172,178,181,187,191,200,209,212,214,220,227,234,239,244,249,254,260,267,270,284],"endOffsets":[69,71,87,90,94,97,101,110,121,124,128,140,150,158,162,166,171,177,180,186,190,199,208,211,213,219,226,233,238,243,248,253,259,266,269,283,285],"tags":["RB",",","NN","IN","NN","IN","NN","VBZ","NN","IN","DT","NN","NN","NN","CC","VBZ","VBN","VBN","TO","VB","DT","JJ","NN","IN","DT","JJ","NN","NN","NN","NN","IN","NN","NN","NN","TO","NN","."],"lemmas":["furthermore",",","phosphorylation","of","raf","by","akt","inhibit","activation","of","the","raf-mek-erk","signaling","pathway","and","have","be","show","to","alter","the","cellular","response","in","a","human","breast","cancer","cell","line","from","cell","cycle","arrest","to","proliferation","."],"entities":["O","O","O","O","O","O","PERSON","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-ADVP","O","B-NP","B-PP","B-NP","B-PP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","I-NP","O","B-VP","I-VP","I-VP","B-VP","I-VP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","B-PP","B-NP","O"],"graphs":{"stanford-basic":{"edges":[{"source":34,"destination":35,"relation":"pobj"},{"source":2,"destination":3,"relation":"prep"},{"source":2,"destination":5,"relation":"prep"},{"source":3,"destination":4,"relation":"pobj"},{"source":5,"destination":6,"relation":"pobj"},{"source":7,"destination":2,"relation":"nsubj"},{"source":7,"destination":36,"relation":"punct"},{"source":7,"destination":8,"relation":"dobj"},{"source":7,"destination":14,"relation":"cc"},{"source":7,"destination":0,"relation":"advmod"},{"source":7,"destination":1,"relation":"punct"},{"source":7,"destination":17,"relation":"conj"},{"source":8,"destination":9,"relation":"prep"},{"source":9,"destination":13,"relation":"pobj"},{"source":13,"destination":10,"relation":"det"},{"source":13,"destination":11,"relation":"nn"},{"source":13,"destination":12,"relation":"nn"},{"source":17,"destination":19,"relation":"xcomp"},{"source":17,"destination":15,"relation":"aux"},{"source":17,"destination":16,"relation":"auxpass"},{"source":19,"destination":18,"relation":"aux"},{"source":19,"destination":22,"relation":"dobj"},{"source":22,"destination":34,"relation":"prep"},{"source":22,"destination":20,"relation":"det"},{"source":22,"destination":21,"relation":"amod"},{"source":22,"destination":23,"relation":"prep"},{"source":23,"destination":29,"relation":"pobj"},{"source":29,"destination":24,"relation":"det"},{"source":29,"destination":25,"relation":"amod"},{"source":29,"destination":26,"relation":"nn"},{"source":29,"destination":27,"relation":"nn"},{"source":29,"destination":28,"relation":"nn"},{"source":29,"destination":30,"relation":"prep"},{"source":30,"destination":33,"relation":"pobj"},{"source":33,"destination":31,"relation":"nn"},{"source":33,"destination":32,"relation":"nn"}],"roots":[7]},"stanford-collapsed":{"edges":[{"source":2,"destination":4,"relation":"prep_of"},{"source":2,"destination":6,"relation":"prep_by"},{"source":7,"destination":2,"relation":"nsubj"},{"source":7,"destination":36,"relation":"punct"},{"source":7,"destination":8,"relation":"dobj"},{"source":7,"destination":0,"relation":"advmod"},{"source":7,"destination":1,"relation":"punct"},{"source":7,"destination":17,"relation":"conj_and"},{"source":8,"destination":13,"relation":"prep_of"},{"source":13,"destination":10,"relation":"det"},{"source":13,"destination":11,"relation":"nn"},{"source":13,"destination":12,"relation":"nn"},{"source":17,"destination":2,"relation":"nsubjpass"},{"source":17,"destination":19,"relation":"xcomp"},{"source":17,"destination":15,"relation":"aux"},{"source":17,"destination":16,"relation":"auxpass"},{"source":19,"destination":18,"relation":"aux"},{"source":19,"destination":22,"relation":"dobj"},{"source":22,"destination":35,"relation":"prep_to"},{"source":22,"destination":20,"relation":"det"},{"source":22,"destination":21,"relation":"amod"},{"source":22,"destination":29,"relation":"prep_in"},{"source":29,"destination":24,"relation":"det"},{"source":29,"destination":25,"relation":"amod"},{"source":29,"destination":26,"relation":"nn"},{"source":29,"destination":27,"relation":"nn"},{"source":29,"destination":28,"relation":"nn"},{"source":29,"destination":33,"relation":"prep_from"},{"source":33,"destination":31,"relation":"nn"},{"source":33,"destination":32,"relation":"nn"}],"roots":[7]}}}]} """)

    // mentions
    val protein1 = new TextBoundMention(Seq("Protein", "Entity"), Interval(0), 0, doc, false, "<MANUAL>")
    val protein2 = new TextBoundMention(Seq("Protein", "Entity"), Interval(3), 0, doc, false, "<MANUAL>")
    val protein3 = new TextBoundMention(Seq("Protein", "Entity"), Interval(4), 1, doc, false, "<MANUAL>")
    val protein4 = new TextBoundMention(Seq("Protein", "Entity"), Interval(6), 1, doc, false, "<MANUAL>")

    val bindingTrigger = new TextBoundMention("BindingTrigger", Interval(1), 0, doc, false, "<MANUAL>")
    val binding = new EventMention(
      labels = Seq("Binding", "SimpleEvent", "Event", "PossibleController"),
      trigger =  bindingTrigger,
      arguments = Map("theme" -> Seq(protein1, protein2)),
      sentence = 0,
      document = doc,
      keep = false,
      foundBy = "binding-rule"
    )
    val phosphoTrigger = new TextBoundMention("PhosphoTrigger", Interval(2), 1, doc, false, "<MANUAL>")
    val phosphorylation = new EventMention(
      labels = Seq("Phosphorylation", "SimpleEvent", "Event", "PossibleController"),
      trigger =  phosphoTrigger,
      arguments = Map("theme" -> Seq(protein3)),
      sentence = 1,
      document = doc,
      keep = false,
      foundBy = "phospho-rule"
    )
    val posreg = new RelationMention(
      labels = Seq("Positive_regulation", "Regulation", "ComplexEvent", "Event", "PossibleController"),
      arguments = Map("controller" -> Seq(protein4), "controlled" -> Seq(phosphorylation)),
      sentence = 1,
      document = doc,
      keep = false,
      foundBy = "reg-rule"
    )

    val state = State(Seq(protein1, protein2, protein3, protein4, binding, phosphorylation, posreg))

    val rules =
      """
        |taxonomy:
        |  - PossibleController:
        |      - Event:
        |          - SimpleEvent:
        |              - Binding
        |              - Phosphorylation
        |          - ComplexEvent:
        |              - Regulation:
        |                  - Positive_regulation
        |                  - Negative_regulation
        |              - ActivationEvent:
        |                  - Positive_activation
        |                  - Negative_activation
        |      - Entity:
        |        - Protein
        |      - Precedence
        |
        |rules:
        |  - name: cross-sentence-furthermore
        |    label: Precedence
        |    priority: 2
        |    type: "cross-sentence"
        |    right-window: 1
        |    example: "Akt interacts with Raf and phosphorylates it at Ser259 .  Furthermore , phosphorylation of Raf by Akt inhibits activation of the Raf-MEK-ERK signaling pathway and has been shown to alter the cellular response in a human breast cancer cell line from cell cycle arrest to proliferation ."
        |    pattern: |
        |      before:Event = @Event
        |      after:ComplexEvent = (?<= [lemma=furthermore] ",") @ComplexEvent
      """.stripMargin

    val ee = ExtractorEngine(rules)
    val mentions = ee.extractFrom(doc, state)
    mentions should have size (1)
    val Seq(prec) = mentions
    prec.label should equal("Precedence")
    prec.arguments.keys should have size(2)
    prec.arguments.keys should contain("after")
    prec.arguments("after") should have size(1)
    val Seq(after) = prec.arguments("after")
    after matches "Positive_regulation" should be(true)
  }

  it should "not match in the same sentence" in {

    val text = "Furthermore , phosphorylation of Raf by Akt inhibits activation of the Raf-MEK-ERK signaling pathway and has been shown to alter the cellular response in a human breast cancer cell line from cell cycle arrest to proliferation ."
    val doc = jsonStringToDocument(""" {"sentences":[{"words":["Furthermore",",","phosphorylation","of","Raf","by","Akt","inhibits","activation","of","the","Raf-MEK-ERK","signaling","pathway","and","has","been","shown","to","alter","the","cellular","response","in","a","human","breast","cancer","cell","line","from","cell","cycle","arrest","to","proliferation","."],"startOffsets":[0,12,14,30,33,37,40,44,53,64,67,71,83,93,101,105,109,114,120,123,129,133,142,151,154,156,162,169,176,181,186,191,196,202,209,212,226],"endOffsets":[11,13,29,32,36,39,43,52,63,66,70,82,92,100,104,108,113,119,122,128,132,141,150,153,155,161,168,175,180,185,190,195,201,208,211,225,227],"tags":["RB",",","NN","IN","NN","IN","NN","VBZ","NN","IN","DT","NN","NN","NN","CC","VBZ","VBN","VBN","TO","VB","DT","JJ","NN","IN","DT","JJ","NN","NN","NN","NN","IN","NN","NN","NN","TO","NN","."],"lemmas":["furthermore",",","phosphorylation","of","raf","by","akt","inhibit","activation","of","the","raf-mek-erk","signaling","pathway","and","have","be","show","to","alter","the","cellular","response","in","a","human","breast","cancer","cell","line","from","cell","cycle","arrest","to","proliferation","."],"entities":["O","O","O","O","O","O","PERSON","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-ADVP","O","B-NP","B-PP","B-NP","B-PP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","I-NP","O","B-VP","I-VP","I-VP","B-VP","I-VP","B-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","I-NP","I-NP","I-NP","B-PP","B-NP","I-NP","I-NP","B-PP","B-NP","O"],"graphs":{"stanford-collapsed":{"edges":[{"source":2,"destination":4,"relation":"prep_of"},{"source":2,"destination":6,"relation":"prep_by"},{"source":7,"destination":0,"relation":"advmod"},{"source":7,"destination":1,"relation":"punct"},{"source":7,"destination":17,"relation":"conj_and"},{"source":7,"destination":2,"relation":"nsubj"},{"source":7,"destination":36,"relation":"punct"},{"source":7,"destination":8,"relation":"dobj"},{"source":8,"destination":13,"relation":"prep_of"},{"source":13,"destination":10,"relation":"det"},{"source":13,"destination":11,"relation":"nn"},{"source":13,"destination":12,"relation":"nn"},{"source":17,"destination":15,"relation":"aux"},{"source":17,"destination":16,"relation":"auxpass"},{"source":17,"destination":2,"relation":"nsubjpass"},{"source":17,"destination":19,"relation":"xcomp"},{"source":19,"destination":18,"relation":"aux"},{"source":19,"destination":22,"relation":"dobj"},{"source":22,"destination":35,"relation":"prep_to"},{"source":22,"destination":20,"relation":"det"},{"source":22,"destination":21,"relation":"amod"},{"source":22,"destination":29,"relation":"prep_in"},{"source":29,"destination":33,"relation":"prep_from"},{"source":29,"destination":24,"relation":"det"},{"source":29,"destination":25,"relation":"amod"},{"source":29,"destination":26,"relation":"nn"},{"source":29,"destination":27,"relation":"nn"},{"source":29,"destination":28,"relation":"nn"},{"source":33,"destination":31,"relation":"nn"},{"source":33,"destination":32,"relation":"nn"}],"roots":[7]}}}]} """)

    // mentions
    val protein1 = new TextBoundMention(Seq("Protein", "Entity"), Interval(4), 0, doc, false, "<MANUAL>")
    val protein2 = new TextBoundMention(Seq("Protein", "Entity"), Interval(6), 0, doc, false, "<MANUAL>")

    val phosphoTrigger = new TextBoundMention("PhosphoTrigger", Interval(2), 0, doc, false, "<MANUAL>")
    val phosphorylation = new EventMention(
      labels = Seq("Phosphorylation", "SimpleEvent", "Event", "PossibleController"),
      trigger =  phosphoTrigger,
      arguments = Map("theme" -> Seq(protein1)),
      sentence = 0,
      document = doc,
      keep = false,
      foundBy = "phospho-rule"
    )
    val posreg = new RelationMention(
      labels = Seq("Positive_regulation", "Regulation", "ComplexEvent", "Event", "PossibleController"),
      arguments = Map("controller" -> Seq(protein2), "controlled" -> Seq(phosphorylation)),
      sentence = 0,
      document = doc,
      keep = false,
      foundBy = "reg-rule"
    )

    val state = State(Seq(protein1, protein2, phosphorylation, posreg))

    val rules =
      """
        |taxonomy:
        |  - PossibleController:
        |      - Event:
        |          - SimpleEvent:
        |              - Binding
        |              - Phosphorylation
        |          - ComplexEvent:
        |              - Regulation:
        |                  - Positive_regulation
        |                  - Negative_regulation
        |              - ActivationEvent:
        |                  - Positive_activation
        |                  - Negative_activation
        |      - Entity:
        |        - Protein
        |      - Precedence
        |
        |rules:
        |  - name: cross-sentence-furthermore
        |    label: Precedence
        |    priority: 2
        |    type: "cross-sentence"
        |    right-window: 1
        |    example: "Furthermore , phosphorylation of Raf by Akt inhibits activation of the Raf-MEK-ERK signaling pathway and has been shown to alter the cellular response in a human breast cancer cell line from cell cycle arrest to proliferation ."
        |    pattern: |
        |      before:Event = @Event
        |      after:ComplexEvent = (?<= [lemma=furthermore] ",") @ComplexEvent
      """.stripMargin

    val ee = ExtractorEngine(rules)
    val mentions = ee.extractFrom(doc, state).filter(_ matches "Precedence")
    mentions should have size (0)
  }
}
