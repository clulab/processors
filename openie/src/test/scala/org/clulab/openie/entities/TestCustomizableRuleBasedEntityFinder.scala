package org.clulab.openie.entities

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval
import org.clulab.openie.IETestUtils.jsonStringToDocument
import org.scalatest.{FlatSpec, Matchers}

class TestCustomizableRuleBasedEntityFinder extends FlatSpec with Matchers {

  behavior of "CustomizableRuleBasedEntityFinder"

  val entityFinder = CustomizableRuleBasedFinder.fromConfig(ConfigFactory.load("test"))

  it should "expand using specified dependencies" in {

    val doc = jsonStringToDocument(""" {"sentences":[{"words":["The","blue","fish","of","happiness","lived","in","the","ocean","below","Atlantis","."],"startOffsets":[0,4,9,14,17,27,33,36,40,46,52,60],"endOffsets":[3,8,13,16,26,32,35,39,45,51,60,61],"raw":["The","blue","fish","of","happiness","lived","in","the","ocean","below","Atlantis","."],"tags":["DT","JJ","NN","IN","NN","VBD","IN","DT","NN","IN","NNP","."],"lemmas":["the","blue","fish","of","happiness","live","in","the","ocean","below","Atlantis","."],"entities":["O","O","O","O","O","O","O","O","O","O","O","O"],"norms":["O","O","O","O","O","O","O","O","O","O","O","O"],"chunks":["B-NP","I-NP","I-NP","B-PP","B-NP","B-VP","B-PP","B-NP","I-NP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":4,"relation":"nmod_of"},{"source":8,"destination":10,"relation":"nmod_below"},{"source":5,"destination":2,"relation":"nsubj"},{"source":5,"destination":11,"relation":"punct"},{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"amod"},{"source":8,"destination":6,"relation":"case"},{"source":10,"destination":9,"relation":"case"},{"source":8,"destination":7,"relation":"det"},{"source":2,"destination":0,"relation":"det"},{"source":5,"destination":8,"relation":"nmod_in"}],"roots":[5]},"universal-basic":{"edges":[{"source":5,"destination":2,"relation":"nsubj"},{"source":5,"destination":11,"relation":"punct"},{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"amod"},{"source":5,"destination":8,"relation":"nmod"},{"source":8,"destination":6,"relation":"case"},{"source":8,"destination":10,"relation":"nmod"},{"source":8,"destination":7,"relation":"det"},{"source":10,"destination":9,"relation":"case"},{"source":2,"destination":0,"relation":"det"},{"source":2,"destination":4,"relation":"nmod"}],"roots":[5]}}}]} """)
    val entities = entityFinder.extractAndFilter(doc)
    entities should have size(3)
    entities.map(_.text) should contain only ("blue fish of happiness", "ocean", "Atlantis")
    // Since "^nmod" was in the validOutgoing, fish should expand to happiness along nmod_of
    // since "^nmod_below" was in the invalidOutgoing, however, ocean should not expand to Atlantis

  }

  it should "prune invalid base entities using NER" in {

    val doc = jsonStringToDocument(""" {"sentences":[{"words":["The","doctor","used","1983","to","trim","his","beard","."],"startOffsets":[0,4,11,16,21,24,29,33,38],"endOffsets":[3,10,15,20,23,28,32,38,39],"raw":["The","doctor","used","1983","to","trim","his","beard","."],"tags":["DT","NN","VBN","CD","TO","VB","PRP$","NN","."],"lemmas":["the","doctor","use","1983","to","trim","he","beard","."],"entities":["O","TITLE","O","DATE","O","O","O","O","O"],"norms":["O","O","O","1983","O","O","O","O","O"],"chunks":["B-NP","I-NP","B-VP","B-NP","B-VP","I-VP","B-NP","I-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":7,"destination":6,"relation":"nmod:poss"},{"source":2,"destination":5,"relation":"advcl_to"},{"source":5,"destination":7,"relation":"dobj"},{"source":2,"destination":3,"relation":"dobj"},{"source":1,"destination":0,"relation":"det"},{"source":2,"destination":1,"relation":"nsubj"},{"source":2,"destination":8,"relation":"punct"},{"source":5,"destination":4,"relation":"mark"}],"roots":[2]},"universal-basic":{"edges":[{"source":7,"destination":6,"relation":"nmod:poss"},{"source":5,"destination":7,"relation":"dobj"},{"source":2,"destination":5,"relation":"advcl"},{"source":2,"destination":3,"relation":"dobj"},{"source":1,"destination":0,"relation":"det"},{"source":2,"destination":1,"relation":"nsubj"},{"source":2,"destination":8,"relation":"punct"},{"source":5,"destination":4,"relation":"mark"}],"roots":[2]}}}]} """)
    val entities = entityFinder.extractAndFilter(doc)
    entities should have size(2)
    entities.exists(e => e.text == "doctor") should be (true)

    val docWithDoctorAsDate = jsonStringToDocument(""" {"sentences":[{"words":["The","doctor","used","1983","to","trim","his","beard","."],"startOffsets":[0,4,11,16,21,24,29,33,38],"endOffsets":[3,10,15,20,23,28,32,38,39],"raw":["The","doctor","used","1983","to","trim","his","beard","."],"tags":["DT","NN","VBN","CD","TO","VB","PRP$","NN","."],"lemmas":["the","doctor","use","1983","to","trim","he","beard","."],"entities":["O","DATE","O","DATE","O","O","O","O","O"],"norms":["O","O","O","1983","O","O","O","O","O"],"chunks":["B-NP","I-NP","B-VP","B-NP","B-VP","I-VP","B-NP","I-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":7,"destination":6,"relation":"nmod:poss"},{"source":2,"destination":5,"relation":"advcl_to"},{"source":5,"destination":7,"relation":"dobj"},{"source":2,"destination":3,"relation":"dobj"},{"source":1,"destination":0,"relation":"det"},{"source":2,"destination":1,"relation":"nsubj"},{"source":2,"destination":8,"relation":"punct"},{"source":5,"destination":4,"relation":"mark"}],"roots":[2]},"universal-basic":{"edges":[{"source":7,"destination":6,"relation":"nmod:poss"},{"source":5,"destination":7,"relation":"dobj"},{"source":2,"destination":5,"relation":"advcl"},{"source":2,"destination":3,"relation":"dobj"},{"source":1,"destination":0,"relation":"det"},{"source":2,"destination":1,"relation":"nsubj"},{"source":2,"destination":8,"relation":"punct"},{"source":5,"destination":4,"relation":"mark"}],"roots":[2]}}}]} """)
    val newEntities = entityFinder.extractAndFilter(docWithDoctorAsDate)
    newEntities should have size(1)
    newEntities.exists(e => e.text == "doctor") should be (false)

  }

  it should "trim entity edges as specified" in {
    val doc = jsonStringToDocument("""{"sentences":[{"words":["The","blue","fish","of","happiness","."],"startOffsets":[0,4,9,14,17,26],"endOffsets":[3,8,13,16,26,27],"raw":["The","blue","fish","of","happiness","."],"tags":["DT","JJ","NN","IN","NN","."],"lemmas":["the","blue","fish","of","happiness","."],"entities":["O","O","O","O","O","O"],"norms":["O","O","O","O","O","O"],"chunks":["B-NP","I-NP","I-NP","B-PP","B-NP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":2,"destination":4,"relation":"nmod_of"},{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"amod"},{"source":2,"destination":0,"relation":"det"},{"source":2,"destination":5,"relation":"punct"}],"roots":[2]},"universal-basic":{"edges":[{"source":4,"destination":3,"relation":"case"},{"source":2,"destination":1,"relation":"amod"},{"source":2,"destination":0,"relation":"det"},{"source":2,"destination":4,"relation":"nmod"},{"source":2,"destination":5,"relation":"punct"}],"roots":[2]}}}]}""")
    val entity = new TextBoundMention(Seq("Entity"), Interval(0, doc.sentences.head.words.size), 0, doc, true, "test")
    entity.text should be ("The blue fish of happiness.")
    val trimmed = entityFinder.trimEntityEdges(entity, entityFinder.tagSet)
    trimmed.text should be ("blue fish of happiness")
  }

  it should "require N/V only when asked to" in {
    // CustomizableRuleBasedFinder where the base entity must have a noun/verb (default)
    val requireTrue = CustomizableRuleBasedFinder.fromConfig(
      ConfigFactory.load("test").withValue(
        "CustomRuleBasedEntityFinder.entityRulesPath",
        ConfigValueFactory.fromAnyRef("testEntities.yml")
      ).withValue(
        "CustomRuleBasedEntityFinder.avoidRulesPath",
        ConfigValueFactory.fromAnyRef("testAvoid.yml")
      )
    )

    // CustomizableRuleBasedFinder where that requirement is disabled
    val requireFalse = CustomizableRuleBasedFinder.fromConfig(
      ConfigFactory.load("test").withValue(
        "CustomRuleBasedEntityFinder.entityRulesPath",
        ConfigValueFactory.fromAnyRef("testEntities.yml")
      ).withValue(
        "CustomRuleBasedEntityFinder.avoidRulesPath",
        ConfigValueFactory.fromAnyRef("testAvoid.yml")
      ).withValue(
        "CustomRuleBasedEntityFinder.ensureBaseTagNounVerb",
        ConfigValueFactory.fromAnyRef(false)
      )
    )

    // Farmers believe that loans are useful.
    val doc = jsonStringToDocument("""{"sentences":[{"words":["Farmers","believe","that","loans","are","useful","."],"startOffsets":[0,8,16,21,27,31,37],"endOffsets":[7,15,20,26,30,37,38],"raw":["Farmers","believe","that","loans","are","useful","."],"tags":["NNS","VBP","IN","NNS","VBP","JJ","."],"lemmas":["farmer","believe","that","loan","be","useful","."],"entities":["O","O","O","O","O","O","O"],"norms":["","","","","","",""],"chunks":["B-NP","B-VP","B-SBAR","B-NP","B-VP","B-ADJP","O"],"graphs":{"universal-enhanced":{"edges":[{"source":1,"destination":5,"relation":"ccomp"},{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":6,"relation":"punct"},{"source":5,"destination":2,"relation":"mark"},{"source":5,"destination":3,"relation":"nsubj"},{"source":5,"destination":4,"relation":"cop"}],"roots":[1]},"universal-basic":{"edges":[{"source":1,"destination":0,"relation":"nsubj"},{"source":5,"destination":2,"relation":"mark"},{"source":5,"destination":3,"relation":"nsubj"},{"source":5,"destination":4,"relation":"cop"},{"source":1,"destination":5,"relation":"ccomp"},{"source":1,"destination":6,"relation":"punct"}],"roots":[1]},"hybrid":{"edges":[{"source":1,"destination":5,"relation":"ccomp"},{"source":1,"destination":0,"relation":"nsubj"},{"source":1,"destination":6,"relation":"punct"},{"source":5,"destination":2,"relation":"mark"},{"source":5,"destination":3,"relation":"nsubj"},{"source":5,"destination":4,"relation":"cop"},{"source":1,"destination":0,"relation":"A0"},{"source":1,"destination":5,"relation":"A1"}],"roots":[1]},"semantic-roles":{"edges":[{"source":1,"destination":0,"relation":"A0"},{"source":1,"destination":2,"relation":"A1"}],"roots":[5,1,6,3,4]},"enhanced-semantic-roles":{"edges":[{"source":1,"destination":0,"relation":"A0"},{"source":1,"destination":5,"relation":"A1"}],"roots":[1,6,2,3,4]}}}]}""")

    requireTrue.extract(doc).map(_.text) shouldNot contain("useful")

    requireFalse.extract(doc).map(_.text) should contain("useful")

  }

}
