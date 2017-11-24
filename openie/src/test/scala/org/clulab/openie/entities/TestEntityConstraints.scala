package org.clulab.openie.entities

import org.clulab.odin.TextBoundMention
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.serialization.json.JSONSerializer
import org.json4s.jackson.JsonMethods._
import org.clulab.openie.IETestUtils._
import org.scalatest.{ FlatSpec, Matchers }


class TestEntityConstraints extends FlatSpec with Matchers {

  "Uneven parentheses" should "be disallowed in entities" in {

    val doc1 = jsonStringToDocument(""" {"sentences": [{"endOffsets": [8,15,19,21,26,37,39,50,51],"entities": ["O","O","O","O","O","O","O","O","O"],"graphs": {"stanford-basic": {"edges": [{"destination": 0,"relation": "nsubj","source": 1},{"destination": 2,"relation": "neg","source": 1},{"destination": 1,"relation": "parataxis","source": 4},{"destination": 5,"relation": "dobj","source": 4},{"destination": 7,"relation": "prep","source": 4}],"roots": [4]},"stanford-collapsed": {"edges": [{"destination": 0,"relation": "nsubj","source": 1},{"destination": 2,"relation": "neg","source": 1},{"destination": 1,"relation": "parataxis","source": 4},{"destination": 5,"relation": "dobj","source": 4},{"destination": 7,"relation": "prep","source": 4}],"roots": [4]}},"lemmas": ["entity","should","not",")","have","unbalanced","(","parenthesis","."],"startOffsets": [0,9,16,20,22,27,38,39,50],"tags": ["NNS","MD","RB","-RRB-","VB","JJ","-LRB-","NNS","."],"words": ["Entities","should","not",")","have","unbalanced","(","parentheses","."]}],"text": "Entities should not ) have unbalanced (parentheses."} """)

    val entity1 = new TextBoundMention(
      label = "Entity",
      tokenInterval = Interval(start = 0, end = doc1.sentences.head.words.length),
      sentence = 0,
      document = doc1,
      keep = true,
      foundBy = "test-entities"
    )

    EntityConstraints.matchingBrackets(entity1) should be (false)

    val doc2 = jsonStringToDocument(""" {"sentences": [{"endOffsets": [8,15,19,21,26,37,39,50,51],"entities": ["O","O","O","O","O","O","O","O","O"],"graphs": {"stanford-basic": {"edges": [{"destination": 0,"relation": "nsubj","source": 3},{"destination": 1,"relation": "aux","source": 3},{"destination": 2,"relation": "neg","source": 3},{"destination": 4,"relation": "xcomp","source": 3},{"destination": 7,"relation": "dobj","source": 4},{"destination": 5,"relation": "amod","source": 7},{"destination": 6,"relation": "nn","source": 7}],"roots": [3]},"stanford-collapsed": {"edges": [{"destination": 0,"relation": "nsubj","source": 3},{"destination": 1,"relation": "aux","source": 3},{"destination": 2,"relation": "neg","source": 3},{"destination": 4,"relation": "xcomp","source": 3},{"destination": 7,"relation": "dobj","source": 4},{"destination": 5,"relation": "amod","source": 7},{"destination": 6,"relation": "nn","source": 7}],"roots": [3]}},"lemmas": ["entity","should","not","]","have","unbalanced","[","parenthesis","."],"startOffsets": [0,9,16,20,22,27,38,39,50],"tags": ["NNS","MD","RB","VB","VB","JJ","NN","NNS","."],"words": ["Entities","should","not","]","have","unbalanced","[","parentheses","."]}],"text": "Entities should not ] have unbalanced [parentheses."} """)

    val entity2 = new TextBoundMention(
      label = "Entity",
      tokenInterval = Interval(start = 0, end = doc2.sentences.head.words.length),
      sentence = 0,
      document = doc2,
      keep = true,
      foundBy = "test-entities"
    )

    EntityConstraints.matchingBrackets(entity2) should be (false)

  }

  "Even parentheses" should "be valid in entities" in {

    val doc1 = jsonStringToDocument(""" {"sentences": [{"endOffsets": [3,5,11,13,15,16,25,26,27],"entities": ["O","O","O","O","O","O","O","O","O"],"graphs": {"stanford-basic": {"edges": [{"destination": 0,"relation": "det","source": 2},{"destination": 4,"relation": "dep","source": 2},{"destination": 2,"relation": "nsubj","source": 6}],"roots": [6]},"stanford-collapsed": {"edges": [{"destination": 0,"relation": "det","source": 2},{"destination": 4,"relation": "dep","source": 2},{"destination": 2,"relation": "nsubj","source": 6}],"roots": [6]}},"lemmas": ["the","(","entity","(","be",")","balanced",")","."],"startOffsets": [0,4,5,12,13,15,17,25,26],"tags": ["DT","-LRB-","NN","-LRB-","VBZ","-RRB-","JJ","-RRB-","."],"words": ["The","(","entity","(","is",")","balanced",")","."]}],"text": "The (entity (is) balanced)."} """)

    val entity1 = new TextBoundMention(
      label = "Entity",
      tokenInterval = Interval(start = 0, end = doc1.sentences.head.words.length),
      sentence = 0,
      document = doc1,
      keep = true,
      foundBy = "test-entities"
    )

    EntityConstraints.matchingBrackets(entity1) should be (true)

    val doc2 = jsonStringToDocument(""" {"sentences": [{"endOffsets": [3,5,11,13,15,16,25,26,27],"entities": ["O","O","O","O","O","O","O","O","O"],"graphs": {"stanford-basic": {"edges": [{"destination": 0,"relation": "det","source": 3},{"destination": 1,"relation": "num","source": 3},{"destination": 2,"relation": "nn","source": 3},{"destination": 3,"relation": "nsubj","source": 7},{"destination": 4,"relation": "cop","source": 7},{"destination": 5,"relation": "num","source": 7},{"destination": 6,"relation": "amod","source": 7}],"roots": [7]},"stanford-collapsed": {"edges": [{"destination": 0,"relation": "det","source": 3},{"destination": 1,"relation": "num","source": 3},{"destination": 2,"relation": "nn","source": 3},{"destination": 3,"relation": "nsubj","source": 7},{"destination": 4,"relation": "cop","source": 7},{"destination": 5,"relation": "num","source": 7},{"destination": 6,"relation": "amod","source": 7}],"roots": [7]}},"lemmas": ["the","[","entity","[","be","]","balanced","]","."],"startOffsets": [0,4,5,12,13,15,17,25,26],"tags": ["DT","CD","NN","NN","VBZ","CD","JJ","NN","."],"words": ["The","[","entity","[","is","]","balanced","]","."]}],"text": "The [entity [is] balanced]."} """)

    val entity2 = new TextBoundMention(
      label = "Entity",
      tokenInterval = Interval(start = 0, end = doc2.sentences.head.words.length),
      sentence = 0,
      document = doc2,
      keep = true,
      foundBy = "test-entities"
    )

    EntityConstraints.matchingBrackets(entity2) should be (true)

  }

  "Entities longer than n tokens" should "be pruned" in {

    val doc = jsonStringToDocument(""" {"sentences": [{"endOffsets": [4,12,15,19,23,27,31,35,39,43,47,51,55,60,61],"entities": ["O","O","O","O","O","O","O","O","O","O","O","O","O","O","O"],"graphs": {"stanford-basic": {"edges": [{"destination": 0,"relation": "det","source": 1},{"destination": 1,"relation": "nsubj","source": 2},{"destination": 5,"relation": "advmod","source": 2},{"destination": 3,"relation": "advmod","source": 5},{"destination": 4,"relation": "advmod","source": 5},{"destination": 6,"relation": "advmod","source": 5},{"destination": 7,"relation": "advmod","source": 5},{"destination": 8,"relation": "advmod","source": 5},{"destination": 9,"relation": "advmod","source": 5},{"destination": 10,"relation": "advmod","source": 5},{"destination": 11,"relation": "advmod","source": 5},{"destination": 12,"relation": "advmod","source": 5},{"destination": 13,"relation": "advmod","source": 5}],"roots": [2]},"stanford-collapsed": {"edges": [{"destination": 0,"relation": "det","source": 1},{"destination": 1,"relation": "nsubj","source": 2},{"destination": 5,"relation": "advmod","source": 2},{"destination": 3,"relation": "advmod","source": 5},{"destination": 4,"relation": "advmod","source": 5},{"destination": 6,"relation": "advmod","source": 5},{"destination": 7,"relation": "advmod","source": 5},{"destination": 8,"relation": "advmod","source": 5},{"destination": 9,"relation": "advmod","source": 5},{"destination": 10,"relation": "advmod","source": 5},{"destination": 11,"relation": "advmod","source": 5},{"destination": 12,"relation": "advmod","source": 5},{"destination": 13,"relation": "advmod","source": 5}],"roots": [2]}},"lemmas": ["this","enitity","be","far","far","far","far","far","far","far","far","far","too","long","."],"startOffsets": [0,5,13,16,20,24,28,32,36,40,44,48,52,56,60],"tags": ["DT","NN","VBZ","RB","RB","RB","RB","RB","RB","RB","RB","RB","RB","RB","."],"words": ["This","enitity","is","far","far","far","far","far","far","far","far","far","too","long","."]}],"text": "This enitity is far far far far far far far far far too long."} """)

    val entity = new TextBoundMention(
      label = "Entity",
      tokenInterval = Interval(start = 0, end = doc.sentences.head.words.length),
      sentence = 0,
      document = doc,
      keep = true,
      foundBy = "test-entities"
    )

    EntityConstraints.withinMaxLength(mention = entity, n = 10) should be (false)

  }

  // TODO: test avoidance of "et al."-style references
  // Barnaby et al. caused unrest within the community
  // TODO: test validFinalToken
}
