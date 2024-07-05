package org.clulab.openie.utils

abstract class TagSet {
  def isAnyNoun(tag: String): Boolean
  def isAnyVerb(tag: String): Boolean
  def isAnyAdjective(tag: String): Boolean

  def isCoordinating(tag: String): Boolean
  def isValidFinal(tag: String): Boolean
  def isInvalidEdge(tag: String): Boolean
}

/*

1. -LCB- Left  curly brace
2. -RCB- Right curly brace
3. -LRB- Left  round brace
4. -RRB- Right round brace
5. -LSB- Left  square brace
6. -RSB- Right square brace

*/

object TagSet {
  val BRACKETS: Array[(String, String)] = Array(
    ("(", ")"), ("-LRB-", "-RRB-"), // round
    ("{", "}"), ("-LCB-", "-RCB-"), // curly
    ("[", "]"), ("-LSB-", "-RSB-")  // square
  )

  def apply(t: String): TagSet = {
    t match {
      case "english" => new EnglishTagSet
      case "portuguese" => new PortugueseTagSet
      case "spanish" => new SpanishTagSet
      case _ => throw new RuntimeException(s"Invalid tagset selector: ${t}")
    }
  }
}

/*

https://web.stanford.edu/class/cs124/lec/postagging.pdf
https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html

 1. CC   Coordinating conjunction
 2. CD   Cardinal number
 3. DT   Determiner
 4. EX   Existential there
 5. FW   Foreign word
 6. IN   Preposition or subordinating conjunction
 7. JJ   Adjective
 8. JJR  Adjective, comparative
 9. JJS  Adjective, superlative
10. LS   List item marker
11. MD   Modal
12. NN   Noun, singular or mass
13. NNS  Noun, plural
14. NNP  Proper noun, singular
15. NNPS Proper noun, plural
16. PDT  PredeterminerV
17. POS  Possessive ending
18. PRP  Personal pronoun
19. PRP$ Possessive pronoun
20. RB   Adverb
21. RBR  Adverb, comparative
22. RBS  Adverb, superlative
23. RP   Particle
24. SYM  Symbol
25. TO   to
26. UH   Interjection
27. VB   Verb, base form
28. VBD  Verb, past tense
29. VBG  Verb, gerund or present participle
30. VBN  Verb, past participle
31. VBP  Verb, non-3rd person singular present
32. VBZ  Verb, 3rd person singular present
33. WDT  Wh-determiner
34. WP   Wh-pronoun
35. WP$  Possessive wh-pronoun
36. WRB  Wh-adverb

*/

class EnglishTagSet extends TagSet {

  def isAnyNoun(tag: String): Boolean = tag.startsWith("N") // It could be NN, but N is faster.
  def isAnyVerb(tag: String): Boolean = tag.startsWith("V") // Could be VB, but V is faster.
  def isAnyAdjective(tag: String): Boolean = tag.startsWith("J")  // It could be JJ, but J is faster.

  val coordinating: Array[String] = Array("CC", ",", "-LRB-", "-RRB-")
  def isCoordinating(tag: String): Boolean = coordinating.contains(tag) // This will use ==, not startsWith.

  // Set of tags that can end an entity
  val validFinal: Array[String] = Array("N", "V", "J", /*"RB",*/ "DT", "-RC", "-RS", "-RR")
  def isValidFinal(tag: String): Boolean = validFinal.exists(tag.startsWith)

  // Set of tags that we don't want to begin or end an entity
  val invalidEdge: Array[String] = Array("PRP", "IN", "TO", "DT", ",", ".")
  def isInvalidEdge(tag: String): Boolean = invalidEdge.exists(tag.startsWith)
}

/*

VERBF - finite
VERBI - infinite
VERBG - gerund
VERBP - participle, passive

This is the conversion table from the tagset we used for Portuguese:
https://universaldependencies.org/tagset-conversion/pt-conll-uposf.html

ADJ   Adjective
ADP   Preposition?
ADV   Adverb
CCONJ Coordinating conjunction
DET   Determiner
INTJ  Interjection
NOUN  Noun
NUM   Number
PART  Participle?
PRON  Pronoun
PROPN Proper Noun
PUNCT Punctuation
SCONJ Subordinating conjunction
VERBF Verb, finite
VERBG Verb, gerund
VERBI Verb, infinite
VERBP Verb, participle, passive

*/

class PortugueseTagSet extends TagSet {
  def isAnyNoun(tag: String): Boolean = tag == "NOUN" || tag == "PROPN"
  def isAnyVerb(tag: String): Boolean = tag.startsWith("V")
  def isAnyAdjective(tag: String): Boolean = tag == "ADJ"

  val coordinating: Array[String] = Array("CCONJ", ",", "-LRB-", "-RRB-")
  def isCoordinating(tag: String): Boolean = coordinating.contains(tag) // This will use ==, not startsWith.
  val validFinal: Array[String] = Array("N", "PROPN", "V", "ADJ", "ADV", /*"DET",*/ "-RS", "-RR")
  def isValidFinal(tag: String): Boolean = validFinal.exists(tag.startsWith)
  val invalidEdge: Array[String] = Array("PRON", /*"ADP",*/ /*"IN",*/ "DET", ",")
  def isInvalidEdge(tag: String): Boolean = invalidEdge.exists(tag.startsWith)
}

class SpanishTagSet extends TagSet {
  val portugueseTagSet = new PortugueseTagSet()

  def isAnyNoun(tag: String): Boolean = portugueseTagSet.isAnyNoun(tag)
  def isAnyVerb(tag: String): Boolean = portugueseTagSet.isAnyVerb(tag)
  def isAnyAdjective(tag: String): Boolean = portugueseTagSet.isAnyAdjective(tag)

  def isCoordinating(tag: String): Boolean = portugueseTagSet.isCoordinating(tag)
  def isValidFinal(tag: String): Boolean = portugueseTagSet.isValidFinal(tag)
  def isInvalidEdge(tag: String): Boolean = portugueseTagSet.isInvalidEdge(tag)
}