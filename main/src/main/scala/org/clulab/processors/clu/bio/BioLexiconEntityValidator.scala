package org.clulab.processors.clu.bio

import org.clulab.processors.Sentence
import org.clulab.struct.EntityValidator

/**
  * Validates if the span identified as an entity is actually valid
  * User: mihais
  * Date: 10/24/16
  */
@SerialVersionUID(1000L)
class BioLexiconEntityValidator extends EntityValidator {

  override def validMatch(sentence: Sentence, start:Int, end:Int):Boolean = {
    assert(start < end)

    // must contain at least one NN*
    // see also removeSinglePrepositions, for deprecated code
    var nouns = 0
    for(i <- start until end)
      if(sentence.tags.get(i).startsWith("NN"))
        nouns += 1
    if(nouns == 0) {
      return false
    }

    // some entities end with -ing verbs (e.g., "binding")
    // do not accept them when followed by "to"
    // TODO: anything else?
    if(end < sentence.words.length) {
      val last = sentence.words(end - 1)
      val to = sentence.words(end)
      if(last.length > 3 && last.toLowerCase.endsWith("ing") && to.toLowerCase == "to") {
        return false
      }
    }

    true
  }
}
