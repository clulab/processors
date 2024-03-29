package org.clulab.embeddings

trait WordSanitizing extends Serializable {
  def sanitizeWord(word: String): String

  override def equals(other: Any): Boolean =
    this.getClass == other.getClass
}

class DefaultWordSanitizer extends WordSanitizing {

  override def sanitizeWord(word: String): String = {
    EmbeddingUtils.sanitizeWord(word, keepNumbers = true)
  }
}

object EmbeddingUtils {

  /**
    * Normalizes words for word2vec
    * @param uw A word (NOT lemma)
    * @return The normalized form of the word
    */
  def sanitizeWord(uw:String, keepNumbers:Boolean = true):String = {
    val w = uw.toLowerCase()

    // skip parens from corenlp
    if (w == "-lrb-" || w == "-rrb-" || w == "-lsb-" || w == "-rsb-") ""
    // skip URLS
    // added .com and .org to cover more urls (becky)
    else if (w.startsWith("http") || w.contains(".com") || w.contains(".org")) ""
    // normalize numbers to a unique token
    else if (isNumber(w)) {
      if (keepNumbers) "xnumx"
      else ""
    }
    else {
      // remove all non-letters; convert letters to lowercase
      val os = new collection.mutable.StringBuilder()
      var i = 0
      while(i < w.length) {
        val c = w.charAt(i)
        // added underscore since it is our delimiter for dependency stuff...
        if(Character.isLetter(c) || c == '_') os += c
        i += 1
      }
      os.toString()
    }
  }

  def isNumber(w: String):Boolean = {
    val isMaybeNumber = w.forall { c =>
      Character.isDigit(c) || "-+,./\\".contains(c)
    }

    isMaybeNumber && w.exists(Character.isDigit)
  }
}
