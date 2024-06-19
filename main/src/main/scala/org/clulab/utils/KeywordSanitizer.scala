package org.clulab.utils

import org.clulab.processors.Sentence
import scala.collection.mutable.ArrayBuffer
import KeywordSanitizer._

/**
  * Cleans up Sentence words to make them suitable for keyword extraction
  * In particular: 
  *   (a) It merges tokens belonging to the same NE into one string
  *   (b) It removes stop words and punctuation signs
  *   (c) It lower cases everything
  *
  */
class KeywordSanitizer {
  /** 
   * Produces a sequence of sanitized keywords from a sentence
   * The Sentence must have words, lemmas, and entities populated 
   */
  def sanitize(sent: Sentence): Seq[String] = {
    assert(sent.lemmas.nonEmpty)
    assert(sent.entities.nonEmpty)

    var i = 0
    var tokens = new ArrayBuffer[String]()
    val entities = sent.entities.get
    val lemmas = sent.lemmas.get
    while (i < sent.size) {
      // we found the beginning of a NE
      if(entities(i) != O) {
        // the first token after this NE
        val end = findEntityEnd(entities, i)
        // we're building the name from the words to preserve the original name
        val name = sent.words.slice(i, end).mkString(" ")
        tokens += name.toLowerCase() // we are converting NEs to lower case for consistency
        i = end
      } else {
        if(! STOP_LEMMAS.contains(lemmas(i)) && 
           ! PUNCTUATION_SIGNS.contains(sent.words(i))) {
          tokens += lemmas(i) // lemmas should be lower case already   
        }
        i += 1
      }
    }

    tokens
  }

  def mkLabel(ent: String, useBio: Boolean): String = {
    if(ent == O) ent
    else if(useBio) ent.substring(2)
    else ent
  }

  def findEntityEnd(entities:Array[String], start:Int): Int = {
    // are we using the BIO format for entity labels?
    val useBio = entities(start).startsWith("B-") || entities(start).startsWith("I-")
    val startLabel = mkLabel(entities(start), useBio) 
    var i = start
    var inside = true
    while(i < entities.length && inside) {
      val crtLabel = mkLabel(entities(i), useBio)
      if(crtLabel == startLabel) i += 1
      else inside = false
    }
    i
  }
}

object KeywordSanitizer {
  // empty NE tag
  val O = "O"

  // stop words using lemmas. started from the list of stop words in Lucene
  val STOP_LEMMAS = Set(
    "a", "an", "and", "as", "at", "be", "but", "by",
    "for", "if", "in", "into", "it",
    "no", "not", "of", "on", "or", "such",
    "that", "the", "their", "then", "there", "these",
    "they", "this", "to", "will", "with"
  )

  val PUNCTUATION_SIGNS = Set(
    ",", ";", ".", "...", "?", "!"
  )

  
}
