package org.clulab.processors

import org.clulab.discourse.rstparser.DiscourseTree
import org.clulab.struct.CorefChains
import scala.util.hashing.MurmurHash3._


/**
  * Stores all annotations for one document.
  *   Written by: Mihai Surdeanu and Gus Hahn-Powell.
  *   Last Modified: Add apply method to copy Document.
  */
class Document(val sentences: Array[Sentence]) extends Serializable {

  var id: Option[String] = None
  // FIXME: are coreferenceChains needed? Seems like a CoreNLP-specific thing...
  var coreferenceChains: Option[CorefChains] = None
  var discourseTree: Option[DiscourseTree] = None
  var text: Option[String] = None

  /** Clears any internal state potentially constructed by the annotators */
  def clear() { }

  /**
    * Used to compare Documents.
    * @return a hash (Int) based primarily on the sentences
    */
  def equivalenceHash: Int = {

    val stringCode = "org.clulab.processors.Document"

    // Hash representing the sentences.
    // Used by equivalenceHash.
    // return an Int hash based on the Sentence.equivalenceHash of each sentence
    def sentencesHash: Int = {
      val h0 = stringHash(s"$stringCode.sentences")
      val hs = sentences.map(_.equivalenceHash)
      val h = mixLast(h0, unorderedHash(hs))
      finalizeHash(h, sentences.length)
    }

    // the seed (not counted in the length of finalizeHash)
    // decided to use the class name
    val h0 = stringHash(stringCode)
    // comprised of the equiv. hash of sentences
    val h1 = mix(h0, sentencesHash)
    finalizeHash(h1, 1)
  }
}

object Document {

  def apply(sentences: Array[Sentence]): Document = new Document(sentences)

  def apply(id: Option[String], sentences: Array[Sentence], coref: Option[CorefChains], dtree: Option[DiscourseTree], text: Option[String]): Document = {
    val d = Document(sentences)
    d.id = id
    d.coreferenceChains = coref
    d.discourseTree = dtree
    d.text = text
    d
  }

  /** Return a new Document with relevant fields copied from the given Document. */
  def apply (doc: Document): Document =
    Document(doc.id, doc.sentences, doc.coreferenceChains, doc.discourseTree, doc.text)

}
