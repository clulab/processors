package org.clulab.odin.impl

import org.clulab.processors.Document

trait Values {
  def values(strings: Option[Array[String]], msg: String): Array[String] =
    strings match {
      case None => sys.error(msg)
      case Some(strings) => strings
    }

  def word(tok: Int, sent: Int, doc: Document): String =
    doc.sentences(sent).words(tok)

  def lemma(tok: Int, sent: Int, doc: Document): String = {
    val lemmas = values(doc.sentences(sent).lemmas, "sentence has no lemmas")
    lemmas(tok)
  }

  def tag(tok: Int, sent: Int, doc: Document): String = {
    val tags = values(doc.sentences(sent).tags, "sentence has no tags")
    tags(tok)
  }

  def entity(tok: Int, sent: Int, doc: Document): String = {
    val entities = values(doc.sentences(sent).entities, "sentence has no entities")
    entities(tok)
  }

  def chunk(tok: Int, sent: Int, doc: Document): String = {
    val chunks = values(doc.sentences(sent).chunks, "sentence has no chunks")
    chunks(tok)
  }

}
